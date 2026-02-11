# Load necessary libraries for data manipulation and analysis.
# tidyverse: A collection of R packages for data science (e.g., dplyr, ggplot2).
# ipumsr: Helps with reading and managing data from IPUMS.
# duckplyr: A faster backend for dplyr, useful for large datasets.
library(tidyverse)
library(ipumsr)
library(duckplyr)
library(srvyr)

# -----------------------------------------------------------------------------
# This entire section is commented out. It contains the code used to define,
# submit, and download a custom data extract from the IPUMS USA database.
# This is a one-time setup process. The actual analysis below uses the
# data file downloaded from this process.
# -----------------------------------------------------------------------------

# # Get IPUMS variable lists from existing .xml files (data dictionaries).
# ddi63 <- read_ipums_ddi("./data/ipums_raw/usa_00063.xml")
# ddi64 <- read_ipums_ddi("./data/ipums_raw/usa_00064.xml")
# ddi93 <- read_ipums_ddi("./data/ipums_raw/usa_00093.xml")

# # Example of how to view value labels for a variable.
# # ipums_val_labels(ddi63, "YEAR")

# # Combine variable names from different DDI files to create a master list.
# ipums_var_list <- ipums_var_info(ddi63) |>
#   bind_rows(
#     ipums_var_info(ddi64)
#   ) |>
#   filter(var_name != "STATEFIP") |> # Avoid duplicating STATEFIP.
#   pull(var_name) |> # Extract the variable name column.
#   unique() |> # Keep only unique variable names.
#   # Manually add specific variables needed for the analysis.
#   append(list(
#     "MET2013", # Metropolitan area for 2013 definitions.
#     "MET2013ERR", # Error code for MET2013.
#     # Add replicate weights for variance estimation (if needed for future analysis).
#     "REPWT", # Household Replicate Weights (for households)
#     "REPWTP", # Person Replicate Weights (for persons)
#     # Specify STATEFIP but select only cases for Colorado (FIPS code 08).
#     var_spec(
#       name = "STATEFIP",
#       case_selections = "08"
#     )
#   ))

# # Get a list of all 1-year ACS sample names from IPUMS.
# ipums_samples <- get_sample_info("usa") |>
#   # Filter to get samples that match the ACS 1-year naming convention (e.g., "us2021a").
#   filter(str_detect(name, "us2\\d{3}a")) |>
#   pull(name) # Extract the sample names.

# # Define the data extract request for IPUMS.
# ipums_extract <- define_extract_micro(
#   collection = "usa",
#   description = "acs samples for Colorado housing shortage",
#   samples = ipums_samples,
#   variables = ipums_var_list,
#   case_select_who = "households", # Select data at the household level.
#   data_structure = "hierarchical" # Required to get data on vacant households.
# )

# # Submit the extract request to the IPUMS server.
# ipums_extract_submitted <- ipums_extract |>
#   submit_extract()

# # Wait for the extract to be prepared and then download it to the specified directory.
# ipums_extract_submitted |>
#   wait_for_extract() |>
#   download_extract(
#     download_dir = "./data/ipums_raw"
#   )

# -----------------------------------------------------------------------------
# Data Loading and Initial Preparation
# -----------------------------------------------------------------------------

# Read the IPUMS data extract (which contains both household and person-level data).
# The result is a list with separate data frames for each record type.

ipums_file_num <- "00112" # Update this number based on the actual file downloaded from IPUMS.

# check to see if the parquet files already exist before reading the raw data
if (
  !file.exists(paste0("./data/household_", ipums_file_num, ".parquet")) ||
    !file.exists(paste0("./data/person_", ipums_file_num, ".parquet"))
) {
  # If the parquet files don't exist, read the raw IPUMS data and save it as parquet for faster future loading.
  ipums_data <- read_ipums_micro_list("./data/ipums_raw/usa_00112.xml")

  ipums_data$HOUSEHOLD |>
    zap_ipums_attributes() |>
    compute_parquet(path = "./data/household_00112.parquet")

  ipums_data$PERSON |>
    zap_ipums_attributes() |>
    compute_parquet(path = "./data/person_00112.parquet")

  rm(ipums_data) # Remove the original data from memory to free up space.
}

ipums_data <- list(
  HOUSEHOLD = read_parquet_duckdb(paste0(
    "./data/household_",
    ipums_file_num,
    ".parquet"
  )),
  PERSON = read_parquet_duckdb(paste0(
    "./data/person_",
    ipums_file_num,
    ".parquet"
  ))
)

ipums_ddi <- read_ipums_ddi("./data/ipums_raw/usa_00112.xml") # Read the DDI file for metadata and variable information.

# Prepare the HOUSEHOLD level dataframe.
housing_df <- ipums_data$HOUSEHOLD |>
  # Join with metropolitan area labels to get readable MSA names.
  left_join(
    ipums_val_labels(ipums_ddi, "MET2013") |>
      rename(MET2013 = val) |> # Rename columns for joining.
      rename(msa = lbl)
  ) |>
  # Remove the RECTYPE column, as it's no longer needed.
  select(-RECTYPE) |>
  # Filter out group quarters populations that are not standard housing units (e.g., military barracks, correctional facilities).
  filter(!(GQ %in% c(3, 4))) |>
  # Create new boolean variables based on IPUMS codes to identify specific housing types.
  mutate(SEASONAL = VACANCY == 4) |> # Define seasonal/recreational housing.
  mutate(
    # Define uninhabitable housing based on lack of kitchen/plumbing and long-term vacancy.
    UNIHABITABLE = (KITCHEN == 1 | PLUMBING == 10) &
      if_else(is.na(VACDUR), VACANCY %in% c(1:3), VACDUR >= 7),
    # Define vacant units that are available for rent or sale.
    VACAVAIL = VACANCY %in% c(1:3)
  ) |>
  # Define all vacant housing units.
  mutate(VACANT = VACANCY != 0)

# Create housing survey design object based on ipums data
housing_svy <- housing_df |>
  as_survey_design(
    weight = HHWT, # Household weight variable for survey design.
    repweights = matches("REPWT[0-9]+"), # Replicate weights for variance estimation.
    type = "ACS",
    mse = TRUE # Indicates that the replicate weights are for mean squared error estimation (ACS uses a specific method for variance estimation).
  )
# -----------------------------------------------------------------------------
# Calculate Aggregate Housing Statistics for Colorado
# -----------------------------------------------------------------------------

# create function for counting housing units with survey weights
survey_count_parquet <- function(svy, name) {
  ipums_file <- paste0("./data/", name, "_", ipums_file_num, ".parquet")
  if (!file.exists(ipums_file)) {
    survey <- svy |>
      survey_count(YEAR, msa, name = name) |>
      compute_parquet(path = ipums_file)
  } else {
    survey <- read_parquet_duckdb(ipums_file)
  }
  return(survey)
}

# Calculate total housing units per year and MSA, using household weights (HHWT).
CO_HOUSING_UNITS <- housing_svy |>
  survey_count_parquet("housing_units")

# Calculate total seasonal housing units.
CO_SEASONAL_HOUSING <- housing_svy |>
  filter(SEASONAL) |>
  survey_count_parquet("housing_units_seasonal")

# Calculate total uninhabitable housing units.
CO_UNINHABITABLE_HOUSING <- housing_svy |>
  filter(UNIHABITABLE) |>
  survey_count_parquet("housing_units_uninhabitable")

# Calculate total vacant-and-available housing units.
CO_VACANT_HOUSING <- housing_svy |>
  filter(VACAVAIL) |>
  survey_count_parquet("housing_units_vacant")

# Calculate the total number of occupied households in Colorado (STATEFIP 8, VACANCY 0).
CO_HOUSEHOLDS <- housing_svy |>
  filter(STATEFIP == 8 & VACANCY == 0) |>
  survey_count_parquet("households")

# -----------------------------------------------------------------------------
# Prepare Inflation Adjustment Data
# -----------------------------------------------------------------------------

# Read Consumer Price Index (CPI) data from a local CSV file.
cpi_df <- read_csv_duckdb("data/CPIAUCSL.csv") |>
  # Extract the year from the date.
  mutate(YEAR = year(observation_date)) |>
  # Calculate the average CPI for each year.
  group_by(YEAR) |>
  summarize(cpi = mean(CPIAUCSL), .groups = "drop") |>
  # Create an adjustment factor to convert all dollar amounts to 2023 dollars.
  mutate(adj_factor = first(cpi[YEAR == 2023]) / cpi)

# -----------------------------------------------------------------------------
# Prepare Combined Household and Person Data for Analysis
# -----------------------------------------------------------------------------

# Prepare the PERSON level dataframe.
person_df <- ipums_data$PERSON |>
  select(-RECTYPE) |>
  # Create a binary variable for head of household (RELATE code 1).
  mutate(HEADSHIP = RELATE == 1)

# Set key parameters for the analysis.
analysis_year <- 2024
baseline_year <- 2000 # The year whose headship rates will be used as a benchmark.
minimum_age <- 18
maximum_age <- 45
target_vacancy_rate <- .05 # A 5% target vacancy rate for a healthy housing market.

# check to see if the hh file exists
if (!file.exists(paste0("./data/hh_", ipums_file_num, ".parquet"))) {
  # Create the main analysis dataframe by joining household and person data.
  hh_df <- collect(housing_df) |>
    # Join housing data to person data. A right_join keeps all persons.
    right_join(
      collect(person_df),
      by = c("YEAR", "SERIAL", "CBSERIAL", "SAMPLE") # Unique identifiers for a household.
    ) |>
    # Filter to the non-institutionalized population.
    filter(GQ %in% c(0, 1, 2, 5)) |>
    # Recode IPUMS missing value codes (e.g., 9999999) to 0 or NA for monetary variables.
    mutate(HHINCOME = if_else(HHINCOME == 9999999, 0, HHINCOME)) |>
    mutate(OWNCOST = if_else(OWNCOST == 99999, 0, OWNCOST)) |>
    mutate(INCTOT = if_else(INCTOT == 9999999, NA_integer_, INCTOT)) |>
    # Create a single HOUSINGCOST variable from either owner costs or renter costs.
    mutate(
      HOUSINGCOST = case_when(
        OWNERSHP == 1 ~ OWNCOST,
        OWNERSHP == 2 ~ RENTGRS,
        TRUE ~ NA_integer_ # Set to NA if neither owner nor renter.
      )
    ) |>
    # Assume total personal income (INCTOT) is 0 for children 15 and under, where it is often NA.
    mutate(INCTOT = if_else(is.na(INCTOT) & AGE <= 15, 0, INCTOT)) |>
    # Join CPI data to perform inflation adjustment.
    left_join(cpi_df, by = "YEAR") |>
    # Adjust all monetary values to constant 2023 dollars.
    mutate(HOUSINGCOST = HOUSINGCOST * adj_factor) |>
    mutate(HHINCOME = HHINCOME * adj_factor) |>
    mutate(INCTOT = INCTOT * adj_factor) |>
    mutate(FTOTINC = FTOTINC * adj_factor) |>
    # Re-create the HEADSHIP variable to ensure it's in the final dataframe.
    mutate(HEADSHIP = RELATE == 1) |>
    # Create 5-year age group bins.
    mutate(
      AGEGROUP = as.character(cut(
        AGE,
        c(0, seq(15, 65, 5), Inf),
        include.lowest = TRUE,
        right = FALSE
      ))
    ) |>
    # To avoid division by zero or negative values, set any income/cost less than 1 to 1.
    mutate(INCTOT = if_else(INCTOT < 1, 1, INCTOT)) |>
    mutate(HOUSINGCOST = if_else(HOUSINGCOST < 1, 1, HOUSINGCOST)) |>
    # Calculate the housing cost to income ratio (annualized).
    mutate(HCOSTRATIO = HOUSINGCOST / (INCTOT) * 12) |>
    mutate(
      age_dummy = (AGE >= minimum_age & AGE < maximum_age)
    ) |>
    # Move MSA column for better readability.
    relocate(msa, .after = MET2013) |>
    # compute to parquet
    compute_parquet(path = paste0("./data/hh_", ipums_file_num, ".parquet"))
} else {
  hh_df <- read_parquet_duckdb(
    paste0("./data/hh_", ipums_file_num, ".parquet"),
    prudence = "lavish"
  )
}

# Create hh srvy object for survey analysis on the combined household-person data.
hh_svy <- hh_df |>
  as_survey_design(
    weight = PERWT, # Person weight variable for survey design.
    repweights = matches("REPWTP[0-9]+"), # Replicate weights for variance estimation.
    type = "ACS",
    mse = TRUE # Indicates that the replicate weights are for mean squared error estimation (ACS uses a specific method for variance estimation).
  )

if (
  !file.exists(paste0("./data/co_headship_rates_", ipums_file_num, ".parquet"))
) {
  # Create a table showing the headship rate (propensity to be head of household) by age group over the years.
  co_hr_table <- hh_svy |>
    filter(STATEFIP == 8, AGE >= 15) |> # Filter for Colorado residents age 15+.
    group_by(AGEGROUP, YEAR) |>
    # Calculate the weighted mean headship rate using person weights (PERWT).
    summarise(
      HEADSHIP = survey_mean(HEADSHIP, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Pivot Headship and _se to have years as columns for easier comparison.
    pivot_longer(
      cols = starts_with("HEADSHIP"),
      names_to = "TYPE",
      values_to = "HEADSHIP"
    ) |>
    # Reshape data to have years as columns for easy comparison.
    pivot_wider(names_from = YEAR, values_from = HEADSHIP) |>
    compute_parquet(
      path = paste0("./data/co_headship_rates_", ipums_file_num, ".parquet")
    )
} else {
  co_hr_table <- read_parquet_duckdb(paste0(
    "./data/co_headship_rates_",
    ipums_file_num,
    ".parquet"
  ))
}
# -----------------------------------------------------------------------------
# Housing Shortage Calculation
# -----------------------------------------------------------------------------

# This is the core logic to estimate "missing households".
# It compares current headship rates to a baseline year's rates.
if (
  !file.exists(paste0(
    "./data/baseline_headship_rates_",
    ipums_file_num,
    ".parquet"
  ))
) {
  baseline_df <- hh_svy |>
    # Step 1: Calculate the baseline headship rate for each MSA and age group in the baseline_year.
    filter(
      STATEFIP == 8,
      YEAR == baseline_year
    ) |>
    group_by(msa, AGEGROUP, MET2013, age_dummy) |>
    summarise(
      NEWHR = survey_mean(HEADSHIP), # This is the baseline headship rate.
      .groups = "drop" # Ungroup after summarizing to avoid unintended grouping in later steps.
    ) |>
    compute_parquet(
      path = paste0(
        "./data/baseline_headship_rates_",
        ipums_file_num,
        ".parquet"
      )
    )
} else {
  baseline_df <- read_parquet_duckdb(paste0(
    "./data/baseline_headship_rates_",
    ipums_file_num,
    ".parquet"
  ))
}

# create function to calculate missing households with survey weights based on YEAR

calculate_missing_households <- function(year) {
  missing_hh_file <- paste0(
    "./data/",
    ipums_file_num,
    "/missing_hh_",
    year,
    "_",
    ".parquet"
  )
  if (!file.exists(missing_hh_file)) {
    simple_target_hh_df <- hh_df |>
      filter(
        STATEFIP == 8,
        YEAR == year
      ) |>
      # Step 2: Join the baseline headship rates back to the full Colorado person-level data.
      left_join(
        baseline_df,
        by = join_by(msa, AGEGROUP, MET2013, age_dummy),
        relationship = "one-to-one"
      ) |>
      as_survey_design(
        weight = PERWT, # Person weight variable for survey design.
        repweights = matches("REPWTP[0-9]+"), # Replicate weights
        type = "ACS",
        mse = TRUE # Indicates that the replicate weights are for mean squared error estimation (ACS uses a specific method for variance estimation).
      ) |>
      group_by(msa, AGEGROUP, MET2013, NEWHR, PUMA, age_dummy, YEAR) |>
      # Step 3: Calculate actual vs. hypothetical households for each year.
      summarise(
        # Hypothetical households if baseline headship rates applied to today's population.
        NEWHH_ = survey_total(NEWHR),
        # Actual number of households today.
        HH = survey_total(HEADSHIP),
        .groups = "drop" # Ungroup after summarizing to avoid unintended grouping in later steps.
      ) |>
      # Step 4: Determine the target number of households.
      mutate(
        # For the target age group, if hypothetical households > actual, use the hypothetical number.
        # This prevents counting a "surplus" if headship rates have risen.
        NEWHH = if_else(
          age_dummy & NEWHH_ > HH,
          NEWHH_,
          HH
        ),
        NEWHH_se = if_else(
          age_dummy & NEWHH_ > HH,
          NEWHH__se,
          HH_se
        ),
        # Step 5: Calculate "missing households" as the difference between target and actual.
        MISSINGHH = NEWHH - HH,
        # Standard error of difference
        MISSINGHH_se = sqrt(NEWHH_se^2 + HH_se^2)
      ) |>
      arrange(YEAR, msa, AGEGROUP) |>
      compute_parquet(path = missing_hh_file)
  } else {
    simple_target_hh_df <- read_parquet_duckdb(missing_hh_file)
  }
}

simple_target_hh_df <- map(
  2005:analysis_year,
  calculate_missing_households,
  .progress = TRUE
) |>
  bind_rows()

# Aggregate results to the metropolitan area level.
metro_totals <- simple_target_hh_df |>
  # Sum up the household counts by year and MSA.
  summarise(
    HH = sum(HH),
    HH_se = sqrt(sum(HH_se^2)),
    NEWHH = sum(NEWHH),
    NEWHH_se = sqrt(sum(NEWHH_se^2)),
    MISSINGHH = sum(MISSINGHH),
    MISSINGHH_se = sqrt(sum(MISSINGHH_se^2)),
    .by = c(YEAR, msa) # PUMA is now aggregated out
  ) |>
  # Join the pre-calculated housing unit counts.
  left_join(CO_SEASONAL_HOUSING) |>
  left_join(CO_UNINHABITABLE_HOUSING) |>
  left_join(CO_VACANT_HOUSING) |>
  left_join(CO_HOUSING_UNITS) |>
  # Calculate the final housing shortage metrics.
  mutate(
  vacancy_rate = housing_units_vacant / housing_units,
    necessary_units = (HH + MISSINGHH) / (1 - target_vacancy_rate),
    # SE for necessary_units using delta method
    necessary_units_se = sqrt(HH_se^2 + MISSINGHH_se^2) / (1 - target_vacancy_rate),
    available_units = housing_units -
      housing_units_seasonal -
      replace_na(housing_units_uninhabitable, 0),
    # Combine SEs for available_units (assuming independence)
    available_units_se = sqrt(
      housing_units_se^2 + 
      housing_units_seasonal_se^2 + 
      replace_na(housing_units_uninhabitable_se^2, 0)
    ),
    underproduction = necessary_units - available_units,
    # SE for underproduction
    underproduction_se = sqrt(necessary_units_se^2 + available_units_se^2)
  ) |>
  arrange(YEAR, msa)

# Create a statewide total by summarizing the metro-level data.
statewide_totals <- metro_totals |>
  summarize(
across(
      c(HH, NEWHH, MISSINGHH, housing_units_vacant, housing_units_seasonal, 
        housing_units_uninhabitable, housing_units, necessary_units, 
        available_units, underproduction),
      \(x) sum(x, na.rm = TRUE)
    ),
    # Standard errors: sum variances then sqrt
    across(
      ends_with("_se"),
      \(x) sqrt(sum(x^2, na.rm = TRUE))
    ),
    .by = YEAR
  ) |>
  # Add a label to identify these rows as statewide totals.
  mutate(
    msa = "Colorado Statewide",
    vacancy_rate = housing_units_vacant / housing_units,
    .after = YEAR
  )

# Combine the metro-level and statewide totals into a single dataframe.
all_totals <- metro_totals |>
  bind_rows(statewide_totals)

# Write the final results to a CSV file.
write_csv(
  simple_target_hh_df,
  paste0("./data/", ipums_file_num, "/simple_target_hh_df.csv")
)

write_csv(all_totals, paste0("./data/", ipums_file_num, "/all_totals.csv"))
view(all_totals)
