# Load necessary libraries for data manipulation and analysis.
# tidyverse: Collection of R packages for data science (dplyr, ggplot2, tidyr, etc.)
# ipumsr: Read and manage IPUMS microdata extracts
# duckplyr: High-performance dplyr backend using DuckDB for large datasets
# srvyr: Survey analysis with dplyr syntax
library(tidyverse)
library(ipumsr)
library(duckplyr)
library(srvyr)

# -----------------------------------------------------------------------------
# IPUMS Extract Definition (Commented Out)
# -----------------------------------------------------------------------------
# This section contains code to define, submit, and download a custom data 
# extract from IPUMS USA. This is a one-time setup that was already completed.
# The analysis below uses the downloaded data file.
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

# Read IPUMS microdata extract containing hierarchical household and person records.
# Data is cached as parquet files for faster subsequent loads.

ipums_file_num <- "00112" # IPUMS extract number

# Check if parquet files exist; if not, convert XML data to parquet format
if (
  !file.exists(paste0("./data/household_", ipums_file_num, ".parquet")) ||
    !file.exists(paste0("./data/person_", ipums_file_num, ".parquet"))
) {
  # Read raw IPUMS data and convert to parquet for faster future loading
  ipums_data <- read_ipums_micro_list("./data/ipums_raw/usa_00112.xml")

  ipums_data$HOUSEHOLD |>
    zap_ipums_attributes() |>
    compute_parquet(path = "./data/household_00112.parquet")

  ipums_data$PERSON |>
    zap_ipums_attributes() |>
    compute_parquet(path = "./data/person_00112.parquet")

  rm(ipums_data) # Free memory
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

ipums_ddi <- read_ipums_ddi("./data/ipums_raw/usa_00112.xml") # Metadata and variable labels

# Prepare household-level data
housing_df <- ipums_data$HOUSEHOLD |>
  # Add readable MSA names from metadata
  left_join(
    ipums_val_labels(ipums_ddi, "MET2013") |>
      rename(MET2013 = val) |>
      rename(msa = lbl)
  ) |>
  select(-RECTYPE) |> # Remove record type indicator (not needed after separation)
  # Exclude institutional group quarters (military barracks, correctional facilities, etc.)
  filter(!(GQ %in% c(3, 4))) |>
  # Create housing unit classification flags
  mutate(SEASONAL = VACANCY == 4) |> # Seasonal/recreational units
  mutate(
    # Uninhabitable: lacks kitchen/plumbing AND vacant 6+ months (or certain vacancy types)
    UNIHABITABLE = (KITCHEN == 1 | PLUMBING == 10) &
      if_else(is.na(VACDUR), VACANCY %in% c(1:3), VACDUR >= 7),
    # Available for rent or sale
    VACAVAIL = VACANCY %in% c(1:3)
  ) |>
  mutate(VACANT = VACANCY != 0) # All vacant units

# Create survey design object with household weights and replicate weights
housing_svy <- housing_df |>
  as_survey_design(
    weight = HHWT, # Household weight
    repweights = matches("REPWT[0-9]+"), # Replicate weights for variance estimation
    type = "ACS",
    mse = TRUE # Use MSE method for standard errors
  )
# -----------------------------------------------------------------------------
# Calculate Aggregate Housing Statistics for Colorado
# -----------------------------------------------------------------------------

# Helper function: count housing units with survey weights, cache to parquet
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

# Calculate weighted housing counts by year and MSA
CO_HOUSING_UNITS <- housing_svy |>
  survey_count_parquet("housing_units")


CO_SEASONAL_HOUSING <- housing_svy |>
  filter(SEASONAL) |>
  survey_count_parquet("housing_units_seasonal")


CO_UNINHABITABLE_HOUSING <- housing_svy |>
  filter(UNIHABITABLE) |>
  survey_count_parquet("housing_units_uninhabitable")


CO_VACANT_HOUSING <- housing_svy |>
  filter(VACAVAIL) |>
  survey_count_parquet("housing_units_vacant")

CO_HOUSEHOLDS <- housing_svy |>
  filter(STATEFIP == 8 & VACANCY == 0) |> # Colorado, occupied units only
  survey_count_parquet("households")

# -----------------------------------------------------------------------------
# Inflation Adjustment Data
# -----------------------------------------------------------------------------

# Load CPI data and calculate inflation adjustment factors to 2023 dollars
cpi_df <- read_csv_duckdb("data/CPIAUCSL.csv") |>
  mutate(YEAR = year(observation_date)) |>
  # Average CPI by year
  group_by(YEAR) |>
  summarize(cpi = mean(CPIAUCSL), .groups = "drop") |>
  # Adjustment factor: converts any year to 2023 dollars
  mutate(adj_factor = first(cpi[YEAR == 2023]) / cpi)

# -----------------------------------------------------------------------------
# Combined Household and Person Data
# -----------------------------------------------------------------------------

# Prepare person-level data
person_df <- ipums_data$PERSON |>
  select(-RECTYPE) |>
  mutate(HEADSHIP = RELATE == 1) # Household head indicator

# Analysis parameters
analysis_year <- 2024
baseline_year <- 2000 # Reference year for headship rate comparison
minimum_age <- 18
maximum_age <- 45
target_vacancy_rate <- .05 # 5% healthy vacancy rate

# Check if combined household-person file exists; create if needed
if (!file.exists(paste0("./data/hh_", ipums_file_num, ".parquet"))) {
  # Join household and person data, adjust for inflation, create derived variables
  hh_df <- collect(housing_df) |>
    # Keep all persons (right_join)
    right_join(
      collect(person_df),
      by = c("YEAR", "SERIAL", "CBSERIAL", "SAMPLE") # Household identifiers
    ) |>
    # Non-institutionalized population only
    filter(GQ %in% c(0, 1, 2, 5)) |>
    # Recode IPUMS missing/topcode values for income variables
    mutate(HHINCOME = if_else(HHINCOME == 9999999, 0, HHINCOME)) |>
    mutate(OWNCOST = if_else(OWNCOST == 99999, 0, OWNCOST)) |>
    mutate(INCTOT = if_else(INCTOT == 9999999, NA_integer_, INCTOT)) |>
    # Combine owner and renter housing costs
    mutate(
      HOUSINGCOST = case_when(
        OWNERSHP == 1 ~ OWNCOST,
        OWNERSHP == 2 ~ RENTGRS,
        TRUE ~ NA_integer_
      )
    ) |>
    # Assume children ≤15 have zero income when missing
    mutate(INCTOT = if_else(is.na(INCTOT) & AGE <= 15, 0, INCTOT)) |>
    # Apply inflation adjustment
    left_join(cpi_df, by = "YEAR") |>
    mutate(HOUSINGCOST = HOUSINGCOST * adj_factor) |>
    mutate(HHINCOME = HHINCOME * adj_factor) |>
    mutate(INCTOT = INCTOT * adj_factor) |>
    mutate(FTOTINC = FTOTINC * adj_factor) |>
    mutate(HEADSHIP = RELATE == 1) |>
    # Create 5-year age groups
    mutate(
      AGEGROUP = as.character(cut(
        AGE,
        c(0, seq(15, 65, 5), Inf),
        include.lowest = TRUE,
        right = FALSE
      ))
    ) |>
    # Prevent division by zero in cost-to-income calculations
    mutate(INCTOT = if_else(INCTOT < 1, 1, INCTOT)) |>
    mutate(HOUSINGCOST = if_else(HOUSINGCOST < 1, 1, HOUSINGCOST)) |>
    # Annualized housing cost-to-income ratio
    mutate(HCOSTRATIO = HOUSINGCOST / (INCTOT) * 12) |>
    mutate(
      age_dummy = (AGE >= minimum_age & AGE < maximum_age)
    ) |>
    relocate(msa, .after = MET2013) |>
    compute_parquet(path = paste0("./data/hh_", ipums_file_num, ".parquet"))
} else {
  hh_df <- read_parquet_duckdb(
    paste0("./data/hh_", ipums_file_num, ".parquet"),
    prudence = "lavish"
  )
}

# Create survey design object for person-level analysis
hh_svy <- hh_df |>
  as_survey_design(
    weight = PERWT, # Person weight
    repweights = matches("REPWTP[0-9]+"), # Person replicate weights
    type = "ACS",
    mse = TRUE
  )

# Calculate headship rates by age group and year for Colorado
if (
  !file.exists(paste0("./data/co_headship_rates_", ipums_file_num, ".parquet"))
) {
  co_hr_table <- hh_svy |>
    filter(STATEFIP == 8, AGE >= 15) |> # Colorado residents age 15+
    group_by(AGEGROUP, YEAR) |>
    # Weighted mean headship rate (proportion who are household heads)
    summarise(
      HEADSHIP = survey_mean(HEADSHIP, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Reshape for comparison across years
    pivot_longer(
      cols = starts_with("HEADSHIP"),
      names_to = "TYPE",
      values_to = "HEADSHIP"
    ) |>
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

# Core methodology: estimate "missing households" by comparing current headship
# rates to baseline year rates, then calculate housing units needed

# Calculate baseline headship rates (reference year for comparison)
if (
  !file.exists(paste0(
    "./data/baseline_headship_rates_",
    ipums_file_num,
    ".parquet"
  ))
) {
  baseline_df <- hh_svy |>
    filter(
      STATEFIP == 8,
      YEAR == baseline_year
    ) |>
    group_by(msa, AGEGROUP, MET2013, age_dummy) |>
    summarise(
      NEWHR = survey_mean(HEADSHIP), # Baseline headship rate
      .groups = "drop"
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

# Function to calculate missing households for a given year
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
      # Join baseline headship rates to current population
      left_join(
        baseline_df,
        by = join_by(msa, AGEGROUP, MET2013, age_dummy),
        relationship = "one-to-one"
      ) |>
      as_survey_design(
        weight = PERWT,
        repweights = matches("REPWTP[0-9]+"),
        type = "ACS",
        mse = TRUE
      ) |>
      group_by(msa, AGEGROUP, MET2013, NEWHR, PUMA, age_dummy, YEAR) |>
      # Calculate actual vs. expected households
      summarise(
        # Expected households if baseline headship rates applied to current population
        NEWHH_ = survey_total(NEWHR),
        # Actual households
        HH = survey_total(HEADSHIP),
        .groups = "drop"
      ) |>
      # Determine target household count
      mutate(
        # For target age group: use expected if higher than actual (suppressed household formation)
        # Otherwise use actual (headship may have increased)
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
        # Missing households = target - actual
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

# Calculate missing households for all analysis years
simple_target_hh_df <- map(
  2005:analysis_year,
  calculate_missing_households,
  .progress = TRUE
) |>
  bind_rows()

# Aggregate to metropolitan area level
metro_totals <- simple_target_hh_df |>
  # Sum across age groups and PUMAs within each MSA-year
  summarise(
    HH = sum(HH),
    HH_se = sqrt(sum(HH_se^2)),
    NEWHH = sum(NEWHH),
    NEWHH_se = sqrt(sum(NEWHH_se^2)),
    MISSINGHH = sum(MISSINGHH),
    MISSINGHH_se = sqrt(sum(MISSINGHH_se^2)),
    .by = c(YEAR, msa)
  ) |>
  # Join housing unit counts
  left_join(CO_SEASONAL_HOUSING) |>
  left_join(CO_UNINHABITABLE_HOUSING) |>
  left_join(CO_VACANT_HOUSING) |>
  left_join(CO_HOUSING_UNITS) |>
  # Calculate housing shortage metrics
  mutate(
    vacancy_rate = housing_units_vacant / housing_units,
    # Units needed to house all households (actual + missing) with healthy vacancy
    necessary_units = (HH + MISSINGHH) / (1 - target_vacancy_rate),
    # Standard error using delta method
    necessary_units_se = sqrt(HH_se^2 + MISSINGHH_se^2) / (1 - target_vacancy_rate),
    # Units available after excluding seasonal and uninhabitable
    available_units = housing_units -
      housing_units_seasonal -
      replace_na(housing_units_uninhabitable, 0),
    # Combined standard error (assuming independence)
    available_units_se = sqrt(
      housing_units_se^2 + 
      housing_units_seasonal_se^2 + 
      replace_na(housing_units_uninhabitable_se^2, 0)
    ),
    # Housing shortage = needed - available
    underproduction = necessary_units - available_units,
    # Standard error of shortage
    underproduction_se = sqrt(necessary_units_se^2 + available_units_se^2)
  ) |>
  arrange(YEAR, msa)

# Create statewide totals by summing across MSAs
statewide_totals <- metro_totals |>
  summarize(
    # Sum counts across metros
    across(
      c(HH, NEWHH, MISSINGHH, housing_units_vacant, housing_units_seasonal, 
        housing_units_uninhabitable, housing_units, necessary_units, 
        available_units, underproduction),
      \(x) sum(x, na.rm = TRUE)
    ),
    # Combine standard errors (sqrt of sum of variances)
    across(
      ends_with("_se"),
      \(x) sqrt(sum(x^2, na.rm = TRUE))
    ),
    .by = YEAR
  ) |>
  mutate(
    msa = "Colorado Statewide",
    vacancy_rate = housing_units_vacant / housing_units,
    .after = YEAR
  )

# Combine metro and statewide results
all_totals <- metro_totals |>
  bind_rows(statewide_totals)

# Export results to CSV
write_csv(
  simple_target_hh_df,
  paste0("./data/", ipums_file_num, "/simple_target_hh_df.csv")
)

write_csv(all_totals, paste0("./data/", ipums_file_num, "/all_totals.csv"))

