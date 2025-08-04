# Load necessary libraries for data manipulation and analysis.
# tidyverse: A collection of R packages for data science (e.g., dplyr, ggplot2).
# ipumsr: Helps with reading and managing data from IPUMS.
# duckplyr: A faster backend for dplyr, useful for large datasets.
library(tidyverse)
library(ipumsr)
library(duckplyr)

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
#
# # Example of how to view value labels for a variable.
# # ipums_val_labels(ddi63, "YEAR")
#
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
#     # Specify STATEFIP but select only cases for Colorado (FIPS code 08).
#     var_spec(
#       name = "STATEFIP",
#       case_selections = "08"
#     )
#   ))
#
# # Get a list of all 1-year ACS sample names from IPUMS.
# ipums_samples <- get_sample_info("usa") |>
#   # Filter to get samples that match the ACS 1-year naming convention (e.g., "us2021a").
#   filter(str_detect(name, "us2\\d{3}a")) |>
#   pull(name) # Extract the sample names.
#
# # Define the data extract request for IPUMS.
# ipums_extract <- define_extract_micro(
#   collection = "usa",
#   description = "acs samples for Colorado housing shortage",
#   samples = ipums_samples,
#   variables = ipums_var_list,
#   case_select_who = "households", # Select data at the household level.
#   data_structure = "hierarchical" # Required to get data on vacant households.
# )
#
# # Submit the extract request to the IPUMS server.
# ipums_extract_submitted <- ipums_extract |>
#   submit_extract()
#
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
ipums_data <- read_ipums_micro_list("./data/ipums_raw/usa_00099.xml")

# Prepare the HOUSEHOLD level dataframe.
housing_df <- ipums_data$HOUSEHOLD |>
  # Convert IPUMS-specific attributes (like labeled integers) to standard R data types.
  zap_ipums_attributes() |>
  # Join with metropolitan area labels to get readable MSA names.
  left_join(
    ipums_val_labels(ipums_data$HOUSEHOLD$MET2013) |>
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

# -----------------------------------------------------------------------------
# Calculate Aggregate Housing Statistics for Colorado
# -----------------------------------------------------------------------------

# Calculate total housing units per year and MSA, using household weights (HHWT).
CO_HOUSING_UNITS <- housing_df |>
  count(YEAR, msa, wt = HHWT, name = "housing_units")

# Calculate total seasonal housing units.
CO_SEASONAL_HOUSING <- housing_df |>
  filter(SEASONAL) |>
  count(YEAR, msa, wt = HHWT, name = "housing_units_seasonal")

# Calculate total uninhabitable housing units.
CO_UNIHABITABLE_HOUSING <- housing_df |>
  filter(UNIHABITABLE) |>
  count(YEAR, msa, wt = HHWT, name = "housing_units_uninhabitable")

# Calculate total vacant-and-available housing units.
CO_VACANT_HOUSING <- housing_df |>
  filter(VACAVAIL) |>
  count(YEAR, msa, wt = HHWT, name = "housing_units_vacant")

# Calculate the total number of occupied households in Colorado (STATEFIP 8, VACANCY 0).
CO_HOUSEHOLDS <- housing_df |>
  filter(STATEFIP == 8 & VACANCY == 0) |>
  count(YEAR, msa, wt = HHWT)

# -----------------------------------------------------------------------------
# Prepare Inflation Adjustment Data
# -----------------------------------------------------------------------------

# Read Consumer Price Index (CPI) data from a local CSV file.
cpi_df <- "data/CPIAUCSL.csv" |>
  read_csv(col_types = cols()) |>
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
  zap_ipums_attributes() |>
  select(-RECTYPE) |>
  # Create a binary variable for head of household (RELATE code 1).
  mutate(HEADSHIP = RELATE == 1)

# Create the main analysis dataframe by joining household and person data.
hh_df <- housing_df |>
  # Join housing data to person data. A right_join keeps all persons.
  right_join(
    person_df,
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
    AGEGROUP = cut(
      AGE,
      c(0, seq(15, 65, 5), Inf),
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  # To avoid division by zero or negative values, set any income/cost less than 1 to 1.
  mutate(INCTOT = if_else(INCTOT < 1, 1, INCTOT)) |>
  mutate(HOUSINGCOST = if_else(HOUSINGCOST < 1, 1, HOUSINGCOST)) |>
  # Calculate the housing cost to income ratio (annualized).
  mutate(HCOSTRATIO = HOUSINGCOST / (INCTOT) * 12)

# Create a table showing the headship rate (propensity to be head of household) by age group over the years.
co_hr_table <- hh_df |>
  filter(STATEFIP == 8, AGE >= 15) |> # Filter for Colorado residents age 15+.
  group_by(AGEGROUP, YEAR) |>
  # Calculate the weighted mean headship rate using person weights (PERWT).
  summarise(HEADSHIP = weighted.mean(HEADSHIP, PERWT), .groups = "drop") |>
  # Reshape data to have years as columns for easy comparison.
  pivot_wider(names_from = YEAR, values_from = HEADSHIP)

# -----------------------------------------------------------------------------
# Housing Shortage Calculation
# -----------------------------------------------------------------------------

# Set key parameters for the analysis.
analysis_year <- 2021
baseline_year <- 2000 # The year whose headship rates will be used as a benchmark.
minimum_age <- 18
maximum_age <- 45
target_vacancy_rate <- .05 # A 5% target vacancy rate for a healthy housing market.

# Add a dummy variable to the main dataframe to flag the target age group.
hh_df <- hh_df |>
  mutate(
    age_dummy = (AGE >= minimum_age & AGE < maximum_age)
  ) |>
  relocate(msa, .after = MET2013) # Move MSA column for better readability.

# This is the core logic to estimate "missing households".
# It compares current headship rates to a baseline year's rates.
simple_target_hh_df <- hh_df |>
  # Step 1: Calculate the baseline headship rate for each MSA and age group in the baseline_year.
  filter(
    STATEFIP == 8,
    YEAR == baseline_year
  ) |>
  summarise(
    NEWHH = weighted.mean(HEADSHIP, PERWT), # This is the baseline headship rate.
    .by = c(msa, AGEGROUP, MET2013, age_dummy)
  ) |>
  # Step 2: Join the baseline headship rates back to the full Colorado person-level data.
  right_join(
    hh_df |> filter(STATEFIP == 8),
    by = join_by(msa, AGEGROUP, MET2013, age_dummy)
  ) |>
  # Step 3: Calculate actual vs. hypothetical households for each year.
  summarise(
    # Hypothetical households if baseline headship rates applied to today's population.
    NEWHH_ = sum(NEWHH * PERWT),
    # Actual number of households today.
    HH = sum(HEADSHIP * PERWT),
    .by = c(msa, AGEGROUP, MET2013, PUMA, age_dummy, YEAR)
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
    # Step 5: Calculate "missing households" as the difference between target and actual.
    MISSINGHH = NEWHH - HH
  ) |>
  arrange(YEAR, msa, AGEGROUP)


# Aggregate results to the metropolitan area level.
metro_totals <- simple_target_hh_df |>
  # Sum up the household counts by year and MSA.
  summarise(
    HH = sum(HH),
    NEWHH = sum(NEWHH),
    MISSINGHH = sum(MISSINGHH),
    .by = c(YEAR, msa) # PUMA is now aggregated out
  ) |>
  # Join the pre-calculated housing unit counts.
  left_join(CO_SEASONAL_HOUSING) |>
  left_join(CO_UNIHABITABLE_HOUSING) |>
  left_join(CO_VACANT_HOUSING) |>
  left_join(CO_HOUSING_UNITS) |>
  # Calculate the final housing shortage metrics.
  mutate(
    vacancy_rate = housing_units_vacant / housing_units,
    # Total units needed to house the target population at the target vacancy rate.
    necessary_units = (HH + MISSINGHH) / (1 - target_vacancy_rate),
    # Total supply of units available for the general population.
    available_units = housing_units -
      housing_units_seasonal -
      replace_na(housing_units_uninhabitable, 0),
    # The final estimate of housing underproduction (shortage).
    underproduction = necessary_units - available_units
  ) |>
  arrange(YEAR, msa)

# Create a statewide total by summarizing the metro-level data.
statewide_totals <- metro_totals |>
  summarize(
    # Sum all numeric columns, ignoring NAs.
    across(-c(msa), \(x) sum(x, na.rm = TRUE)),
    .by = YEAR
  ) |>
  # Add a label to identify these rows as statewide totals.
  mutate(
    msa = "Colorado Statewide",
    .after = YEAR
  )

# Combine the metro-level and statewide totals into a single dataframe.
all_totals <- metro_totals |>
  bind_rows(statewide_totals)

# Write the final results to a CSV file.
write_csv(all_totals, "all_totals.csv")
