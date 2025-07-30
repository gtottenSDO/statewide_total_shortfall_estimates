library(tidyverse)
library(ipumsr)
# library(survey)
# library(spatstat.univar)
library(duckplyr)

# Get IPUMS variable lists from existing .xml files usa_00063.xml and usa_00064.xml
# ddi63 <- read_ipums_ddi("./data/ipums_raw/usa_00063.xml")
# ddi64 <- read_ipums_ddi("./data/ipums_raw/usa_00064.xml")
# ddi93 <- read_ipums_ddi("./data/ipums_raw/usa_00093.xml")
# ipums_val_labels(ddi63, "YEAR")
# ipums_var_list <- ipums_var_info(ddi63) |>
#   bind_rows(
#     ipums_var_info(ddi64)
#   ) |>
#   filter(var_name != "STATEFIP") |>
#   pull(var_name) |>
#   unique() |>
#   append(list(
#     "MET2013",
#     "MET2013ERR",
#     var_spec(
#       name = "STATEFIP",
#       case_selections = "08"
#     )
#   ))

# # get list of samples for data
# ipums_samples <- get_sample_info("usa") |>
#   #filter to 1 year acs samples
#   # filter(str_detect(description, ".ACS$")) |>
#   filter(str_detect(name, "us2\\d{3}a")) |>
#   pull(name)

# ipums_extract <- define_extract_micro(
#   collection = "usa",
#   description = "acs samples for Colorado housing shortage",
#   samples = ipums_samples,
#   variables = ipums_var_list,
#   case_select_who = "households",
#   data_structure = "hierarchical" #required to vacant houesholds
# )

# ipums_extract_submitted <- ipums_extract |>
#   submit_extract()

# ipums_extract_submitted |>
#   wait_for_extract() |>
#   download_extract(
#     download_dir = "./data/ipums_raw"
#   )

ipums_data <- read_ipums_micro_list("./data/ipums_raw/usa_00099.xml")
housing_df <- ipums_data$HOUSEHOLD |>
  zap_ipums_attributes() |>
  # add metro names
  left_join(
    ipums_val_labels(ipums_data$HOUSEHOLD$MET2013) |>
      rename(MET2013 = val) |>
      rename(msa = lbl)
  ) |>
  #remove rectype
  select(-RECTYPE) |>
  # need to remove group quarters
  filter(!(GQ %in% c(3, 4))) |>
  # define seasonal housing
  mutate(SEASONAL = VACANCY == 4) |>
  # define uninhabitable housing
  mutate(
    UNIHABITABLE = (KITCHEN == 1 | PLUMBING == 10) &
      if_else(is.na(VACDUR), VACANCY %in% c(1:3), VACDUR >= 7),
    VACAVAIL = VACANCY %in% c(1:3)
  ) |>
  # define Vacancy
  mutate(VACANT = VACANCY != 0)


CO_HOUSING_UNITS <- housing_df |>
  count(YEAR, msa, wt = HHWT, name = "housing_units")

CO_SEASONAL_HOUSING <- housing_df |>
  filter(SEASONAL) |>
  count(YEAR, msa, wt = HHWT, name = "housing_units_seasonal")

CO_UNIHABITABLE_HOUSING <- housing_df |>
  filter(UNIHABITABLE) |>
  count(YEAR, msa, wt = HHWT, name = "housing_units_uninhabitable")

CO_VACANT_HOUSING <- housing_df |>
  filter(VACAVAIL) |>
  count(YEAR, msa, wt = HHWT, name = "housing_units_vacant")


CO_HOUSEHOLDS <- housing_df |>
  filter(STATEFIP == 8 & VACANCY == 0) |>
  count(YEAR, msa, wt = HHWT)

# this is our data frame we use for inflation adjusting calculations
cpi_df <- "data/CPIAUCSL.csv" |>
  read_csv(col_types = cols()) |>
  mutate(YEAR = year(observation_date)) |>
  group_by(YEAR) |>
  summarize(cpi = mean(CPIAUCSL), .groups = "drop") |>
  mutate(adj_factor = first(cpi[YEAR == 2023]) / cpi)

person_df <- ipums_data$PERSON |>
  zap_ipums_attributes() |>
  select(-RECTYPE) |>
  mutate(HEADSHIP = RELATE == 1) # recode headship to binary

hh_df <- housing_df |>
  # join with person data to get headship status
  right_join(
    person_df,
    by = c("YEAR", "SERIAL", "CBSERIAL", "SAMPLE")
  ) |>
  # we want to filter out the group quarter population and say that
  # their impact on the housing market is exogenous to other factors
  filter(GQ %in% c(0, 1, 2, 5)) |>
  # recode missing codes to either missing or zero
  mutate(HHINCOME = if_else(HHINCOME == 9999999, 0, HHINCOME)) |>
  mutate(OWNCOST = if_else(OWNCOST == 99999, 0, OWNCOST)) |>
  mutate(INCTOT = if_else(INCTOT == 9999999, NA_integer_, INCTOT)) |>
  mutate(
    HOUSINGCOST = case_when(
      OWNERSHP == 1 ~ OWNCOST,
      OWNERSHP == 2 ~ RENTGRS,
      TRUE ~ NA_integer_
    )
  ) |>
  # income totals are NA for most people 15 and younger so lets set their
  # income to 0
  mutate(INCTOT = if_else(is.na(INCTOT) & AGE <= 15, 0, INCTOT)) |>
  # inflation adjust our monetary values
  left_join(cpi_df, by = "YEAR") |>
  mutate(HOUSINGCOST = HOUSINGCOST * adj_factor) |>
  mutate(HHINCOME = HHINCOME * adj_factor) |>
  mutate(INCTOT = INCTOT * adj_factor) |>
  mutate(FTOTINC = FTOTINC * adj_factor) |>
  # recode headship to binary
  mutate(HEADSHIP = RELATE == 1) |>
  # put age groups into five year bins
  mutate(
    AGEGROUP = cut(
      AGE,
      c(0, seq(15, 65, 5), Inf),
      include.lowest = TRUE,
      right = FALSE
    )
  ) |>
  # for maths sake INCTOT and HOUSING cost will be set to 1 when less than
  # this means we treat every one with income loss as having 1 income
  # not a big deal for housing cost which moves from 0 to 1
  mutate(INCTOT = if_else(INCTOT < 1, 1, INCTOT)) |>
  mutate(HOUSINGCOST = if_else(HOUSINGCOST < 1, 1, HOUSINGCOST)) |>
  # calculate housing cost ratio
  mutate(HCOSTRATIO = HOUSINGCOST / (INCTOT) * 12)

co_hr_table <- hh_df |>
  filter(STATEFIP == 8, AGE >= 15) |>
  group_by(AGEGROUP, YEAR) |>
  summarise(HEADSHIP = weighted.mean(HEADSHIP, PERWT), .groups = "drop") |>
  pivot_wider(names_from = YEAR, values_from = HEADSHIP)

analysis_year <- 2021
baseline_year <- 2000
minimum_age <- 18
maximum_age <- 45
target_vacancy_rate <- .05

hh_df <- hh_df |>
  mutate(
    age_dummy = (AGE >= minimum_age & AGE < maximum_age)
  ) |>
  relocate(msa, .after = MET2013)

simple_target_hh_df <- hh_df |>
  filter(
    STATEFIP == 8,
    YEAR == baseline_year
  ) |>
  summarise(
    NEWHH = weighted.mean(HEADSHIP, PERWT),
    .by = c(msa, AGEGROUP, MET2013, age_dummy)
  ) |>
  right_join(
    hh_df |>
      filter(
        STATEFIP == 8
      ),
    by = join_by(msa, AGEGROUP, MET2013, age_dummy)
  ) |>
  summarise(
    NEWHH_ = sum(NEWHH * PERWT),
    HH = sum(HEADSHIP * PERWT),
    .by = c(msa, AGEGROUP, MET2013, PUMA, age_dummy, YEAR)
  ) |>
  mutate(
    NEWHH = if_else(
      age_dummy & NEWHH_ > HH,
      NEWHH_,
      HH
    ),
    MISSINGHH = NEWHH - HH
  ) |>
  arrange(YEAR, msa, AGEGROUP)


metro_totals <- simple_target_hh_df |>
  summarise(
    HH = sum(HH),
    NEWHH = sum(NEWHH),
    MISSINGHH = sum(MISSINGHH),
    .by = c(YEAR, msa)
  ) |>
  left_join(
    CO_SEASONAL_HOUSING
  ) |>
  left_join(
    CO_UNIHABITABLE_HOUSING
  ) |>
  left_join(
    CO_VACANT_HOUSING
  ) |>
  left_join(
    CO_HOUSING_UNITS
  ) |>
  mutate(
    vacancy_rate = housing_units_vacant / housing_units,
    necessary_units = (HH + MISSINGHH) / (1 - target_vacancy_rate),
    available_units = housing_units -
      housing_units_seasonal -
      replace_na(housing_units_uninhabitable, 0),
    underproduction = necessary_units - available_units
  ) |>
  arrange(YEAR, msa)

statewide_totals <- metro_totals |>
  summarize(
    across(-c(msa), \(x) sum(x, na.rm = TRUE)),
    .by = YEAR
  ) |>
  mutate(
    msa = "Colorado Statewide",
    .after = YEAR
  )

all_totals <- metro_totals |>
  bind_rows(statewide_totals)

write_csv(all_totals, "all_totals.csv")
