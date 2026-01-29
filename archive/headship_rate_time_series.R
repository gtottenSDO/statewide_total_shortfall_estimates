library(tidyverse)
library(ipumsr)
library(survey)

housing_df <- read_ipums_micro("./data/ipums_raw/usa_00069.xml") %>%
  # need to remove group quarters
  filter(!(GQ %in% c(3, 4))) %>%
  # define seasonal housing
  # consider adding in the code 7 which is for migrant farm workers housing
  mutate(SEASONAL = VACANCY == 4) %>%
  # DONOT KEEP THIS IN HERE
  mutate(VACDUR = if_else(is.na(VACDUR), 7, VACDUR)) %>%
  # define uninhabitable housing
  mutate(UNIHABITABLE = (KITCHEN == 1 | PLUMBING == 10) & VACANCY != 0) %>%
  # define Vacancy
  mutate(VACANT = VACANCY != 0)

housing_stat_df <- housing_df %>%
  filter(STATEFIP == 8) %>%
  group_by(YEAR) %>%
  summarize(
    SEASONAL = sum(HHWT * SEASONAL),
    UNIHABITABLE = sum(UNIHABITABLE*HHWT),
    HOUSEHOLDS = sum(HHWT * (VACANCY == 0)),
    HU = sum(HHWT))

hh_df <- read_ipums_micro("./data/ipums_raw/usa_00070.xml") %>%
  # we want to filter out the group quarter population and say that
  # their impact on the housing market is exogenous to other factors
  filter(GQ %in% c(0,1,2,5)) %>%
  # recode headship to binary
  mutate(HEADSHIP = RELATE == 1) %>%
  # put age groups into five year bins
  mutate(AGEGROUP = cut(
    AGE, c(0, seq(15, 85, 5), Inf),  include.lowest = TRUE, right = FALSE)
  )

missing_hh_df <- hh_df %>%
  filter(STATEFIP == 8, AGE >= 15, YEAR == 2000) %>%
  group_by(AGEGROUP) %>%
  summarise(NEWHH = weighted.mean(HEADSHIP, PERWT), .groups = "drop") %>%
  right_join(
    hh_df %>%
      filter(STATEFIP == 8, AGE >= 15),
    by = "AGEGROUP"
  ) %>%
  group_by(YEAR, AGEGROUP) %>%
  summarise(
    NEWHH = sum(NEWHH*PERWT),
    HH = sum(HEADSHIP*PERWT),
    .groups = "drop") %>%
  mutate(MISSINGHH = NEWHH - HH)

missing_hh_df %>%
  group_by(YEAR) %>%
  summarise(MISSINGHH = round(sum(MISSINGHH))) %>%
  left_join(housing_stat_df, by = "YEAR") %>%
  mutate(UNDERPROD = (HOUSEHOLDS + MISSINGHH) * 1.05 - (HU - UNIHABITABLE - SEASONAL)) %>%
  mutate(DELTA = HU - lag(HU))
