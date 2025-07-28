library(tidyverse)
library(ipumsr)
library(survey)
library(spatstat.univar)
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
#   as.list()

# ipums_var_list[[length(ipums_var_list) + 1]] <- var_spec(
#   name = "STATEFIP",
#   case_selections = "08"
# )

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

ipums_data <- read_ipums_micro_list("./data/ipums_raw/usa_00097.xml")
housing_df <- ipums_data$HOUSEHOLD |>
  zap_ipums_attributes() |>
  #remove rectype
  select(-RECTYPE) |>
  # need to remove group quarters
  filter(!(GQ %in% c(3, 4))) |>
  # define seasonal housing
  mutate(SEASONAL = VACANCY == 4) |>
  # define uninhabitable housing
  mutate(UNIHABITABLE = (KITCHEN == 1 | PLUMBING == 10) & VACDUR >= 7) |>
  # define Vacancy
  mutate(VACANT = VACANCY != 0)


CO_HOUSING_UNITS <- housing_df |>
  count(YEAR, wt = HHWT) |>
  pull(n, name = "YEAR")

CO_SEASONAL_HOUSING <- housing_df |>
  filter(SEASONAL) |>
  count(YEAR, wt = HHWT) |>
  pull(n, name = "YEAR")

CO_UNIHABITABLE_HOUSING <- housing_df |>
  filter(UNIHABITABLE) |>
  count(YEAR, wt = HHWT) |>
  pull(n, name = "YEAR")

housing_df |>
  mutate(
    HOUSINGTYPE = case_when(
      SEASONAL ~ "Seasonal",
      UNIHABITABLE ~ "Uninhabitable",
      TRUE ~ "Remaining"
    )
  ) |>
  group_by(HOUSINGTYPE) |>
  summarize(USHH = sum(HHWT), .groups = "drop") |>
  mutate(USHH = str_c(sprintf("%.2f", USHH / sum(USHH) * 100), "%")) |>
  left_join(
    housing_df |>
      filter(STATEFIP == 8) |>
      mutate(
        HOUSINGTYPE = case_when(
          SEASONAL ~ "Seasonal",
          UNIHABITABLE ~ "Uninhabitable",
          TRUE ~ "Remaining"
        )
      ) |>
      group_by(HOUSINGTYPE) |>
      summarize(COHH = sum(HHWT), .groups = "drop") |>
      mutate(COHH = str_c(sprintf("%.2f", COHH / sum(COHH) * 100), "%")),
    by = "HOUSINGTYPE"
  ) |>
  rename(US = USHH, COLORADO = COHH)

housing_df |>
  filter(!(SEASONAL | UNIHABITABLE)) |>
  mutate(HOUSINGTYPE = if_else(VACANT, "Vacant", "Occupied")) |>
  group_by(HOUSINGTYPE) |>
  summarize(USHH = sum(HHWT), .groups = "drop") |>
  mutate(USHH = str_c(sprintf("%.2f", USHH / sum(USHH) * 100), "%")) |>
  left_join(
    housing_df |>
      filter(STATEFIP == 8) |>
      filter(!(SEASONAL | UNIHABITABLE)) |>
      mutate(HOUSINGTYPE = if_else(VACANT, "Vacant", "Occupied")) |>
      group_by(HOUSINGTYPE) |>
      summarize(COHH = sum(HHWT), .groups = "drop") |>
      mutate(COHH = str_c(sprintf("%.2f", COHH / sum(COHH) * 100), "%")),
    by = "HOUSINGTYPE"
  ) |>
  rename(US = USHH, COLORADO = COHH)

VACANCY_RATE <- .05

CO_HOUSEHOLDS <- housing_df |>
  filter(STATEFIP == 8 & VACANCY == 0) |>
  count(YEAR, wt = HHWT)

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

hh_df |>
  filter(YEAR == 2000, HEADSHIP) |>
  count(AGEGROUP, wt = HHWT) |>
  View()

analysis_year <- 2021
baseline_year <- 2000
minimum_age <- 18
maximum_age <- 45

simple_target_hh_df <- hh_df |>
  filter(
    STATEFIP == 8,
    AGE >= minimum_age,
    AGE < maximum_age,
    YEAR == baseline_year
  ) |>
  group_by(AGEGROUP, CPUMA0010) |>
  summarise(NEWHH = weighted.mean(HEADSHIP, PERWT), .groups = "drop") |>
  right_join(
    hh_df |>
      filter(
        STATEFIP == 8,
        AGE >= minimum_age,
        AGE < maximum_age,
        YEAR == analysis_year
      ),
    by = join_by("AGEGROUP", "CPUMA0010")
  ) |>
  group_by(AGEGROUP, CPUMA0010) |>
  summarise(
    NEWHH = sum(NEWHH * PERWT),
    HH = sum(HEADSHIP * PERWT),
    .groups = "drop"
  ) |>
  mutate(MISSINGHH = NEWHH - HH)

SIMPLE_MISSING_HH <- simple_target_hh_df |>
  filter(MISSINGHH > 0) |>
  pull(MISSINGHH) |>
  sum()

# SIMPLE_UNDERPOD <- (sum(simple_target_hh_df$NEWHH)) /
#   (1 - VACANCY_RATE) -
#   (CO_HOUSING_UNITS[[as.character(analysis_year)]] -
#     CO_SEASONAL_HOUSING[[as.character(analysis_year)]] -
#     CO_UNIHABITABLE_HOUSING[[as.character(analysis_year)]])

SIMPLE_UNDERPROD <- (SIMPLE_MISSING_HH +
  CO_HOUSING_UNITS[[as.character(analysis_year)]]) /
  (1 - VACANCY_RATE) -
  CO_HOUSING_UNITS[[as.character(analysis_year)]] -
  CO_SEASONAL_HOUSING[[as.character(analysis_year)]]

simple_target_hh_df

# hh_df |>
#   filter(YEAR == baseline_year | YEAR == analysis_year) |>
#   filter(RELATE == 1 & STATEFIP == 8) |>
#   mutate(
#     HCOSTQ = cut(
#       HCOSTRATIO,
#       c(0, .35, .60, Inf),
#       labels = c("0-35%", "35-60%", ">60%")
#     )
#   ) |>
#   group_by(YEAR, HCOSTQ) |>
#   summarize(P = sum(HHWT), .groups = "drop_last") |>
#   mutate(P = P / sum(P) * 100) |>
#   ungroup() |>
#   pivot_wider(names_from = YEAR, values_from = P)
# ```

# One way we could build a headship rate counterfactual is by saying would would the headship rate be today if housing costs were more similar to what they were in 2000. This is an improvement in that we can now capture the relationship between housing costs and headship rate, however, it still ignores the relationship between many other variables and headship rates which have changed over time. A way to account for both changing housing cost burden and other variables exists in the 3-way Kitigawa or Oaxaca-Blinder [decomposition](https://giacomovagni.com/blog/2023/oaxaca/) method. This methodology has been used in several other housing studies, such as those done by [Freddie Mac](https://www.freddiemac.com/research/insight/20181205-major-challenge-to-u.s.-housing-supply) and [Brookings](https://www.brookings.edu/articles/make-it-count-measuring-our-housing-supply-shortage/), and allow an analyst to isolate the impact of a single variable on an outcome by shifting that variables value to something it had resembled previously.

# ## The housing burden effect

# $Y$ is the headship status and $X$ is a set of explanatory variables.

# $$
# Y_{\text{baseline}} \sim \text{Binomial}(\theta_{\text{baseline}}) \\
# \text{logit}(\theta_{\text{baseline}}) = \zeta_\text{baseline} X_\text{baseline}^{\text{Housing Burden}} + \mathbf{\beta}_\text{baseline} \mathbf{X}_\text{baseline} \\
# Y_{\text{current}} \sim \text{Binomial}(\theta_{\text{current}}) \\
# \text{logit}(\theta_{\text{current}}) = \zeta_\text{current} X_\text{current}^{\text{Housing Burden}} + \mathbf{\beta}_\text{current} \mathbf{X}_\text{current}
# $$

# ## Housing Cost Counterfactual

# $$
# \text{logit}(\theta_{\text{counterfactual}}) = \zeta_\text{current} X_\text{baseline}^{\text{Housing Burden}} + \mathbf{\beta}_\text{current} \mathbf{X}_\text{current}
# $$

# Note for the counter factual that this change is equivalent to adding the endowment or explained effect of the decomposition for housing cost to $Y_{\text{current}}$. By applying the counterfactual prediction to each observation in our dataset we can then arrive at a new $\text{HR}^*$ which is driven by changing housing cost burden. The table below shows how the model based approach for $\text{HR}^*$ differ from either the observed 2000 or 2023 values.

# ```{r}
# superset_df <- hh_df |>
#   filter(YEAR %in% c(analysis_year, baseline_year)) |>
#   filter(AGE >= minimum_age & AGE <= maximum_age) |>
#   filter(STATEFIP == 8) |>
#   mutate(MARRIED = MARST %in% c(1, 2)) |>
#   mutate(LABFORCE = LABFORCE == 2) |>
#   mutate(HSEDU = EDUC >= 6 & EDUC < 10) |>
#   mutate(COLEDU = EDUC >= 10) |>
#   mutate(TRIGEN = MULTGEN > 2) |>
#   select(
#     YEAR,
#     HEADSHIP,
#     PERWT,
#     AGEGROUP,
#     HOUSINGCOST,
#     NCHILD,
#     MARRIED,
#     LABFORCE,
#     INCTOT,
#     HSEDU,
#     COLEDU,
#     TRIGEN,
#     HCOSTRATIO
#   )

# fm_anlyz_df <- superset_df |>
#   # remove individuals who dont make sense to analysis
#   filter(PERWT != 0, YEAR == analysis_year) |>
#   mutate(HEADSHIP = as.integer(HEADSHIP))

# design_anlyz_df <- svydesign(ids = ~1, weights = ~PERWT, data = fm_anlyz_df)

# cf_houscost_df <- superset_df |>
#   # remove individuals who dont make sense to analysis
#   filter(PERWT != 0, YEAR == baseline_year) |>
#   group_by(AGEGROUP) |>
#   summarize(
#     HCOSTRATIO = weighted.median(HCOSTRATIO, PERWT, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   right_join(
#     superset_df |>
#       filter(YEAR == analysis_year) |>
#       select(-HCOSTRATIO),
#     by = "AGEGROUP"
#   )

# design_anlyz_df <- svydesign(ids = ~1, weights = ~PERWT, data = fm_anlyz_df)

# model_list <- list(
#   age_lm = svyglm(
#     HEADSHIP ~ AGEGROUP,
#     design = design_anlyz_df,
#     family = binomial
#   ),
#   base_lm = svyglm(
#     HEADSHIP ~ log(HCOSTRATIO) * AGEGROUP,
#     design = design_anlyz_df,
#     family = binomial
#   ),
#   famcomp_lm = svyglm(
#     HEADSHIP ~ (log(HCOSTRATIO) + MARRIED + NCHILD + TRIGEN) * AGEGROUP,
#     design = design_anlyz_df,
#     family = binomial
#   ),
#   incedu_lm = svyglm(
#     HEADSHIP ~ (log(HCOSTRATIO) + LABFORCE + HSEDU + COLEDU) * AGEGROUP,
#     design = design_anlyz_df,
#     family = binomial
#   ),
#   full_lm = svyglm(
#     HEADSHIP ~
#       (log(HCOSTRATIO) +
#         MARRIED +
#         NCHILD +
#         TRIGEN +
#         LABFORCE +
#         HSEDU +
#         COLEDU) *
#         AGEGROUP,
#     design = design_anlyz_df,
#     family = binomial
#   )
# )

# model_pred_df <- bind_rows(lapply(names(model_list), function(ln) {
#   cf_houscost_df |>
#     mutate(
#       NEWHR = as.vector(predict(
#         model_list[[ln]],
#         newdata = cf_houscost_df,
#         type = "response"
#       ))
#     ) |>
#     mutate(MODEL = ln)
# }))

# model_pred_df |>
#   group_by(AGEGROUP, MODEL) |>
#   summarize(HR = weighted.mean(NEWHR, PERWT), .groups = "drop") |>
#   pivot_wider(names_from = MODEL, values_from = HR) |>
#   select(AGEGROUP, `Model Based` = full_lm) |>
#   left_join(co_hr_table, by = "AGEGROUP")

# ```

# A noticeable difference between this model based approach and simply using the Headship Rates observed in 2020 is where the difference in observed and target headship rates lay. The model based approach puts more of the missingness of households into younger populations rather than older populations.

# ```{r}

# target_hh_df <- hh_df |>
#   filter(YEAR == analysis_year & PERWT != 0) |>
#   filter(AGE >= 15 & AGE <= maximum_age) |>
#   filter(STATEFIP == 8) |>
#   group_by(AGEGROUP) |>
#   summarize(HH = sum(PERWT * HEADSHIP)) |>
#   left_join(
#     hh_df |>
#       filter(YEAR == analysis_year & PERWT != 0) |>
#       filter(AGE < minimum_age & AGE >= 15) |>
#       filter(STATEFIP == 8) |>
#       mutate(NEWHR = .04) |>
#       group_by(AGEGROUP) |>
#       summarize(NEWHH = sum(PERWT * NEWHR)) |>
#       bind_rows(
#         model_pred_df |>
#           filter(MODEL == "full_lm") |>
#           group_by(AGEGROUP) |>
#           summarize(NEWHH = sum(PERWT * NEWHR))
#       ),
#     by = join_by(AGEGROUP)
#   ) |>
#   mutate(MISSINGHH = NEWHH - HH)

# target_hh_df
# ```

# ```{r}
# CO_MISSING_HH_DECOMP <- sum(target_hh_df$MISSINGHH)

# CO_UNDERPOD_DECOMP <- (sum(target_hh_df$MISSINGHH) + CO_HOUSEHOLDS) /
#   (1 - VACANCY_RATE) -
#   (CO_HOUSING_UNITS - CO_SEASONAL_HOUSING - CO_UNIHABITABLE_HOUSING)
# ```

# From these calculations we find that Colorado has `r prettyNum(round(CO_MISSING_HH ), big.mark = ",")` missing households which leads to a housing underproduction count of `r prettyNum(round(CO_UNDERPOD), big.mark = ",")` units.

# To demonstrate in another way what this model is doing and show its flexibility lets pick another value for housing burden to see its impact on the housing shortage estimate. Let us say instead that we think all individuals should be paying 100% of their income on housing costs. We can place this into the model as follows.

# ```{r}
# cf_all_income <- hh_df |>
#   filter(YEAR == analysis_year & PERWT != 0) |>
#   filter(AGE >= 15 & AGE <= maximum_age) |>
#   filter(STATEFIP == 8) |>
#   group_by(AGEGROUP) |>
#   summarize(HH = sum(PERWT * HEADSHIP)) |>
#   right_join(
#     superset_df |>
#       filter(YEAR == analysis_year) |>
#       mutate(HCOSTRATIO = 1) %>%
#       mutate(
#         NEWHR = predict(
#           model_list[["full_lm"]],
#           newdata = .,
#           type = "response"
#         )
#       ) |>
#       group_by(AGEGROUP) |>
#       summarize(NEWHH = sum(PERWT * NEWHR)),
#     by = join_by(AGEGROUP)
#   ) |>
#   mutate(MISSINGHH = NEWHH - HH) |>
#   pull(MISSINGHH) |>
#   sum()
# ```

# When we do this our model tells us that we actually have to much housing! That is if you want the citizens of Colorado to spend more of their income on housing we need to get rid of `r prettyNum(round(-cf_all_income), big.mark = ",")` units.

# Alternatively, what if we wanted a scenario where individuals only spent 10% of their income on housing, what then would the housing shortage be?

# ```{r}
# cf_ten_income <- hh_df |>
#   filter(YEAR == analysis_year & PERWT != 0) |>
#   filter(AGE >= 15 & AGE <= maximum_age) |>
#   filter(STATEFIP == 8) |>
#   group_by(AGEGROUP) |>
#   summarize(HH = sum(PERWT * HEADSHIP)) |>
#   right_join(
#     superset_df |>
#       filter(YEAR == analysis_year) |>
#       mutate(HCOSTRATIO = .1) |>
#       mutate(
#         NEWHR = predict(
#           model_list[["full_lm"]],
#           newdata = .,
#           type = "response"
#         )
#       ) |>
#       group_by(AGEGROUP) |>
#       summarize(NEWHH = sum(PERWT * NEWHR)),
#     by = join_by(AGEGROUP)
#   ) |>
#   mutate(MISSINGHH = NEWHH - HH) |>
#   pull(MISSINGHH) |>
#   sum()
# ```

# In this case the model says we would need to add `r prettyNum(round(cf_ten_income), big.mark = ",")` units to Colorado's housing total.
