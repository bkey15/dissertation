# load packages ----
library(tidyverse)
library(here)
library(countrycode)
library(states)
library(naniar)

# load data ----
## ptas panel
load(here("data/ch1/preprocessed/ptas_panel.rda"))

## gdp_dat
### using 2022 Fariss et al. data b/c 2024 data is hard to interpret (almost double what it should be in most cases); is important as standard for bop % calculations
gdp_dat <- read_rds(
  here(
    "data/common/raw/fariss/estimates_gdp_model_combined_normal_noslope_gamma_lambda_additive_test_20220215.rds"
    )
  )

## gdppc_dat
gdppc_dat <- read_rds(
  here(
    "data/common/raw/fariss/estimates_gdppc_model_combined_normal_noslope_gamma_lambda_additive_test_20220215.rds"
    )
  )

## pop_dat
pop_dat <- read_rds(
  here(
    "data/common/raw/fariss/estimates_pop_model_combined_normal_noslope_gamma_lambda_additive_test_20210827.rds"
    )
  )

## world bank income groups
wb_grp <- read_csv(here("data/common/raw/wb/world-bank-income-groups.csv")) |> 
  janitor::clean_names()

# prep standard vars ----
## gdp, gdpppc, pop----
### filter latent estimates & recent obs
#### gdp
gdp_dat <- gdp_dat |> 
  filter(indicator == "latent_gdp" & year > 1945)

#### gdppc
gdppc_dat <- gdppc_dat |> 
  filter(indicator == "latent_gdppc" & year > 1945)

#### pop
pop_dat <- pop_dat |> 
  filter(indicator == "latent_pop" & year > 1945)

### convert gwno codes to cow codes (use `states` package to get & compare the code sets; some codes will need to be converted manually)

#### gdp
gdp_dat <- gdp_dat |> 
  mutate(
    cow = countrycode(
      sourcevar = gwno,
      origin = "gwn",
      destination = "cown"
      ),
    cow = case_when(
      gwno == 54 | gwno == 55 | gwno == 56 | gwno == 57 | gwno == 58 | gwno == 60 | gwno == 221 | gwno == 223 | gwno == 232 | gwno == 331 | gwno == 403 | gwno == 591 | gwno == 816 | gwno == 935 | gwno == 983 | gwno == 986 | gwno == 987 | gwno == 990 ~ gwno,
      gwno == 340 & year > 2006 ~ 345,
      gwno == 970 ~ 946,
      gwno == 971 ~ 970,
      gwno == 972 ~ 955,
      gwno == 973 ~ 947,
      .default = cow
      )
    ) |> 
  relocate(cow, .after = gwno) |> 
  filter(
    !is.na(cow),
    gwno != 817
    ) |> 
  arrange(cow, year)

#### gdppc
gdppc_dat <- gdppc_dat |> 
  mutate(
    cow = countrycode(
      sourcevar = gwno,
      origin = "gwn",
      destination = "cown"
      ),
    cow = case_when(
      gwno == 54 | gwno == 55 | gwno == 56 | gwno == 57 | gwno == 58 | gwno == 60 | gwno == 221 | gwno == 223 | gwno == 232 | gwno == 331 | gwno == 403 | gwno == 591 | gwno == 816 | gwno == 935 | gwno == 983 | gwno == 986 | gwno == 987 | gwno == 990 ~ gwno,
      gwno == 340 & year > 2006 ~ 345,
      gwno == 970 ~ 946,
      gwno == 971 ~ 970,
      gwno == 972 ~ 955,
      gwno == 973 ~ 947,
      .default = cow
      )
    ) |> 
  relocate(cow, .after = gwno) |> 
  filter(
    !is.na(cow),
    gwno != 817
    )

#### pop
pop_dat <- pop_dat |> 
  mutate(
    cow = countrycode(
      sourcevar = gwno,
      origin = "gwn",
      destination = "cown"
      ),
    cow = case_when(
      gwno == 54 | gwno == 55 | gwno == 56 | gwno == 57 | gwno == 58 | gwno == 60 | gwno == 221 | gwno == 223 | gwno == 232 | gwno == 331 | gwno == 403 | gwno == 591 | gwno == 816 | gwno == 935 | gwno == 983 | gwno == 986 | gwno == 987 | gwno == 990 ~ gwno,
      gwno == 340 & year > 2006 ~ 345,
      gwno == 970 ~ 946,
      gwno == 971 ~ 970,
      gwno == 972 ~ 955,
      gwno == 973 ~ 947,
      .default = cow
      )
    ) |> 
  relocate(cow, .after = gwno) |> 
  filter(
    !is.na(cow),
    gwno != 817
    ) |> 
  arrange(cow, year)

### shrink
gdp_small <- gdp_dat |> 
  select(cow, year, mean, mean_log10) |> 
  rename(
    gdp_mean = mean,
    gdp_log10 = mean_log10
    )

gdppc_small <- gdppc_dat |> 
  select(cow, year, mean, mean_log10) |> 
  rename(
    gdppc_mean = mean,
    gdppc_log10 = mean_log10
    )

pop_small <- pop_dat |> 
  select(cow, year, mean, mean_log10) |> 
  rename(
    pop_mean = mean,
    pop_log10 = mean_log10
    )

### merge
standards <- gdp_small |> 
  left_join(gdppc_small) |> 
  left_join(pop_small)

## glb_s ----
## "Global South" indicator (generally following procedure set forth in Bodea & Ye, 2018, p. 964, footnote 68)
### get cow codes
wb_grp <- wb_grp |> 
  mutate(
    cow = countrycode(
      sourcevar = code,
      origin = "wb",
      destination = "cown"
      ),
    cow = case_when(
      code == "OWID_CZS" ~ 316,
      code == "OWID_KOS" ~ 347,
      code == "OWID_SRM" ~ 345,
      code == "OWID_USS" ~ 365,
      code == "OWID_YGS" ~ 345,
      code == "SRB" ~ 345,
      .default = cow
      )
    ) |> 
  select(cow, year, classification) |> 
  filter(!is.na(cow)) |> 
  arrange(cow)

### expand
years_hrs <- tibble(year = 1946:2021)

wb_small <- wb_grp |> 
  full_join(years_hrs)
wb_small <- wb_small |> 
  expand(cow, year)

wb_grp <- wb_small |> 
  left_join(wb_grp)

### fill missing data w/ obs most recent to them
wb_grp <- wb_grp |> 
  group_by(cow) |> 
  fill(classification, .direction = "up") |> 
  ungroup()

### filter to date range
### (for now, 1977-2021; most ptas signed more recently)
wb_grp <- wb_grp |> 
  filter(year > 1976 & year < 2022)

### count number of years each country is "high income"
grp_n <- wb_grp |> 
  count(cow, classification)

### filter to non-high-income countries
grp_n <- grp_n |> 
  filter(classification != "High-income countries")

### create indicator
glb_s <- grp_n |> 
  summarize(
    sum = sum(n),
    .by = cow
    ) |> 
  mutate(
    glb_s = if_else(sum > 22, 1, 0)
    ) |> 
  select(cow, glb_s)

### reintroduce high-income countries; set to 0
all_cow <- unique(wb_grp$cow) |> 
  as_tibble_col(column_name = "cow") |> 
  filter(!is.na(cow))

glb_s <- glb_s |> 
  right_join(all_cow) |> 
  mutate(glb_s = impute_zero(glb_s)) |> 
  arrange(cow)

## merge ----
standards <- standards |> 
  left_join(glb_s) |> 
  relocate(glb_s, .after = year)

## miss fix ----
### change cow codes for czechia & yemen (will prevent most important cases of missingness [where inforce == 1] later on & prep alignment w/ HR scores)
standards <- standards |> 
  mutate(
    cow = case_when(
      cow == 315 ~ 316,
      cow == 678 ~ 679,
      .default = cow
      )
    )

### fix missingness in glb_s (for countries appearing in ptas_panel)
standards <- standards |> 
  mutate(
    glb_s = case_when(
      cow == 316 ~ 1,
      cow == 679 ~ 1,
      .default = glb_s
      )
    )

# standardize Lechner vars ----
## merge to get gdp & gdppc of partner countries
ptas_panel <- ptas_panel |> 
  left_join(
    standards,
    by = c(
      "cow_partner" = "cow",
      "year" = "year"
      )
    ) |> 
  rename(glb_s_partner = glb_s) |> 
  relocate(glb_s_partner, .after = iso_partner)

ptas_panel <- standards |> 
  select(cow, year, glb_s) |> 
  right_join(ptas_panel) |> 
  relocate(glb_s, .after = iso) |> 
  arrange(cow, cow_partner, base_treaty)

## interact
ptas_panel <- ptas_panel |> 
  mutate(
    cpr_gdp = cpr_all_lta*gdp_log10,
    cpr_gdppc = cpr_all_lta*gdppc_log10,
    cpr_pop = cpr_all_lta*pop_log10,
    esr_gdp = esr_all_lta*gdp_log10,
    esr_gdppc = esr_all_lta*gdppc_log10,
    esr_pop = esr_all_lta*pop_log10
    )

## keep 0 vals where pta is in force; otherwise force to NA (the latter shouldn't be used in computing the legalization means)
ptas_panel <- ptas_panel |> 
  mutate(
    across(
      c(11:13, 21:26),
      ~ case_when(
        .x == 0 & inforce == 1 ~ 0,
        .x == 0 & inforce == 0 ~ NA,
        .default = .x
        )
      )
    )

## prep n_ptas var (to be used for spatial regressions)
ptas_panel <- ptas_panel |> 
  mutate(
    base_treaty_inforce = if_else(
      base_treaty*inforce == 0, NA, base_treaty*inforce
      )
    )

## standardize
### general ----
### note: when computing any_inforce, know there are no obs. where is.na(cpr_mean) & !is.na(esr_mean) (or vice versa)
ptas_gen <- ptas_panel |> 
  summarize(
    cpr_mean = mean(cpr_all_lta, na.rm = TRUE),
    cpr_gdp_mean = mean(cpr_gdp, na.rm = TRUE),
    cpr_gdppc_mean = mean(cpr_gdppc, na.rm = TRUE),
    cpr_pop_mean = mean(cpr_pop, na.rm = TRUE),
    esr_mean = mean(esr_all_lta, na.rm = TRUE),
    esr_gdp_mean = mean(esr_gdp, na.rm = TRUE),
    esr_gdppc_mean = mean(esr_gdppc, na.rm = TRUE),
    esr_pop_mean = mean(esr_pop, na.rm = TRUE),
    n_ptas = n_distinct(base_treaty_inforce, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      ),
    any_inforce = if_else(
      is.na(cpr_mean) & is.na(esr_mean), 0, 1
      )
    ) |> 
  arrange(cow, year) |> 
  relocate(any_inforce, n_ptas, .after = year)

ptas_gen <- ptas_gen |> 
  mutate(
    lech_hr_mean = (cpr_mean + esr_mean)/2,
    lech_hr_gdp_mean = (cpr_gdp_mean + esr_gdp_mean)/2,
    lech_hr_gdppc_mean = (cpr_gdppc_mean + esr_gdppc_mean)/2,
    lech_hr_pop_mean = (cpr_pop_mean + esr_pop_mean)/2
    )

### south-south ----
ptas_ss <- ptas_panel |> 
  filter(glb_s == 1 & glb_s_partner == 1) |> 
  summarize(
    ss_cpr_mean = mean(cpr_all_lta, na.rm = TRUE),
    ss_cpr_gdp_mean = mean(cpr_gdp, na.rm = TRUE),
    ss_cpr_gdppc_mean = mean(cpr_gdppc, na.rm = TRUE),
    ss_cpr_pop_mean = mean(cpr_pop, na.rm = TRUE),
    ss_esr_mean = mean(esr_all_lta, na.rm = TRUE),
    ss_esr_gdp_mean = mean(esr_gdp, na.rm = TRUE),
    ss_esr_gdppc_mean = mean(esr_gdppc, na.rm = TRUE),
    ss_esr_pop_mean = mean(esr_pop, na.rm = TRUE),
    ss_n_ptas = n_distinct(base_treaty_inforce, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      ),
    ss_any_inforce = if_else(
      is.na(ss_cpr_mean) & is.na(ss_esr_mean), 0, 1
      )
    ) |> 
  arrange(cow, year) |> 
  relocate(ss_any_inforce, .after = year)

ptas_ss <- ptas_ss |> 
  mutate(
    ss_lech_hr_mean = (ss_cpr_mean + ss_esr_mean)/2,
    ss_lech_hr_gdp_mean = (ss_cpr_gdp_mean + ss_esr_gdp_mean)/2,
    ss_lech_hr_gdppc_mean = (ss_cpr_gdppc_mean + ss_esr_gdppc_mean)/2,
    ss_lech_hr_pop_mean = (ss_cpr_pop_mean + ss_esr_pop_mean)/2
    )

### north-south ----
ptas_ns <- ptas_panel |> 
  filter((glb_s == 1 & glb_s_partner == 0) | (glb_s == 0 & glb_s_partner == 1)) |> 
  summarize(
    ns_cpr_mean = mean(cpr_all_lta, na.rm = TRUE),
    ns_cpr_gdp_mean = mean(cpr_gdp, na.rm = TRUE),
    ns_cpr_gdppc_mean = mean(cpr_gdppc, na.rm = TRUE),
    ns_cpr_pop_mean = mean(cpr_pop, na.rm = TRUE),
    ns_esr_mean = mean(esr_all_lta, na.rm = TRUE),
    ns_esr_gdp_mean = mean(esr_gdp, na.rm = TRUE),
    ns_esr_gdppc_mean = mean(esr_gdppc, na.rm = TRUE),
    ns_esr_pop_mean = mean(esr_pop, na.rm = TRUE),
    ns_n_ptas = n_distinct(base_treaty_inforce, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      ),
    ns_any_inforce = if_else(
      is.na(ns_cpr_mean) & is.na(ns_esr_mean), 0, 1
      )
    ) |> 
  arrange(cow, year) |> 
  relocate(ns_any_inforce, .after = year)

ptas_ns <- ptas_ns |> 
  mutate(
    ns_lech_hr_mean = (ns_cpr_mean + ns_esr_mean)/2,
    ns_lech_hr_gdp_mean = (ns_cpr_gdp_mean + ns_esr_gdp_mean)/2,
    ns_lech_hr_gdppc_mean = (ns_cpr_gdppc_mean + ns_esr_gdppc_mean)/2,
    ns_lech_hr_pop_mean = (ns_cpr_pop_mean + ns_esr_pop_mean)/2
    )

### north-north ----
ptas_nn <- ptas_panel |> 
  filter(glb_s == 0 & glb_s_partner == 0) |> 
  summarize(
    nn_cpr_mean = mean(cpr_all_lta, na.rm = TRUE),
    nn_cpr_gdp_mean = mean(cpr_gdp, na.rm = TRUE),
    nn_cpr_gdppc_mean = mean(cpr_gdppc, na.rm = TRUE),
    nn_cpr_pop_mean = mean(cpr_pop, na.rm = TRUE),
    nn_esr_mean = mean(esr_all_lta, na.rm = TRUE),
    nn_esr_gdp_mean = mean(esr_gdp, na.rm = TRUE),
    nn_esr_gdppc_mean = mean(esr_gdppc, na.rm = TRUE),
    nn_esr_pop_mean = mean(esr_pop, na.rm = TRUE),
    nn_n_ptas = n_distinct(base_treaty_inforce, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      ),
    nn_any_inforce = if_else(
      is.na(nn_cpr_mean) & is.na(nn_esr_mean), 0, 1
      )
    ) |> 
  arrange(cow, year) |> 
  relocate(nn_any_inforce, .after = year)

ptas_nn <- ptas_nn |> 
  mutate(
    nn_lech_hr_mean = (nn_cpr_mean + nn_esr_mean)/2,
    nn_lech_hr_gdp_mean = (nn_cpr_gdp_mean + nn_esr_gdp_mean)/2,
    nn_lech_hr_gdppc_mean = (nn_cpr_gdppc_mean + nn_esr_gdppc_mean)/2,
    nn_lech_hr_pop_mean = (nn_cpr_pop_mean + nn_esr_pop_mean)/2
    )

### merge ----
ptas_standard <- ptas_gen |> 
  left_join(ptas_ss) |> 
  left_join(ptas_ns) |> 
  left_join(ptas_nn)

# save ----
standards |> 
  save(file = here("data/ch1/preprocessed/standards.rda"))

ptas_standard |> 
  save(file = here("data/ch1/preprocessed/ptas_standard.rda"))
