# load packages ----
library(tidyverse)
library(here)
library(countrycode)
library(states)

# load data ----
## ptas panel
load(here("data/ch1/preprocessed/ptas_panel.rda"))

## gdp_dat
gdp_dat <- read_rds(
  here(
    "data/common/raw/fariss/estimates_gdp_model_combined_normal_noslope_gamma_lambda_additive_test_20240416.rds"
    )
  )

## gdppc_dat
gdppc_dat <- read_rds(
  here(
    "data/common/raw/fariss/estimates_gdppc_model_combined_normal_noslope_gamma_lambda_additive_test_20240416.rds"
    )
  )

## pop_dat
pop_dat <- read_rds(
  here(
    "data/common/raw/fariss/estimates_pop_model_combined_normal_noslope_gamma_lambda_additive_test_20240416.rds"
    )
  )

# prep gdp, gdppc, & pop vars ----
## filter latent estimates & recent obs
### gdp
gdp_dat <- gdp_dat |> 
  filter(indicator == "latent_gdp" & year > 1945)

### gdppc
gdppc_dat <- gdppc_dat |> 
  filter(indicator == "latent_gdppc" & year > 1945)

### pop
pop_dat <- pop_dat |> 
  filter(indicator == "latent_pop" & year > 1945)

## convert gwno codes to cow codes (use `states` package to get & compare the code sets; some codes will need to be converted manually)

### gdp
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

### gdppc
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

### pop
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

## shrink
gdp_small <- gdp_dat |> 
  select(cow, year, mean_log10) |> 
  rename(gdp_log10 = mean_log10)

gdppc_small <- gdppc_dat |> 
  select(cow, year, mean_log10) |> 
  rename(gdppc_log10 = mean_log10)

pop_small <- pop_dat |> 
  select(cow, year, mean_log10) |> 
  rename(pop_log10 = mean_log10)

## merge
standards <- gdp_small |> 
  left_join(gdppc_small) |> 
  left_join(pop_small)

## change cow codes for czechia & yemen (will prevent most important cases of missingness [where inforce == 1] later on & prep alignment w/ HR scores)
standards <- standards |> 
  mutate(
    cow = case_when(
      cow == 315 ~ 316,
      cow == 678 ~ 679,
      .default = cow
      )
    )

### save standards
standards |> 
  save(
    file = here("data/ch1/preprocessed/standards.rda")
    )

# standardize Lechner vars ----
## merge to get gdp & gdppc of partner countries
ptas_panel <- ptas_panel |> 
  left_join(
    standards,
    by = c(
      "cow_partner" = "cow", "year" = "year"
      )
    )

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
      c(9:11, 16:21),
      ~ case_when(
        .x == 0 & inforce == 1 ~ 0,
        .x == 0 & inforce == 0 ~ NA,
        .default = .x
        )
      )
    )

## standardize
ptas_standard <- ptas_panel |> 
  summarize(
    cpr_mean = mean(cpr_all_lta, na.rm = TRUE),
    cpr_gdp_mean = mean(cpr_gdp, na.rm = TRUE),
    cpr_gdppc_mean = mean(cpr_gdppc, na.rm = TRUE),
    cpr_pop_mean = mean(cpr_pop, na.rm = TRUE),
    esr_mean = mean(esr_all_lta, na.rm = TRUE),
    esr_gdp_mean = mean(esr_gdp, na.rm = TRUE),
    esr_gdppc_mean = mean(esr_gdppc, na.rm = TRUE),
    esr_pop_mean = mean(esr_pop, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      3:10,
      ~ if_else(
        is.nan(.x), NA, .x
        )
      ),
    inforce = if_else(
      is.na(cpr_mean) | is.na(esr_mean), 0, 1
      )
    ) |> 
  arrange(cow, year) |> 
  relocate(inforce, .after = year)

# save ----
ptas_standard |> 
  save(
    file = here("data/ch1/preprocessed/ptas_standard.rda")
    )
