# Replicate Hafner-Burton (2005) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals (& lagged covars).

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_2_l1.rda"))
load(here("data/ch1/results/imputations/imp_3_l1.rda"))

# start 1968 ----
## cpr ----
### mean ----
fit_cpr1_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr1_1968 <- pool(fit_cpr1_1968)
sum_cpr1_1968 <- summary(pool_cpr1_1968)

### gdp mean ----
fit_cpr2_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr2_1968 <- pool(fit_cpr2_1968)
sum_cpr2_1968 <- summary(pool_cpr2_1968)

### gdppc mean ----
fit_cpr3_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr3_1968 <- pool(fit_cpr3_1968)
sum_cpr3_1968 <- summary(pool_cpr3_1968)

### combine ----
cpr_1968 <- list(
  mean = sum_cpr1_1968,
  gdp = sum_cpr2_1968,
  gdppc = sum_cpr3_1968
  )

## esr ----
### mean ----
fit_esr1_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr1_1968 <- pool(fit_esr1_1968)
sum_esr1_1968 <- summary(pool_esr1_1968)

### gdp mean ----
fit_esr2_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr2_1968 <- pool(fit_esr2_1968)
sum_esr2_1968 <- summary(pool_esr2_1968)

### gdppc mean ----
fit_esr3_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr3_1968 <- pool(fit_esr3_1968)
sum_esr3_1968 <- summary(pool_esr3_1968)

### combine ----
esr_1968 <- list(
  mean = sum_esr1_1968,
  gdp = sum_esr2_1968,
  gdppc = sum_esr3_1968
  )

## both ----
### mean ----
fit_both1_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both1_1968 <- pool(fit_both1_1968)
sum_both1_1968 <- summary(pool_both1_1968)

### gdp mean ----
fit_both2_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both2_1968 <- pool(fit_both2_1968)
sum_both2_1968 <- summary(pool_both2_1968)

### gdppc mean ----
fit_both3_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both3_1968 <- pool(fit_both3_1968)
sum_both3_1968 <- summary(pool_both3_1968)

### combine ----
both_1968 <- list(
  mean = sum_both1_1968,
  gdp = sum_both2_1968,
  gdppc = sum_both3_1968
  )

## lech ----
### mean ----
fit_lech1_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ lech_hr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_lech1_1968 <- pool(fit_lech1_1968)
sum_lech1_1968 <- summary(pool_lech1_1968)

### gdp mean ----
fit_lech2_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ lech_hr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_lech2_1968 <- pool(fit_lech2_1968)
sum_lech2_1968 <- summary(pool_lech2_1968)

### gdppc mean ----
fit_lech3_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ lech_hr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_lech3_1968 <- pool(fit_lech3_1968)
sum_lech3_1968 <- summary(pool_lech3_1968)

### combine ----
lech_1968 <- list(
  mean = sum_lech1_1968,
  gdp = sum_lech2_1968,
  gdppc = sum_lech3_1968
  )

## all combine ----
imp_2_hb_res_gen <- list(
  cpr = cpr_1968,
  esr = esr_1968,
  both = both_1968,
  lech = lech_1968
  )

# start 1977 ----
## cpr ----
### mean ----
fit_cpr1_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr1_1977 <- pool(fit_cpr1_1977)
sum_cpr1_1977 <- summary(pool_cpr1_1977)

### gdp mean ----
fit_cpr2_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr2_1977 <- pool(fit_cpr2_1977)
sum_cpr2_1977 <- summary(pool_cpr2_1977)

### gdppc mean ----
fit_cpr3_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr3_1977 <- pool(fit_cpr3_1977)
sum_cpr3_1977 <- summary(pool_cpr3_1977)

### combine ----
cpr_1977 <- list(
  mean = sum_cpr1_1977,
  gdp = sum_cpr2_1977,
  gdppc = sum_cpr3_1977
  )

## esr ----
### mean ----
fit_esr1_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr1_1977 <- pool(fit_esr1_1977)
sum_esr1_1977 <- summary(pool_esr1_1977)

### gdp mean ----
fit_esr2_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr2_1977 <- pool(fit_esr2_1977)
sum_esr2_1977 <- summary(pool_esr2_1977)

### gdppc mean ----
fit_esr3_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr3_1977 <- pool(fit_esr3_1977)
sum_esr3_1977 <- summary(pool_esr3_1977)

### combine ----
esr_1977 <- list(
  mean = sum_esr1_1977,
  gdp = sum_esr2_1977,
  gdppc = sum_esr3_1977
  )

## both ----
### mean ----
fit_both1_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both1_1977 <- pool(fit_both1_1977)
sum_both1_1977 <- summary(pool_both1_1977)

### gdp mean ----
fit_both2_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both2_1977 <- pool(fit_both2_1977)
sum_both2_1977 <- summary(pool_both2_1977)

### gdppc mean ----
fit_both3_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both3_1977 <- pool(fit_both3_1977)
sum_both3_1977 <- summary(pool_both3_1977)

### combine ----
both_1977 <- list(
  mean = sum_both1_1977,
  gdp = sum_both2_1977,
  gdppc = sum_both3_1977
  )

## lech ----
### mean ----
fit_lech1_1977 <- with(
  imp_2_l1,
  lm(hr_score ~ lech_hr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
  )

pool_lech1_1977 <- pool(fit_lech1_1977)
sum_lech1_1977 <- summary(pool_lech1_1977)

### gdp mean ----
fit_lech2_1977 <- with(
  imp_2_l1,
  lm(hr_score ~ lech_hr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_lech2_1977 <- pool(fit_lech2_1977)
sum_lech2_1977 <- summary(pool_lech2_1977)

### gdppc mean ----
fit_lech3_1977 <- with(
  imp_2_l1,
  lm(hr_score ~ lech_hr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_lech3_1977 <- pool(fit_lech3_1977)
sum_lech3_1977 <- summary(pool_lech3_1977)

### combine ----
lech_1977 <- list(
  mean = sum_lech1_1977,
  gdp = sum_lech2_1977,
  gdppc = sum_lech3_1977
  )

## all combine ----
imp_3_hb_res_gen <- list(
  cpr = cpr_1977,
  esr = esr_1977,
  both = both_1977,
  lech = lech_1977
  )

# save ----
imp_2_hb_res_gen |> 
  save(file = here("data/ch1/results/fits/hb_rep/mice/start_1968/imp_2_hb_res_gen.rda"))
imp_3_hb_res_gen |> 
  save(file = here("data/ch1/results/fits/hb_rep/mice/start_1977/imp_3_hb_res_gen.rda"))
