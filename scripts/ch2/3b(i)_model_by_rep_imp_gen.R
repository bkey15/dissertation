# Replicate Bodea-Ye (2018) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals (& lagged covars).

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch2/results/imputations/imp_1962_l1.rda"))
load(here("data/ch2/results/imputations/imp_1981_l1.rda"))
load(here("data/ch2/results/imputations/imp_1990_l1.rda"))

## note: B-Y limit cases to developing countries, but here, we're using all cases. The closest B-Y "rep" appears in the "imp_south" script.

# no interaction terms ----
## start 1962 ----
### n_bits ----
fit_noin_1_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ n_bits + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_1_1962 <- pool(fit_noin_1_1962)
sum_noin_1_1962 <- summary(pool_noin_1_1962)

### mean_gdp ----
fit_noin_2_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ partner_mean_gdp + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_2_1962 <- pool(fit_noin_2_1962)
sum_noin_2_1962 <- summary(pool_noin_2_1962)

### mean_gdppc ----
fit_noin_3_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ partner_mean_gdppc + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_3_1962 <- pool(fit_noin_3_1962)
sum_noin_3_1962 <- summary(pool_noin_3_1962)

### mean_pop ----
fit_noin_4_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ partner_mean_pop + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
)

pool_noin_4_1962 <- pool(fit_noin_4_1962)
sum_noin_4_1962 <- summary(pool_noin_4_1962)

### combine ----
by_res_noin_1962 <- list(
  n_bits = sum_noin_1_1962,
  gdp = sum_noin_2_1962,
  gdppc = sum_noin_3_1962,
  pop = sum_noin_4_1962
  )

## start 1981 ----
### n_bits ----
fit_noin_1_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ n_bits + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
)

pool_noin_1_1981 <- pool(fit_noin_1_1981)
sum_noin_1_1981 <- summary(pool_noin_1_1981)

### mean_gdp ----
fit_noin_2_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ partner_mean_gdp + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_2_1981 <- pool(fit_noin_2_1981)
sum_noin_2_1981 <- summary(pool_noin_2_1981)

### mean_gdppc ----
fit_noin_3_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ partner_mean_gdppc + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_3_1981 <- pool(fit_noin_3_1981)
sum_noin_3_1981 <- summary(pool_noin_3_1981)

### mean_pop ----
fit_noin_4_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ partner_mean_pop + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_4_1981 <- pool(fit_noin_4_1981)
sum_noin_4_1981 <- summary(pool_noin_4_1981)

### combine ----
by_res_noin_1981 <- list(
  n_bits = sum_noin_1_1981,
  gdp = sum_noin_2_1981,
  gdppc = sum_noin_3_1981,
  pop = sum_noin_4_1981
  )

## start 1990 ----
### n_bits ----
fit_noin_1_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ n_bits + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_1_1990 <- pool(fit_noin_1_1990)
sum_noin_1_1990 <- summary(pool_noin_1_1990)

### mean_gdp ----
fit_noin_2_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ partner_mean_gdp + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_2_1990 <- pool(fit_noin_2_1990)
sum_noin_2_1990 <- summary(pool_noin_2_1990)

### mean_gdppc ----
fit_noin_3_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ partner_mean_gdppc + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_3_1990 <- pool(fit_noin_3_1990)
sum_noin_3_1990 <- summary(pool_noin_3_1990)

### mean_pop ----
fit_noin_4_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ partner_mean_pop + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_noin_4_1990 <- pool(fit_noin_4_1990)
sum_noin_4_1990 <- summary(pool_noin_4_1990)

### combine ----
by_res_noin_1990 <- list(
  n_bits = sum_noin_1_1990,
  gdp = sum_noin_2_1990,
  gdppc = sum_noin_3_1990,
  pop = sum_noin_4_1990
  )

## all combine ----
no_interact <- list(
  start_1962 = by_res_noin_1962,
  start_1981 = by_res_noin_1981,
  start_1990 = by_res_noin_1990
  )

# has interaction terms ----
## start 1962 ----
### n_bits ----
fit_int_1_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ n_bits + e_polity2 + n_bits*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_1_1962 <- pool(fit_int_1_1962)
sum_int_1_1962 <- summary(pool_int_1_1962)

### mean_gdp ----
fit_int_2_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ partner_mean_gdp + e_polity2 + partner_mean_gdp*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_2_1962 <- pool(fit_int_2_1962)
sum_int_2_1962 <- summary(pool_int_2_1962)

### mean_gdppc ----
fit_int_3_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ partner_mean_gdppc + e_polity2 + partner_mean_gdppc*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_3_1962 <- pool(fit_int_3_1962)
sum_int_3_1962 <- summary(pool_int_3_1962)

### mean_pop ----
fit_int_4_1962 <- with(
  imp_1962_l1,
  lm(hr_score ~ partner_mean_pop + e_polity2 + partner_mean_pop*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_4_1962 <- pool(fit_int_4_1962)
sum_int_4_1962 <- summary(pool_int_4_1962)

### combine ----
by_res_int_1962 <- list(
  n_bits = sum_int_1_1962,
  gdp = sum_int_2_1962,
  gdppc = sum_int_3_1962,
  pop = sum_int_4_1962
  )

## start 1981 ----
### n_bits ----
fit_int_1_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ n_bits + e_polity2 + n_bits*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_1_1981 <- pool(fit_int_1_1981)
sum_int_1_1981 <- summary(pool_int_1_1981)

### mean_gdp ----
fit_int_2_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ partner_mean_gdp + e_polity2 + partner_mean_gdp*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_2_1981 <- pool(fit_int_2_1981)
sum_int_2_1981 <- summary(pool_int_2_1981)

### mean_gdppc ----
fit_int_3_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ partner_mean_gdppc + e_polity2 + partner_mean_gdppc*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_3_1981 <- pool(fit_int_3_1981)
sum_int_3_1981 <- summary(pool_int_3_1981)

### mean_pop ----
fit_int_4_1981 <- with(
  imp_1981_l1,
  lm(hr_score ~ partner_mean_pop + e_polity2 + partner_mean_pop*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_4_1981 <- pool(fit_int_4_1981)
sum_int_4_1981 <- summary(pool_int_4_1981)

### combine ----
by_res_int_1981 <- list(
  n_bits = sum_int_1_1981,
  gdp = sum_int_2_1981,
  gdppc = sum_int_3_1981,
  pop = sum_int_4_1981
  )

## start 1990 ----
### n_bits ----
fit_int_1_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ n_bits + e_polity2 + n_bits*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_1_1990 <- pool(fit_int_1_1990)
sum_int_1_1990 <- summary(pool_int_1_1990)

### mean_gdp ----
fit_int_2_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ partner_mean_gdp + e_polity2 + partner_mean_gdp*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_2_1990 <- pool(fit_int_2_1990)
sum_int_2_1990 <- summary(pool_int_2_1990)

### mean_gdppc ----
fit_int_3_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ partner_mean_gdppc + e_polity2 + partner_mean_gdppc*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_3_1990 <- pool(fit_int_3_1990)
sum_int_3_1990 <- summary(pool_int_3_1990)

### mean_pop ----
fit_int_4_1990 <- with(
  imp_1990_l1,
  lm(hr_score ~ partner_mean_pop + e_polity2 + partner_mean_pop*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year))
  )

pool_int_4_1990 <- pool(fit_int_4_1990)
sum_int_4_1990 <- summary(pool_int_4_1990)

### combine ----
by_res_int_1990 <- list(
  n_bits = sum_int_1_1990,
  gdp = sum_int_2_1990,
  gdppc = sum_int_3_1990,
  pop = sum_int_4_1990
  )

## all combine ----
has_interact <- list(
  start_1962 = by_res_int_1962,
  start_1981 = by_res_int_1981,
  start_1990 = by_res_int_1990
  )

# final combine ----
imp_by_rep_res_gen <- list(
  no_interact = no_interact,
  has_interact = has_interact
  )

# save ----
imp_by_rep_res_gen |> 
  save(file = here("data/ch2/results/fits/by_rep/mice/imp_by_rep_res_gen.rda"))
