# Replicate Bodea-Ye (2018) w/ two-way fixed effects. Not doing autocorrelation for now (see notes).
# Use cluster-robust SEs?

# load packages ----
library(tidyverse)
library(here)
library(broom)

# load data ----
## L1 ----
load(here("data/ch2/preprocessed/bits_1962_l1.rda"))
load(here("data/ch2/preprocessed/bits_1981_l1.rda"))
load(here("data/ch2/preprocessed/bits_1990_l1.rda"))

# filter to glb_s countries ----
## note: B-Y limit cases to developing countries
bits_1962_l1 <- bits_1962_l1 |> 
  filter(glb_s == 1)
bits_1981_l1 <- bits_1981_l1 |> 
  filter(glb_s == 1)
bits_1990_l1 <- bits_1990_l1 |> 
  filter(glb_s == 1)

# no interaction term ----
## start 1962 (1st in-force BIT) ----
### n_bits ----
sum_noin_1_1962 <- lm(hr_score ~ n_bits + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### mean_gdp ----
sum_noin_2_1962 <- lm(hr_score ~ partner_mean_gdp + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### mean_gdppc ----
sum_noin_3_1962 <- lm(hr_score ~ partner_mean_gdppc + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### mean_pop ----
sum_noin_4_1962 <- lm(hr_score ~ partner_mean_pop + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### combine ----
by_res_noin_1962 <- list(
  n_bits = sum_noin_1_1962,
  gdp = sum_noin_2_1962,
  gdppc = sum_noin_3_1962,
  pop = sum_noin_4_1962
  )

## start 1981 (B-Y start date) ----
### n_bits ----
sum_noin_1_1981 <- lm(hr_score ~ n_bits + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### mean_gdp ----
sum_noin_2_1981 <- lm(hr_score ~ partner_mean_gdp + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### mean_gdppc ----
sum_noin_3_1981 <- lm(hr_score ~ partner_mean_gdppc + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### mean_pop ----
sum_noin_4_1981 <- lm(hr_score ~ partner_mean_pop + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### combine ----
by_res_noin_1981 <- list(
  n_bits = sum_noin_1_1981,
  gdp = sum_noin_2_1981,
  gdppc = sum_noin_3_1981,
  pop = sum_noin_4_1981
  )

## start 1990 (B-Y robust start date) ----
### n_bits ----
sum_noin_1_1990 <- lm(hr_score ~ n_bits + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

### mean_gdp ----
sum_noin_2_1990 <- lm(hr_score ~ partner_mean_gdp + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

### mean_gdppc ----
sum_noin_3_1990 <- lm(hr_score ~ partner_mean_gdppc + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

### mean_pop ----
sum_noin_4_1990 <- lm(hr_score ~ partner_mean_pop + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

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

# has interaction term ----
## start 1962 ----
### n_bits ----
sum_int_1_1962 <- lm(hr_score ~ n_bits + e_polity2 + n_bits*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### mean_gdp ----
sum_int_2_1962 <- lm(hr_score ~ partner_mean_gdp + e_polity2 + partner_mean_gdp*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### mean_gdppc ----
sum_int_3_1962 <- lm(hr_score ~ partner_mean_gdppc + e_polity2 + partner_mean_gdppc*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### mean_pop ----
sum_int_4_1962 <- lm(hr_score ~ partner_mean_pop + e_polity2 + partner_mean_pop*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1962_l1) |> 
  summary() |> 
  tidy()

### combine ----
by_res_int_1962 <- list(
  n_bits = sum_int_1_1962,
  gdp = sum_int_2_1962,
  gdppc = sum_int_3_1962,
  pop = sum_int_4_1962
  )

## start 1981 ----
### n_bits ----
sum_int_1_1981 <- lm(hr_score ~ n_bits + e_polity2 + n_bits*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### mean_gdp ----
sum_int_2_1981 <- lm(hr_score ~ partner_mean_gdp + e_polity2 + partner_mean_gdp*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### mean_gdppc ----
sum_int_3_1981 <- lm(hr_score ~ partner_mean_gdppc + e_polity2 + partner_mean_gdppc*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### mean_pop ----
sum_int_4_1981 <- lm(hr_score ~ partner_mean_pop + e_polity2 + partner_mean_pop*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1981_l1) |> 
  summary() |> 
  tidy()

### combine ----
by_res_int_1981 <- list(
  n_bits = sum_int_1_1981,
  gdp = sum_int_2_1981,
  gdppc = sum_int_3_1981,
  pop = sum_int_4_1981
  )

## start 1990 ----
### n_bits ----
sum_int_1_1990 <- lm(hr_score ~ n_bits + e_polity2 + n_bits*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

### mean_gdp ----
sum_int_2_1990 <- lm(hr_score ~ partner_mean_gdp + e_polity2 + partner_mean_gdp*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

### mean_gdppc ----
sum_int_3_1990 <- lm(hr_score ~ partner_mean_gdppc + e_polity2 + partner_mean_gdppc*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

### mean_pop ----
sum_int_4_1990 <- lm(hr_score ~ partner_mean_pop + e_polity2 + partner_mean_pop*e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year), data = bits_1990_l1) |> 
  summary() |> 
  tidy()

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
by_rep_res_std <- list(
  no_interact = no_interact,
  has_interact = has_interact
  )

# save ----
by_rep_res_std |> 
  save(file = here("data/ch2/results/fits/by_rep/standard/by_rep_res_std.rda"))
