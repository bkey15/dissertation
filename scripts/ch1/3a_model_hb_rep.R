# Replicate Hafner-Burton (2005) w/ two-way fixed effects. Not doing autocorrelation for now (see notes).
# Use cluster-robust SEs?

# load packages ----
library(tidyverse)
library(here)
library(broom)

# load data ----
## L1 ----
load(here("data/ch1/preprocessed/ptas_1968_l1.rda"))
load(here("data/ch1/preprocessed/ptas_1977_l1.rda"))

# start 1968 (1st ratified hra) ----

## cpr ----
### mean ----
sum_cpr1_1968 <- lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_cpr2_1968 <- lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### gdppc mean ----
sum_cpr3_1968 <- lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### combine ----
cpr_1968 <- list(
  mean = sum_cpr1_1968,
  gdp = sum_cpr2_1968,
  gdppc = sum_cpr3_1968
  )

## esr ----
### mean ----
sum_esr1_1968 <- lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_esr2_1968 <- lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### gdppc mean ----
sum_esr3_1968 <- lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### combine ----
esr_1968 <- list(
  mean = sum_esr1_1968,
  gdp = sum_esr2_1968,
  gdppc = sum_esr3_1968
  )

## both ----
### mean ----
sum_both1_1968 <- lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_both2_1968 <- lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |>
  summary() |> 
  tidy()

### gdppc mean ----
sum_both3_1968 <- lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### combine ----
both_1968 <- list(
  mean = sum_both1_1968,
  gdp = sum_both2_1968,
  gdppc = sum_both3_1968
  )

## lech ----
### mean ----
sum_lech1_1968 <- lm(hr_score ~ lech_hr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_lech2_1968 <- lm(hr_score ~ lech_hr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### gdppc mean ----
sum_lech3_1968 <- lm(hr_score ~ lech_hr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

### combine ----
lech_1968 <- list(
  mean = sum_lech1_1968,
  gdp = sum_lech2_1968,
  gdppc = sum_lech3_1968
  )

## all combine ----
imp_2_hb_res <- list(
  cpr = cpr_1968,
  esr = esr_1968,
  both = both_1968,
  lech = lech_1968
  )

# Initial reaction: pretty much across the board, we see decreases in HR respect as treatment "dosage" increases, except for gdppc-weighted treatments, where we see increases in HR respect. May suggest that provisions only "work" when partner is rich.

# start 1977 (spilker & böhmelt) ----
## cpr ----
### mean ----
sum_cpr1_1977 <- lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_cpr2_1977 <- lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### gdppc mean ----
sum_cpr3_1977 <- lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### combine ----
cpr_1977 <- list(
  mean = sum_cpr1_1977,
  gdp = sum_cpr2_1977,
  gdppc = sum_cpr3_1977
  )

## esr ----
### mean ----
sum_esr1_1977 <- lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_esr2_1977 <- lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### gdppc mean ----
sum_esr3_1977 <- lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### combine ----
esr_1977 <- list(
  mean = sum_esr1_1977,
  gdp = sum_esr2_1977,
  gdppc = sum_esr3_1977
  )

## both ----
### mean ----
sum_both1_1977 <- lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_both2_1977 <- lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |>
  summary() |> 
  tidy()

### gdppc mean ----
sum_both3_1977 <- lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### combine ----
both_1977 <- list(
  mean = sum_both1_1977,
  gdp = sum_both2_1977,
  gdppc = sum_both3_1977
  )

## lech ----
### mean ----
sum_lech1_1977 <- lm(hr_score ~ lech_hr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### gdp mean ----
sum_lech2_1977 <- lm(hr_score ~ lech_hr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### gdppc mean ----
sum_lech3_1977 <- lm(hr_score ~ lech_hr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

### combine ----
lech_1977 <- list(
  mean = sum_lech1_1977,
  gdp = sum_lech2_1977,
  gdppc = sum_lech3_1977
  )

## all combine ----
imp_3_hb_res <- list(
  cpr = cpr_1977,
  esr = esr_1977,
  both = both_1977,
  lech = lech_1977
  )

# save ----
imp_2_hb_res |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/imp_2_hb_res_gen.rda"))
imp_3_hb_res |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/imp_3_hb_res_gen.rda"))
