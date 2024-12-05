# Replicate Hafner-Burton (2005) w/ two-way fixed effects. Not doing autocorrelation for now (see notes).
# Use cluster-robust SEs?

# load packages ----
library(tidyverse)
library(here)
library(broom)

# load data ----
## L1 ----
load(here("data/ch1/preprocessed/ptas_final_l1.rda"))
load(here("data/ch1/preprocessed/ptas_1968_l1.rda"))
load(here("data/ch1/preprocessed/ptas_1977_l1.rda"))

# cpr ----
## no start year ----
sum_cpr1_nost <- lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_cpr2_nost <- lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_cpr3_nost <- lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_cpr4_nost <- lm(hr_score ~ cpr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

## 1968 (1st ratified hra) ----
sum_cpr1_1968 <- lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_cpr2_1968 <- lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_cpr3_1968 <- lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_cpr4_1968 <- lm(hr_score ~ cpr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

## 1977 (spilker & böhmelt) ----
sum_cpr1_1977 <- lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_cpr2_1977 <- lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_cpr3_1977 <- lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_cpr4_1977 <- lm(hr_score ~ cpr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

# esr ----
## no start year ----
sum_esr1_nost <- lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_esr2_nost <- lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_esr3_nost <- lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_esr4_nost <- lm(hr_score ~ esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

## 1968 ----
sum_esr1_1968 <- lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_esr2_1968 <- lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_esr3_1968 <- lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_esr4_1968 <- lm(hr_score ~ esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

## 1977 ----
sum_esr1_1977 <- lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_esr2_1977 <- lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_esr3_1977 <- lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_esr4_1977 <- lm(hr_score ~ esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

# Initial reaction: pretty much across the board, we see decreases in HR respect as treatment "dosage" increases, except for gdppc-weighted treatments, where we see increases in HR respect. May suggest that provisions only "work" when partner is rich.

# both ----
## no start year ----
sum_both1_nost <- lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary() |> 
  tidy()

sum_both2_nost <- lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_both3_nost <- lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

sum_both4_nost <- lm(hr_score ~ cpr_pop_mean + esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary() |> 
  tidy()

## 1968 ----
sum_both1_1968 <- lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_both2_1968 <- lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |>
  summary() |> 
  tidy()

sum_both3_1968 <- lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

sum_both4_1968 <- lm(hr_score ~ cpr_pop_mean + esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary() |> 
  tidy()

## 1977 ----
sum_both1_1977 <- lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_both2_1977 <- lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |>
  summary() |> 
  tidy()

sum_both3_1977 <- lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

sum_both4_1977 <- lm(hr_score ~ cpr_pop_mean + esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary() |> 
  tidy()

# save ----
## no start year ----
sum_cpr1_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_cpr1_nost.rda"))
sum_cpr2_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_cpr2_nost.rda"))
sum_cpr3_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_cpr3_nost.rda"))
sum_esr1_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_esr1_nost.rda"))
sum_esr2_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_esr2_nost.rda"))
sum_esr3_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_esr3_nost.rda"))
sum_both1_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_both1_nost.rda"))
sum_both2_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_both2_nost.rda"))
sum_both3_nost |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/no_start_year/sum_both3_nost.rda"))

## 1968 ----
sum_cpr1_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_cpr1_1968.rda"))
sum_cpr2_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_cpr2_1968.rda"))
sum_cpr3_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_cpr3_1968.rda"))
sum_esr1_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_esr1_1968.rda"))
sum_esr2_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_esr2_1968.rda"))
sum_esr3_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_esr3_1968.rda"))
sum_both1_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_both1_1968.rda"))
sum_both2_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_both2_1968.rda"))
sum_both3_1968 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1968/sum_both3_1968.rda"))

## 1977 ----
sum_cpr1_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_cpr1_1977.rda"))
sum_cpr2_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_cpr2_1977.rda"))
sum_cpr3_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_cpr3_1977.rda"))
sum_esr1_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_esr1_1977.rda"))
sum_esr2_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_esr2_1977.rda"))
sum_esr3_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_esr3_1977.rda"))
sum_both1_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_both1_1977.rda"))
sum_both2_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_both2_1977.rda"))
sum_both3_1977 |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/start_1977/sum_both3_1977.rda"))
