# Replicate Hafner-Burton (2005) w/ two-way fixed effects. Not doing autocorrelation for now (see notes).
# Use cluster-robust SEs?

# load packages ----
library(tidyverse)
library(here)

# load data ----
## L1 ----
load(here("data/ch1/preprocessed/ptas_final_l1.rda"))
load(here("data/ch1/preprocessed/ptas_1968_l1.rda"))
load(here("data/ch1/preprocessed/ptas_1977_l1.rda"))

# cpr ----
## no start point ----
lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

lm(hr_score ~ cpr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

## 1968 (1st ratified hra) ----
lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ cpr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

## 1977 (spilker & böhmelt) ----
lm(hr_score ~ cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

lm(hr_score ~ cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

lm(hr_score ~ cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

lm(hr_score ~ cpr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

# esr ----
## no start point ----
lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

lm(hr_score ~ esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

## 1968 ----
lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

## 1977 ----
lm(hr_score ~ esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

lm(hr_score ~ esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

lm(hr_score ~ esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

lm(hr_score ~ esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1977_l1) |> 
  summary()

# Initial reaction: pretty much across the board, we see decreases in HR respect as treatment "dosage" increases, except for gdppc-weighted treatments, where we see increases in HR respect. May suggest that provisions only "work" when partner is rich.

# both ----
## no start point ----
lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |> 
  summary()

lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary()

lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary()

lm(hr_score ~ cpr_pop_mean + esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_final_l1) |>
  summary()

## 1968 ----
lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |>
  summary()

lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

lm(hr_score ~ cpr_pop_mean + esr_pop_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year), data = ptas_1968_l1) |> 
  summary()

