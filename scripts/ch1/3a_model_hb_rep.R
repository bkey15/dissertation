# Replicate Hafner-Burton (2005) w/ two-way fixed effects. Not doing autocorrelation for now.
# Use cluster-robust SEs?

# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))
load(here("data/ch1/preprocessed/ptas_1968.rda"))
load(here("data/ch1/preprocessed/ptas_1977.rda"))

# cpr ----
## no start point ----
lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

## 1968 (1st ratified hra) ----
lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

## 1977 (spilker & böhmelt) ----
lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

# esr ----
## no start point ----
lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

## 1968 ----
lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

# Initial reaction: pretty much across the board, we see decreases in HR respect as treatment "dosage" increases, except for gdppc-weighted treatments, where we see increases in HR respect. May suggest that provisions only "work" when partner is rich.

# both ----
## no start point ----
lm(hr_score ~ lag(cpr_mean) + lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

## 1968 ----
lm(hr_score ~ lag(cpr_mean) + lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

# check dropped rows ----
drops1 <- ptas_final |> 
  select(hr_score, cpr_mean, e_polity2, p_durable, wdi_popden, wdi_trade, inv, gdppc_log10, cow, year) |> 
  filter(c(is.na(cow) | is.na(year) | is.na(hr_score) | is.na(cpr_mean) | is.na(e_polity2) | is.na(p_durable) | is.na(wdi_popden) | is.na(wdi_trade) | is.na(inv) | is.na(gdppc_log10)))

drops2 <- ptas_1968 |> 
  select(hr_score, cpr_mean, e_polity2, p_durable, wdi_popden, wdi_trade, inv, gdppc_log10, cow, year) |> 
  filter(c(is.na(cow) | is.na(year) | is.na(hr_score) | is.na(cpr_mean) | is.na(e_polity2) | is.na(p_durable) | is.na(wdi_popden) | is.na(wdi_trade) | is.na(inv) | is.na(gdppc_log10)))

