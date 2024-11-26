# Replicate Hafner-Burton (2005) w/ two-way fixed effects

# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))

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

## 1949 (1st inforce pta) ----
### note: selecting 1966 as start point yields same results, hence why I don't provide the code for it
ptas_1949 <- ptas_final |> 
  mutate(
    across(
      6:13,
      ~ if_else(
        year > 1948 & is.na(.x), 0, .x
        )
      )
    )

lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
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

## 1949 ----
lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
  summary()

lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
  summary()

lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
  summary()

lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1949) |> 
  summary()
