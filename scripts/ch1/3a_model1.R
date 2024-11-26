library(tidyverse)
library(here)

load(here("data/ch1/preprocessed/ptas_final.rda"))

https://unctadstat.unctad.org/datacentre/dataviewer/US.TradeMerchTotal

test <- ptas_final |> 
  mutate(hr_lag = lag(hr_score))

# cpr ----
lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

# esr ----
lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

