# Replicate Hafner-Burton (2005) w/ two-way fixed effects. Not doing autocorrelation for now (see notes).
# Use cluster-robust SEs?

# load packages ----
library(tidyverse)
library(here)
library(broom)

# load data ----
load(here("data/ch1/results/imputations/imp_1968_t_lags.rda"))
load(here("data/ch1/results/imputations/imp_1977_t_lags.rda"))

## L1 ----
hb_rep_1968_l1 <- imp_1968_t_lags[["l1"]] |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(.imp == 0)

hb_rep_1977_l1 <- imp_1977_t_lags[["l1"]] |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(.imp == 0)

## get treat names
treat_names <- hb_rep_1968_l1 |> 
  select(
    contains("mean"),
    -contains(c("pop", "_x_")),
    -starts_with(
      c("ss_", "ns_", "nn_")
      )
    ) |> 
  names()

treat_names_cpr <- hb_rep_1968_l1 |> 
  select(
    starts_with("cpr_"),
    -contains("pop")
    ) |> 
  names()

treat_names_esr <- hb_rep_1968_l1 |> 
  select(
    starts_with("esr_"),
    -contains("pop")
    ) |> 
  names()

# start 1968 (1st ratified hra) ----
start_1968 <- list()

## single treatment ----
for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year)"
    )
  
  sum <- with(hb_rep_1968_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1968[[as.character(name)]] <- sum
}

## cpr & esr ----
for(i in seq_along(treat_names_cpr)){
  j <- treat_names_cpr[[i]]
  k <- treat_names_esr[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year)"
    )
  
  sum <- with(hb_rep_1968_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1968[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

# Initial reaction: pretty much across the board, we see decreases in HR respect as treatment "dosage" increases, except for gdppc-weighted treatments, where we see increases in HR respect. May suggest that provisions only "work" when partner is rich.

# start 1977 (spilker & böhmelt) ----
start_1977 <- list()

## single treatment ----
for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year)"
    )
  
  sum <- with(hb_rep_1977_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1977[[as.character(name)]] <- sum
}

## cpr & esr ----
for(i in seq_along(treat_names_cpr)){
  j <- treat_names_cpr[[i]]
  k <- treat_names_esr[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + factor(cow) + factor(year)"
  )
  
  sum <- with(hb_rep_1977_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1977[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

# combine ----
hb_rep_res_std <- list(
  start_1968 = start_1968,
  start_1977 = start_1977
  )

# save ----
hb_rep_res_std |> 
  save(file = here("data/ch1/results/fits/hb_rep/standard/hb_rep_res_std.rda"))
