# Replicate Hafner-Burton (2005) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals (& lagged covars).

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_1968_t_lags.rda"))
load(here("data/ch1/results/imputations/imp_1977_t_lags.rda"))

## L1 ----
hb_rep_1968_l1 <- imp_1968_t_lags[["l1"]]
hb_rep_1977_l1 <- imp_1977_t_lags[["l1"]]

## get treat names
treat_names <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    contains("mean"),
    -contains(c("pop", "_x_")),
    -starts_with(
      c("ss_", "ns_", "nn_")
      )
    ) |> 
  names()

treat_names_cpr <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("cpr_"),
    -contains("pop")
    ) |> 
  names()

treat_names_esr <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("esr_"),
    -contains("pop")
    ) |> 
  names()

# start 1968 ----
start_1968 <- list()

## single treatment ----
for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(hb_rep_1968_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
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
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(hb_rep_1968_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1968[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

# start 1977 ----
start_1977 <- list()

## single treatment ----
for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(hb_rep_1977_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
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
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(hb_rep_1977_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1977[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

# combine ----
imp_hb_rep_res_gen <- list(
  start_1968 = start_1968,
  start_1977 = start_1977
  )

# save ----
imp_hb_rep_res_gen |> 
  save(file = here("data/ch1/results/fits/hb_rep/mice/imp_hb_rep_res_gen.rda"))
