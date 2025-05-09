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

## get treat names
treat_names <- imp_1962_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(n_bits, starts_with("partner_")) |> 
  names()

## get interaction names
interact_names <- imp_1962_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("e_polity2_x_") & !contains(
      c("ss", "ns", "nn")
    )
  ) |> 
  names()

# no interaction terms ----
## start 1962 ----
start_1962 <- list()

for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1962_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1962[[as.character(name)]] <- sum
}

## start 1981 ----
start_1981 <- list()

for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1981_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1981[[as.character(name)]] <- sum
}

## start 1990 ----
start_1990 <- list()

for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1990_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1990[[as.character(name)]] <- sum
}

## combine ----
no_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# has interaction terms ----
## start 1962 ----
start_1962 <- list()

for(i in seq_along(treat_names)){
  j <- treat_names[[i]]
  k <- interact_names[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + e_polity2 + ",
    k,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1962_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1962[[as.character(j)]] <- sum
}

## start 1981 ----
start_1981 <- list()

for(i in seq_along(treat_names)){
  j <- treat_names[[i]]
  k <- interact_names[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + e_polity2 + ",
    k,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1981_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1981[[as.character(j)]] <- sum
}

## start 1990 ----
start_1990 <- list()

for(i in seq_along(treat_names)){
  j <- treat_names[[i]]
  k <- interact_names[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + e_polity2 + ",
    k,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1990_l1, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1990[[as.character(j)]] <- sum
}

## combine ----
has_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# all combine ----
imp_by_rep_res_gen_std <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# save ----
imp_by_rep_res_gen_std |> 
  save(file = here("data/ch2/results/fits/by_rep/mice/imp_by_rep_res_gen_std.rda"))
