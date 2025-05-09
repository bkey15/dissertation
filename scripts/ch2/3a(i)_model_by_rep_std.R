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

# filter to south ----
## note: B-Y limit cases to developing countries
bits_1962_l1 <- bits_1962_l1 |> 
  filter(glb_s == 1)
bits_1981_l1 <- bits_1981_l1 |> 
  filter(glb_s == 1)
bits_1990_l1 <- bits_1990_l1 |> 
  filter(glb_s == 1)

## get treat names
treat_names <- bits_1962_l1 |> 
  select(n_bits, starts_with("partner_")) |> 
  names()

## get interaction names
interact_names <- bits_1962_l1 |> 
  select(
    starts_with("e_polity2_x_") & !contains(
      c("ss", "ns", "nn")
      )
    ) |> 
  names()

# no interaction term ----
## start 1962 (1st in-force BIT) ----
start_1962 <- list()

for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1962_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1962[[as.character(name)]] <- sum
}

## start 1981 (B-Y start date) ----
start_1981 <- list()

for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1981_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1981[[as.character(name)]] <- sum
}

## start 1990 (B-Y robust start date) ----
start_1990 <- list()

for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1990_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1990[[as.character(name)]] <- sum
}

## combine ----
no_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# has interaction term ----
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
  
  sum <- with(bits_1962_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
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
  
  sum <- with(bits_1981_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
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
  
  sum <- with(bits_1990_l1, lm(as.formula(formula))) |> 
    summary() |> 
    tidy()
  
  start_1990[[as.character(j)]] <- sum
}

## combine ----
has_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# all combine ----
by_rep_res_std <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# save ----
by_rep_res_std |> 
  save(file = here("data/ch2/results/fits/by_rep/standard/by_rep_res_std.rda"))
