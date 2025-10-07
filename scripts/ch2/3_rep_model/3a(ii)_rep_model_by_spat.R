# Replicate Bodea-Ye (2018) w/ two-way fixed effects. Not doing autocorrelation for now (see notes).

# load packages ----
library(tidyverse)
library(here)
library(broom)

# load data ----
## L1 ----
load(here("data/ch2/results/imputations/imp_1962_sp_l1.rda"))

# filter data ----
## unimputed data + n_bits spatial lag ----
imp0_1962_sp_l1 <- imp_1962_sp_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(.imp == 0) |> 
  select(-n_bits_sp_lag)

n_bits_sp_lag <- imp_1962_sp_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(.imp == 1) |> 
  select(cow, year, n_bits_sp_lag)

bits_1962_sp_l1 <- left_join(imp0_1962_sp_l1, n_bits_sp_lag) |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    )

## glb_s countries ----
## note: B-Y limit cases to developing countries
bits_1962_sp_l1 <- bits_1962_sp_l1 |> 
  filter(glb_s == 1)

### get treat names
treat_names <- bits_1962_sp_l1 |> 
  select(
    n_bits,
    starts_with("partner_"),
    -ends_with("sp_lag")
    ) |>
  names()

### get interaction names
interact_names <- bits_1962_sp_l1 |> 
  select(
    starts_with("e_polity2_x_"),
    -contains(
      c("ss", "ns", "nn")
      ),
    -ends_with("sp_lag")
    ) |> 
  names()

## start years ----
bits_1981_sp_l1 <- bits_1962_sp_l1 |> 
  filter(year > 1980)

bits_1990_sp_l1 <- bits_1962_sp_l1 |> 
  filter(year > 1989)

# no interaction term ----
## start 1962 (1st in-force BIT) ----
start_1962 <- list()

for(name in treat_names){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1962_sp_l1, lm(as.formula(formula))) |> 
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
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1981_sp_l1, lm(as.formula(formula))) |> 
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
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1990_sp_l1, lm(as.formula(formula))) |> 
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
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1962_sp_l1, lm(as.formula(formula))) |> 
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
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1981_sp_l1, lm(as.formula(formula))) |> 
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
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  sum <- with(bits_1990_sp_l1, lm(as.formula(formula))) |> 
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

# final combine ----
by_rep_res_sp <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# save ----
by_rep_res_sp |> 
  save(file = here("data/ch2/results/fits/by_rep/standard/by_rep_res_sp.rda"))
