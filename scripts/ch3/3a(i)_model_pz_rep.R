# Soft replicate Peez (2025) w/ two-way fixed effects & no imputed vals.
# Note: soft replicate = LMs using Peez's matching covars. He doesn't use the model below in his paper.
# Use cluster-robust SEs?

# load packages ----
library(tidyverse)
library(here)
library(broom)

# load data ----
load(here("data/ch3/results/imputations/imp_1990_t_lags.rda"))

# get unimputed data ----
ems_1990 <- list()
lag_names <- names(imp_1990_t_lags)

for(lag in lag_names){
  df <- imp_1990_t_lags[[lag]] |> 
    mice::complete(
      action = "long",
      include = TRUE
      ) |> 
    filter(.imp == 0)
  
  imp_1990_t_lags[[as.character(lag)]] <- df
}

# get treat & interaction names ----
treat_names <- c("n_ems", "any_inforce")
interact_names <- c(
  "v2x_polyarchy_x_n_ems",
  "v2x_polyarchy_x_any_inforce"
  )

# no interaction terms ----
## start 1990 ----
start_1990 <- list()

for(lag in lag_names){
  df <- imp_1990_t_lags[[lag]]
  for(treat in treat_names){
    formula <- paste0(
      "hr_score ~ ",
      treat,
      " + coup_success + cont_elect + gov_kill + v2x_neopat + v2cademmob + wdi_gdpg + wdi_inflat + gdppc_log10 + pol_prox + v2x_frassoc_thick + v2x_polyarchy + factor(cow) + factor(year)"
      )
  
    sum <- with(df, lm(as.formula(formula))) |> 
      summary() |> 
      tidy()
  
    start_1990[[as.character(lag)]][[as.character(treat)]] <- sum
    }
  }

## combine ----
no_interactions <- list(
  start_1990 = start_1990
  )

# has interaction term ----
## start 1990 ----
start_1990 <- list()

for(lag in lag_names){
  df <- imp_1990_t_lags[[lag]]
  for(i in seq_along(treat_names)){
    j <- treat_names[[i]]
    k <- interact_names[[i]]
    
    formula <- paste0(
      "hr_score ~ ",
      j,
      " + ",
      k,
      " + coup_success + cont_elect + gov_kill + v2x_neopat + v2cademmob + wdi_gdpg + wdi_inflat + gdppc_log10 + pol_prox + v2x_frassoc_thick + v2x_polyarchy + factor(cow) + factor(year)"
      )
    
    sum <- with(df, lm(as.formula(formula))) |> 
      summary() |> 
      tidy()
    
    start_1990[[as.character(lag)]][[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
    }
}

## combine ----
has_interactions <- list(
  start_1990 = start_1990
  )

# all combine ----
pz_rep_res <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# save ----
pz_rep_res |> 
  save(file = here("data/ch3/results/fits/pz_rep/pz_rep_res.rda"))
