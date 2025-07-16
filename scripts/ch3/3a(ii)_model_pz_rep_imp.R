# Soft replicate Peez (2025) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals (& lagged covars).
# Note: soft replicate = LMs using Peez's matching covars. He doesn't use the model below in his paper.

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch3/results/imputations/imp_1990_t_lags.rda"))

# get lag & treat names ----
lag_names <- names(imp_1990_t_lags)
treat_names <- c("n_ems", "any_inforce")

# run models ----
## start 1990 ----
start_1990 <- list()

for(lag in lag_names){
  imp_dat <- imp_1990_t_lags[[lag]]
  for(treat in treat_names){
    formula <- paste0(
      "hr_score ~ ",
      treat,
      " + coup_success + cont_elect + gov_kill + v2x_neopat + v2cademmob + wdi_gdpg + wdi_inflat + gdppc_log10 + pol_prox + v2x_frassoc_thick + v2x_polyarchy + factor(cow) + factor(year)"
    )
    
    fit <- with(imp_dat, lm(as.formula(formula)))
    
    pool <- pool(fit)
    sum <- summary(pool)
    
    start_1990[[as.character(lag)]][[as.character(treat)]] <- sum
  }
}

# combine ----
imp_pz_rep_res <- list(
  start_1990 = start_1990
  )

# save ----
imp_pz_rep_res |> 
  save(file = here("data/ch3/results/fits/pz_rep/imp_pz_rep_res.rda"))
