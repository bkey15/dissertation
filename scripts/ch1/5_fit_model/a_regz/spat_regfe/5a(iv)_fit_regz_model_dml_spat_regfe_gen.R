# Fit ridge models using double machine learning (DML). Includes tuning for the ridge lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(parallel)
library(future)
library(tictoc)

# run prep script ----
source(here("scripts/ch1/4b(i)_prep_regz_model_dml_spat_regfe_gen.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# set seed ----
set.seed(15275)

# prep learners ----
lrn_spec <- lrn(
  "regr.cv_glmnet",
  alpha = 0,
  eps = 1e-8,
  lambda.min.ratio = 1e-8,
  s = "lambda.min",
  nlambda = 50,
  nfolds = 5,
  parallel_predict = TRUE
  )

# fit models ----
imp_dml_fits_spat_regfe_gen <- list()
interact_stat <- names(imp_dml_dats_spat_regfe_gen)

for(stat in interact_stat){
  list_1 <- imp_dml_dats_spat_regfe_gen[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
        list_4 <- list_3[[treat]]
        m <- 1:length(list_4)
        for(i in m){
          spec <- DoubleMLPLR$new(
            data = list_4[[i]],
            ml_l = lrn_spec,
            ml_m = lrn_spec,
            n_folds = 5,
            n_rep = 3
            )
          tic(
            paste(
              as.character(stat),
              as.character(year),
              as.character(lag),
              as.character(treat),
              as.character(i),
              sep = "_"
              )
            )
          fit <- spec$fit()
          toc(log = TRUE)
          imp_dml_fits_spat_regfe_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_spat_regfe_gen <- tic.log()

# save fits ----
imp_dml_fits_spat_regfe_gen |> 
  save(file = here("data/ch1/results/fits/dml_regularize/full_dat/imp_dml_fits_spat_regfe_gen.rda"))
tictoc_imp_dml_fits_spat_regfe_gen |> 
  save(file = here("data/ch1/results/fits/dml_regularize/runtimes/tictoc_imp_dml_fits_spat_regfe_gen.rda"))
