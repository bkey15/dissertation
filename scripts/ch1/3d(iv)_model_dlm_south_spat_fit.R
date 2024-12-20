# Fit lasso models using double machine learning (DML). Includes tuning for the lasso lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3learners)
library(parallel)
library(doMC)

# load data ----
load(here("data/ch1/results/fits/dml_lasso/dml_initial/imp_2_dml_south_spat_dats.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_initial/imp_3_dml_south_spat_dats.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# fit models ----
## imp_2 ----
imp_2_dml_south_spat_fits <- list()
treat_names <- names(imp_2_dml_south_spat_dats)
m <- 1:5

for(name in treat_names){
  list <- imp_2_dml_south_spat_dats[[name]]
  for(i in m){
    set.seed(15275)
    spec <- DoubleMLPLR$new(
      data = list[[i]],
      ml_l = lrn(
        "regr.cv_glmnet",
        s = "lambda.min",
        parallel = TRUE
      ),
      ml_m = lrn(
        "regr.cv_glmnet",
        s = "lambda.min",
        parallel = TRUE
      ),
      n_folds = 5,
      n_rep = 3
    )
    fit <- spec$fit()
    imp_2_dml_south_spat_fits[[as.character(name)]][[as.character(i)]] <- fit
  }
}

## imp_3 ----
imp_3_dml_south_spat_fits <- list()
treat_names <- names(imp_3_dml_south_spat_dats)
m <- 1:5

for(name in treat_names){
  list <- imp_3_dml_south_spat_dats[[name]]
  for(i in m){
    set.seed(15275)
    spec <- DoubleMLPLR$new(
      data = list[[i]],
      ml_l = lrn(
        "regr.cv_glmnet",
        s = "lambda.min",
        parallel = TRUE
      ),
      ml_m = lrn(
        "regr.cv_glmnet",
        s = "lambda.min",
        parallel = TRUE
      ),
      n_folds = 5,
      n_rep = 3
    )
    fit <- spec$fit()
    imp_3_dml_south_spat_fits[[as.character(name)]][[as.character(i)]] <- fit
  }
}

# save fits ----
imp_2_dml_south_spat_fits |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_2_dml_south_spat_fits.rda"))
imp_3_dml_south_spat_fits |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_3_dml_south_spat_fits.rda"))
