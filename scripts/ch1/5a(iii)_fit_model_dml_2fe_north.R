# Fit lasso models using double machine learning (DML). Includes tuning for the lasso lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3learners)
library(parallel)
library(doMC)

# run prep script ----
source(here("scripts/ch1/4a(iii)_prep_model_dml_2fe_north.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# fit models ----
imp_dml_fits_2fe_north <- list()
interact_stat <- names(imp_dml_dats_2fe_north)

for(stat in interact_stat){
  list_1 <- imp_dml_dats_2fe_north[[stat]]
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
        if(str_detect(treat, "lech")){
          for(i in m){
            set.seed(15275)
            spec <- DoubleMLPLR$new(
              data = list_4[[i]],
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
            imp_dml_fits_2fe_north[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
          }
        }
      }
    }
  }
}

# save fits ----
imp_dml_fits_2fe_north |> 
  save(file = here("data/ch1/results/fits/dml_lasso/full_dat/imp_dml_fits_2fe_north.rda"))
