# Fit tree models using double machine learning (DML). Includes tuning of hyperparameters via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(parallel)
library(future)
library(tictoc)

# run prep script ----
source(here("scripts/ch1/test8.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# set seed ----
set.seed(15275)

# prep learners ----
lrn_spec <- lrn(
  "regr.xgboost",
  nthread = n,
  parallel_predict = TRUE
  )

# fit models ----
imp_dml_fits_2fe_gen <- list()
interact_stat <- names(imp_dml_dats_2fe_gen)

for(stat in interact_stat){
  list_1 <- imp_dml_dats_2fe_gen[[stat]]
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
          par_grids <- list(
            "ml_l" = lts(lrn(
              "regr.xgboost"
              ))$param_set,
            "ml_m" = lts(lrn(
              "regr.xgboost"
              ))$param_set
            )
          tune_sets <- list(
            terminator = trm(
              "evals",
              n_evals = 10
              ),
            algorithm = tnr(
              "random_search",
              batch_size = 10
              ),
            rsmp_tune = rsmp(
              "cv",
              folds = 5
              ),
            measure = list(
              "ml_l" = msr("regr.mse"),
              "ml_m" = msr("regr.mse")
              )
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
          spec$tune(
            param_set = par_grids,
            tune_settings = tune_sets
            )
          fit <- spec$fit(store_predictions = TRUE)
          toc(log = TRUE)
          imp_dml_fits_2fe_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_2fe_gen <- tic.log()

# save fits ----
imp_dml_fits_2fe_gen |> 
  save(file = here("data/ch1/results/fits/dml_tree/full_dat/imp_dml_fits_2fe_gen.rda"))
tictoc_imp_dml_fits_2fe_gen |> 
  save(file = here("data/ch1/results/fits/dml_tree/runtimes/tictoc_imp_dml_fits_2fe_gen.rda"))
