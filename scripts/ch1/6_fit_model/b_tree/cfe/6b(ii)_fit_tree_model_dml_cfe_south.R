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
source(here("scripts/ch1/4c(ii)_prep_model_dml_2fe_south.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# set seed ----
set.seed(15275)

# fit models ----
imp_dml_fits_2fe_south <- list()
interact_stat <- names(imp_dml_dats_2fe_south)

for(stat in interact_stat){
  list_1 <- imp_dml_dats_2fe_south[[stat]]
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
            ml_l = lrn(
              "regr.ranger",
              num.threads = n,
              num.trees = 250,
              replace = FALSE,
              parallel_predict = TRUE
              ),
            ml_m = lrn(
              "regr.ranger",
              num.threads = n,
              num.trees = 250,
              replace = FALSE,
              parallel_predict = TRUE
              ),
            n_folds = 5,
            n_rep = 3
            )
          par_grids <- list(
            "ml_l" = lrn(
              "regr.ranger",
              mtry.ratio = to_tune(),
              sample.fraction = to_tune(0.1, 1)
              )$param_set,
            "ml_m" = lrn(
              "regr.ranger",
              mtry.ratio = to_tune(),
              sample.fraction = to_tune(0.1, 1)
              )$param_set
            )
          tune_sets <- list(
            terminator = trm(
              "evals",
              n_evals = 4
              ),
            algorithm = tnr(
              "random_search",
              batch_size = 4
              ),
            rsmp_tune = rsmp(
              "cv",
              folds = 3
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
          fit <- spec$fit()
          toc(log = TRUE)
          imp_dml_fits_2fe_south[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_2fe_south <- tic.log()

# save fits ----
imp_dml_fits_2fe_south |> 
  save(file = here("data/ch1/results/fits/dml_tree/full_dat/rc_imp_dml_fits_2fe_south.rda"))
tictoc_imp_dml_fits_2fe_south |> 
  save(file = here("data/ch1/results/fits/dml_tree/runtimes/tictoc_imp_dml_fits_2fe_south.rda"))
