# Fit tree models using double machine learning (DML). Includes tuning of hyperparameters via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(parallel)
library(future)
library(tictoc)

# set seed ----
## important: set seed before running prep script
set.seed(15275)

# run prep script ----
source(here("scripts/ch1/4_prep_model/a_regz/ii_slx/4a(iv)_prep_regz_model_dml_slx_gen.R"))

# load splits ----
load(here("data/ch1/results/splits/bcr_slx/splits_imp_dml_dats_bcr_slx_gen.rda"))

# set cores ----
## check
n <- detectCores()

## set
plan(multisession, workers = n)

# prep learners ----
lrn_spec <- lrn(
  "regr.xgboost",
  nthread = 1,
  booster = "gbtree",
  tree_method = "hist",
  colsample_bylevel = 1,
  alpha = 1
  )

ps_spec <- lrn(
  "regr.xgboost",
  eta = to_tune(0.01, 0.2, logscale = TRUE),
  nrounds = to_tune(300, 900),
  max_depth = to_tune(1, 3),
  min_child_weight = to_tune(0.5, 20, logscale = TRUE),
  subsample = to_tune(0.6, 1),
  colsample_bytree = to_tune(0.6, 1),
  lambda = to_tune(0.1, 10, logscale = TRUE)
  )$param_set

n_resolution <- 12

# fit models ----
imp_dml_fits_bcr_slx_gen_tree <- list()
interact_stat <- names(imp_dml_dats_slx_gen)

lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

for(stat in interact_stat){
  list_1a <- imp_dml_dats_slx_gen[[stat]]
  list_1b <- splits_imp_dml_dats_bcr_slx_gen[[stat]]
  start_yrs <- names(list_1a)
  for(year in start_yrs){
    list_2a <- list_1a[[year]]
    list_2b <- list_1b[[year]]
    lag_names <- names(list_2a)
    for(lag in lag_names){
      list_3a <- list_2a[[lag]]
      list_3b <- list_2b[[lag]]
      treat_names <- names(list_3a)
      for(treat in treat_names){
        list_4a <- list_3a[[treat]]
        list_4b <- list_3b[[treat]]
        m <- 1:length(list_4a)
        for(i in m){
          spec <- DoubleMLPLR$new(
            data = list_4a[[i]],
            ml_l = lrn_spec,
            ml_m = lrn_spec
            )
          smpls <- list_4b[[i]][-1]
          spec$set_sample_splitting(smpls)
          
          rsmp_task <- as_task_regr(
            list_4a[[i]]$data_model,
            target = "hr_score"
            )
          rsmp_set <- rsmp("custom")
          rsmp_set$instantiate(rsmp_task, train_sets = list_4b[[i]][[1]]$train_ids, test_sets = list_4b[[i]][[1]]$test_ids)
          
          tune_sets <- list(
            terminator = trm(
              "evals",
              n_evals = n_resolution
              ),
            algorithm = tnr(
              "random_search",
              batch_size = n_resolution
              ),
            rsmp_tune = rsmp_set,
            measure = list(
              ml_l = msr("regr.mse"),
              ml_m = msr("regr.mse")
              )
            )
          par_grids <- list(
            "ml_l" = ps_spec,
            "ml_m" = ps_spec
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
          imp_dml_fits_bcr_slx_gen_tree[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_bcr_slx_gen_tree <- tic.log()

# save fits ----
imp_dml_fits_bcr_slx_gen_tree |> 
  save(file = here("data/ch1/results/fits/dml_tree/full_dat/imp_dml_fits_bcr_slx_gen_tree.rda"))
tictoc_imp_dml_fits_bcr_slx_gen_tree |> 
  save(file = here("data/ch1/results/fits/dml_tree/runtimes/tictoc_imp_dml_fits_bcr_slx_gen_tree.rda"))
