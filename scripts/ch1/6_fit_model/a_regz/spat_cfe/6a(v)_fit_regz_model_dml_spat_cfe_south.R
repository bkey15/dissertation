# Fit ridge models using double machine learning (DML). Includes tuning for the ridge lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(parallel)
library(future)
library(tictoc)

# set seed ----
## note: not needed at the moment since splitting occurs prior & search strategy for tuning grid is non-random; but could be useful if one decided to use a random search
set.seed(15275)

# run prep script ----
source(here("scripts/ch1/4_prep_model/a_regz/spat_cfe/4a(vi)_prep_regz_model_dml_spat_cfe_north.R"))

# load splits ----
load(here("data/ch1/results/splits/spat_cfe/splits_imp_dml_dats_spat_cfe_north.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# prep learners ----
lrn_spec <- lrn(
  "regr.glmnet"#,
  #parallel_predict = TRUE
  )
default_s <- lts(
  lrn("regr.glmnet")
  )$param_set$values$s$call
ps_spec <- lrn(
  "regr.glmnet",
  alpha = 0,
  nlambda = 1,
  s = eval(parse(text = default_s))
  )$param_set
n_resolution <- 10

# fit models ----
imp_dml_fits_spat_cfe_north <- list()
interact_stat <- names(imp_dml_dats_spat_cfe_north)

for(stat in interact_stat){
  list_1a <- imp_dml_dats_spat_cfe_north[[stat]]
  list_1b <- splits_imp_dml_dats_spat_cfe_north[[stat]]
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
              "none"
              ),
            algorithm = tnr(
              "grid_search",
              resolution = n_resolution,
              batch_size = n_resolution
              ),
            rsmp_tune = rsmp_set,
            measure = list(
              "ml_l" = msr("regr.mse"),
              "ml_m" = msr("regr.mse")
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
          imp_dml_fits_spat_cfe_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_spat_cfe_north <- tic.log()

# save fits ----
imp_dml_fits_spat_cfe_north |> 
  save(file = here("data/ch1/results/fits/dml_regularize/full_dat/imp_dml_fits_spat_cfe_north.rda"))
tictoc_imp_dml_fits_spat_cfe_north |> 
  save(file = here("data/ch1/results/fits/dml_regularize/runtimes/tictoc_imp_dml_fits_spat_cfe_north.rda"))
