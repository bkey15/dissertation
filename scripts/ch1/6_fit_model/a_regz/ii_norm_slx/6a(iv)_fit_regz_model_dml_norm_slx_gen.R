# Fit ridge models using double machine learning (DML). Includes tuning for the ridge lambda parameter via v-fold cross-validation.
# Model type: SLX, normal DML sample splitting

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

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# prep learners ----
## note: this model could run on regr.cv_glmnet, but am keeping regr.glmnet to remain consistent
lrn_spec <- lrn("regr.glmnet")
default_s <- lts(
  lrn("regr.glmnet")
  )$param_set$values$s$call
ps_spec <- lrn(
  "regr.glmnet",
  nlambda = 1,
  s = eval(parse(text = default_s))
  )$param_set
n_resolution <- 10

# fit models ----
imp_dml_fits_norm_slx_gen <- list()
interact_stat <- names(imp_dml_dats_slx_gen)

for(stat in interact_stat){
  list_1 <- imp_dml_dats_slx_gen[[stat]]
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
            n_folds = 10,
            n_rep = 3
            )
          
          rsmp_task <- as_task_regr(
            list_4[[i]]$data,
            target = "hr_score"
            )
          rsmp_set <- rsmp(
            "cv",
            folds = 10
            )
          rsmp_set$instantiate(rsmp_task)
          
          tune_sets <- list(
            terminator = trm("none"),
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
          imp_dml_fits_norm_slx_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_norm_slx_gen <- tic.log()

# save fits ----
imp_dml_fits_norm_slx_gen |> 
  save(file = here("data/ch1/results/fits/dml_regz/full_dat/imp_dml_fits_norm_slx_gen.rda"))
tictoc_imp_dml_fits_norm_slx_gen |> 
  save(file = here("data/ch1/results/fits/dml_regz/runtimes/tictoc_imp_dml_fits_norm_slx_gen.rda"))
