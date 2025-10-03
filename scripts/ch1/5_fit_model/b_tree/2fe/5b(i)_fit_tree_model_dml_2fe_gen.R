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
source(here("scripts/ch1/4_prep_model/b_tree/2fe/4b(i)_prep_tree_model_dml_2fe_gen.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# prep learners ----
lrn_spec <- lrn(
  "regr.ranger",
  num.threads = n,
  parallel_predict = TRUE
  )
ps_spec <- lrn(
  "regr.ranger",
  num.threads = n,
  num.trees = 100,
  mtry.ratio = to_tune(0.25, 0.75),
  replace = to_tune(),
  sample.fraction = to_tune(0.25, 0.75)
  )$param_set

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
          
          rsmp_task <- as_task_regr(
            list_4[[i]]$data,
            target = "hr_score"
            )
          rsmp_task$set_col_roles(
            cols = "cow",
            roles = "group"
            )
          rsmp_set <- rsmp(
            "cv",
            folds = 5
            )
          rsmp_set$instantiate(rsmp_task)
          
          tune_sets <- list(
            terminator = trm(
              "evals",
              n_evals = 20
              ),
            algorithm = tnr(
              "random_search",
              batch_size = 20
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
          fit <- spec$fit(store_predictions = TRUE, store_models = TRUE)
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
