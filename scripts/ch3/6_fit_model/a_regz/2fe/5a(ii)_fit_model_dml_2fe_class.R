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
## important: set seed before running prep script
set.seed(15275)

# run prep script ----
source(here("scripts/ch3/4_prep_model/a_regz/2fe/4a(ii)_prep_model_dml_2fe_class.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# prep learners ----
lrn_spec_regr <- lrn(
  "regr.glmnet",
  parallel_predict = TRUE
  )
lrn_spec_class <- lrn(
  "classif.glmnet",
  parallel_predict = TRUE
  )

default_s <- lts(
  lrn("regr.glmnet")
  )$param_set$values$s$call

ps_spec_regr <- lrn(
  "regr.glmnet",
  alpha = 0,
  nlambda = 1,
  s = eval(parse(text = default_s))
  )$param_set
ps_spec_class <- lrn(
  "classif.glmnet",
  alpha = 0,
  nlambda = 1,
  s = eval(parse(text = default_s))
  )$param_set

# fit models ----
## get initial specs ----
imp_dml_fits_2fe_class <- list()
treat_num_stat <- names(imp_dml_dats_2fe)

## finalize ----
for(stat in treat_num_stat){
  list_1 <- imp_dml_dats_2fe[[stat]]
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
          dml_dat_i <- list_4[[i]]
          if(str_detect(treat, "inforce_X1")){
            spec <- DoubleMLPLR$new(
              data = dml_dat_i,
              ml_l = lrn_spec_regr,
              ml_m = lrn_spec_class,
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
              terminator = trm("none"),
              algorithm = tnr(
                "grid_search",
                resolution = 20,
                batch_size = 20
                ),
              rsmp_tune = rsmp_set,
              measure = list(
                "ml_l" = msr("regr.mse"),
                "ml_m" = msr("classif.prauc")
                )
              )
            par_grids <- list(
              "ml_l" = ps_spec_regr,
              "ml_m" = ps_spec_class
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
            imp_dml_fits_2fe_class[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
          }
          else{
            spec <- DoubleMLPLR$new(
              data = list_4[[i]],
              ml_l = lrn_spec_regr,
              ml_m = lrn_spec_regr,
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
              terminator = trm("none"),
              algorithm = tnr(
                "grid_search",
                resolution = 20,
                batch_size = 20
                ),
              rsmp_tune = rsmp_set,
              measure = list(
                "ml_l" = msr("regr.mse"),
                "ml_m" = msr("regr.mse")
                )
              )
            par_grids <- list(
              "ml_l" = ps_spec_regr,
              "ml_m" = ps_spec_regr
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
            imp_dml_fits_2fe_class[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
          }
        }
      }
    }
  }
}

tictoc_imp_dml_fits_2fe_class <- tic.log()

# save fits ----
imp_dml_fits_2fe_class |> 
  save(file = here("data/ch3/results/fits/dml_regularize/full_dat/imp_dml_fits_2fe_class.rda"))
tictoc_imp_dml_fits_2fe_class |> 
  save(file = here("data/ch3/results/fits/dml_regularize/runtimes/tictoc_imp_dml_fits_2fe_class.rda"))
