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
source(here("scripts/ch1/4a(i)_prep_regz_model_dml_2fe_gen.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# set seed ----
set.seed(15275)

# prep learners ----
treat_encode <- po("encode", method = "treatment")

## for param set ----
lrn_ridge_0 <- lrn(
  "regr.cv_glmnet",
  alpha = 0,
  eps = 1e-8,
  lambda.min.ratio = 1e-8,
  s = "lambda.min",
  nlambda = 50,
  nfolds = 5,
  parallel_predict = TRUE,
  id = "ridge_0"
  )
lrn_lasso_1 <- lrn(
  "regr.cv_glmnet",
  alpha = 1,
  eps = 1e-8,
  lambda.min.ratio = 1e-8,
  s = "lambda.min",
  nlambda = 50,
  nfolds = 5,
  parallel_predict = TRUE,
  id = "lasso_1"
  )
po_ridge_0 <- po(
  "learner_cv",
  learner = lrn_ridge_0,
  resampling.folds = 5
  )

lrn_rf_0_ps <- lrn(
  "regr.ranger",
  num.threads = n,
  num.trees = 100,
  replace = FALSE,
  mtry.ratio = to_tune(),
  sample.fraction = to_tune(0.1, 1),
  parallel_predict = TRUE
  )
po_rf_0_ps <- po(
  "learner_cv",
  learner = lrn_rf_0_ps,
  resampling.folds = 5,
  id = "regr.ranger.0"
  )

gr_level_0_ps <- gunion(
  list(
    treat_encode %>>% po_ridge_0,
    po_rf_0_ps
    )
  )
gr_stack_ps <- gr_level_0_ps %>>% 
  po("featureunion") %>>% 
  po(
    "learner",
    learner = lrn_lasso_1
    )

## for full learners ----
lrn_rf_0 <- lrn(
  "regr.ranger",
  num.threads = n,
  num.trees = 100,
  replace = FALSE,
  parallel_predict = TRUE
  )
po_rf_0 <- po(
  "learner_cv",
  learner = lrn_rf_0,
  resampling.folds = 5,
  id = "regr.ranger.0"
  )

gr_level_0 <- gunion(
  list(
    treat_encode %>>% po_ridge_0,
    po_rf_0
    )
  )
gr_stack <- gr_level_0 %>>% 
  po("featureunion") %>>% 
  po(
    "learner",
    learner = lrn_lasso_1
    )
gr_stack_lrn <- as_learner(gr_stack)

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
              ml_l = gr_stack_lrn,
              ml_m = gr_stack_lrn,
              n_folds = 5,
              n_rep = 3
              )
            par_grids <- list(
              "ml_l" = gr_stack_ps$param_set,
              "ml_m" = gr_stack_ps$param_set
              )
            tune_sets <- list(
              terminator = trm(
                "evals",
                n_evals = 5
                ),
              algorithm = tnr(
                "random_search",
                batch_size = 5
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
            fit <- spec$fit(store_predictions = TRUE, store_models = TRUE)
            toc(log = TRUE)
            imp_dml_fits_2fe_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_2fe_gen <- tic.log()

