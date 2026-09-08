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
# source(here("scripts/ch1/4_prep_model/b_tree/ii_slx/4b(iv)_prep_tree_model_dml_slx_gen.R"))
source(here("scripts/ch1/4_prep_model/a_regz/ii_slx/4a(iv)_prep_regz_model_dml_slx_gen.R"))

# load splits ----
load(here("data/ch1/results/splits/bcr_slx/splits_imp_dml_dats_bcr_slx_gen.rda"))

# set cores ----
## check
n <- detectCores()

## set
# plan(multisession, workers = 5)
plan(multisession, workers = n)

# prep learners ----
lrn_spec_lasso <- lrn_spec <- lrn("regr.glmnet")
lrn_spec_tree <- lrn(
  "regr.xgboost",
  nthread = xgb_threads,
  booster = "gbtree",
  tree_method = "hist",
  colsample_bylevel = 1,
  alpha = 1
  )

ps_spec <- ps(
  regr.xgboost.eta = p_dbl(0.01, 0.2, logscale = TRUE),
  regr.xgboost.nrounds = p_dbl(300, 900),
  regr.xgboost.max_depth = p_dbl(1, 3),
  regr.xgboost.min_child_weight = p_dbl(0.5, 20, logscale = TRUE),
  regr.xgboost.subsample = p_dbl(0.6, 1),
  regr.xgboost.colsample_bytree = p_dbl(0.6, 1),
  regr.xgboost.lambda = p_dbl(0.1, 10, logscale = TRUE)
  )

