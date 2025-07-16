# Fit lasso models using double machine learning (DML). Includes tuning for the lasso lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3learners)
library(parallel)
library(doMC)

# load data ----
load(here("data/ch3/results/fits/dml_lasso/dml_initial/imp_dml_dats_2fe.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# fit models ----
imp_dml_fits_2fe <- list()
start_yrs <- names(imp_dml_dats_2fe)
lag_names <- names(imp_dml_dats_2fe[[1]])
treat_names <- names(imp_dml_dats_2fe[[1]][[1]])
m <- 1:length(imp_dml_dats_2fe[[1]][[1]][[1]])

for(year in start_yrs){
  list_1 <- imp_dml_dats_2fe[[year]]
  for(lag in lag_names){
    list2 <- list1[[lag]]
    for(treat in treat_names){
      list_3 <- list_2[[treat]]
      for(i in m){
        set.seed(15275)
        spec <- DoubleMLPLR$new(
          data = list_3[[i]],
          ml_l = lrn(
            "regr.cv_glmnet",
            s = "lambda.min",
            parallel = TRUE,
            parallel_predict = TRUE
            ),
          ml_m = lrn(
            "regr.cv_glmnet",
            s = "lambda.min",
            parallel = TRUE,
            parallel_predict = TRUE
            ),
          n_folds = 5,
          n_rep = 3
          )
        fit <- spec$fit()
        imp_dml_fits_2fe[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
      }
    }
  }
  }

# save fits ----
imp_dml_fits_2fe |> 
  save(file = here("data/ch3/results/fits/dml_lasso/dml_final/imp_dml_fits_2fe.rda"))
