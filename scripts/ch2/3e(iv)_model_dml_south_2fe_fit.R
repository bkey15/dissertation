# Fit lasso models using double machine learning (DML). Includes tuning for the lasso lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3learners)
library(parallel)
library(doMC)

# load data ----
load(here("data/ch2/results/fits/dml_lasso/dml_initial/imp_dml_dats_2fe_south.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# fit models ----
imp_dml_fits_2fe_south <- list()
interact_stat <- names(imp_dml_dats_2fe_south)
start_yrs <- names(imp_dml_dats_2fe_south[[1]])
m <- 1:length(imp_dml_dats_2fe_south[[1]][[1]][[1]])

for(stat in interact_stat){
  list_1 <- imp_dml_dats_2fe_south[[stat]]
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    treat_names <- names(list_2)
    for(name in treat_names){
      list_3 <- list_2[[name]]
      for(i in m){
        set.seed(15275)
        spec <- DoubleMLPLR$new(
          data = list_3[[i]],
          ml_l = lrn(
            "regr.cv_glmnet",
            s = "lambda.min",
            parallel = TRUE
            ),
          ml_m = lrn(
            "regr.cv_glmnet",
            s = "lambda.min",
            parallel = TRUE
            ),
          n_folds = 5,
          n_rep = 3
          )
        fit <- spec$fit(
          store_predictions = TRUE,
          store_models = TRUE
          )
        imp_dml_fits_2fe_south[[as.character(stat)]][[as.character(year)]][[as.character(name)]][[as.character(i)]] <- fit
    }
    }
  }
  }

# save fits ----
imp_dml_fits_2fe_south |> 
  save(file = here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_fits_2fe_south.rda"))
