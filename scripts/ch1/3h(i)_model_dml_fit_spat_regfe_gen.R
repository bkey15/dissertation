# Fit lasso models using double machine learning (DML). Includes tuning for the lasso lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3learners)
library(parallel)
library(doMC)

# load data ----
load(here("data/ch1/results/fits/dml_lasso/dml_initial/imp_dml_dats_spat_regfe_gen.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# fit models ----
imp_dml_fits_spat_regfe_gen <- list()
start_yrs <- names(imp_dml_dats_spat_regfe_gen)
treat_names <- names(imp_dml_dats_spat_regfe_gen[[1]])
m <- 1:length(imp_dml_dats_spat_regfe_gen[[1]][[1]])

for(year in start_yrs){
  list_1 <- imp_dml_dats_spat_regfe_gen[[year]]
  for(name in treat_names){
    list_2 <- list_1[[name]]
    for(i in m){
      set.seed(15275)
      spec <- DoubleMLPLR$new(
        data = list_2[[i]],
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
      fit <- spec$fit()
      imp_dml_fits_spat_regfe_gen[[as.character(year)]][[as.character(name)]][[as.character(i)]] <- fit
    }
  }
}

# save fits ----
imp_dml_fits_spat_regfe_gen |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_dml_fits_spat_regfe_gen.rda"))
