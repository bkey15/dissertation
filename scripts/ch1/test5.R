# Fit tree models using double machine learning (DML). Includes tuning of hyperparameters via v-fold cross-validation.

# load packages ----
library(here)
library(parallel)
library(rlang)
library(tictoc)

# run prep script ----
source(here("scripts/ch1/test4.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
jlb$parallel_backend("threading", n_jobs = int(11))

# set seed ----
py_set_seed(15275)

# prep learners ----
ml_rf <- skl$ensemble$RandomForestRegressor(
  n_estimators = int(100),
  bootstrap = TRUE
  )
par_grids <- dict(
  ml_l = dict(
    max_features = np$arange(0.01, 1.01, 0.01),
    max_samples = np$arange(0.1, 1.01, 0.01)
    ),
  ml_m = dict(
    max_features = np$arange(0.01, 1.01, 0.01),
    max_samples = np$arange(0.1, 1.01, 0.01)
    )
  )

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
          dml_dat_i <- list_4[[i]]
          spec <- dml$DoubleMLPLR(
            obj_dml_data = dml_dat_i,
            ml_l = ml_rf,
            ml_m = ml_rf,
            n_folds = int(5),
            n_rep = int(3)
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
            param_grids = par_grids,
            search_mode = "randomized_search",
            n_iter_randomized_search = int(5)
            )
          fit <- spec$fit()
          
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
