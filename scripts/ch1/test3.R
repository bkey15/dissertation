# Fit ridge models using double machine learning (DML). Includes tuning for the ridge lambda parameter via v-fold cross-validation.

# load packages ----
library(here)
library(parallel)
library(rlang)
library(tictoc)

# run prep script ----
source(here("scripts/ch1/test2.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
jlb$parallel_backend("threading", n_jobs = int(11))

# set seed ----
py_set_seed(15275)

# prep learners ----
ml_ridge <- skl$linear_model$Ridge()

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
              
            ml_l_tune <- skl$linear_model$RidgeCV(
              alphas = np$logspace(int(-6), int(4), int(50)),
              cv = int(10),
              )$fit(dml_dat_i$x, dml_dat_i$y)
            ml_m_tune <- skl$linear_model$RidgeCV(
              alphas = np$logspace(int(-6), int(4), int(50)),
              cv = int(10),
              )$fit(dml_dat_i$x, dml_dat_i$d)
            
            spec <- dml$DoubleMLPLR(
              obj_dml_data = dml_dat_i,
              ml_l = ml_ridge,
              ml_m = ml_ridge,
              n_folds = int(10),
              n_rep = int(5)
              )
            spec$set_ml_nuisance_params("ml_l", as.character(treat), dict(alpha = ml_l_tune$alpha_))
            spec$set_ml_nuisance_params("ml_m", as.character(treat), dict(alpha = ml_m_tune$alpha_))
            
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
  save(file = here("data/ch1/results/fits/dml_regularize/full_dat/imp_dml_fits_2fe_gen.rda"))
tictoc_imp_dml_fits_2fe_gen |> 
  save(file = here("data/ch1/results/fits/dml_regularize/runtimes/tictoc_imp_dml_fits_2fe_gen.rda"))
