# Fit lasso models using double machine learning (DML). Includes tuning for the lasso lambda parameter via v-fold cross-validation.

# load packages ----
library(here)
library(parallel)
library(rlang)

# run prep script ----
source(here("scripts/ch3/4b_prep_model_dml_spat_regfe.R"))

# set cores ----
## check
n <- detectCores() - 1

# fit models ----
## get initial specs ----
imp_dml_fits_spat_regfe <- list()
treat_num_stat <- names(imp_dml_dats_spat_regfe)

ml_l <- skl$linear_model$LassoCV(n_jobs = int(n))
ml_m_regr <- skl$linear_model$LassoCV(n_jobs = int(n))
ml_m_classif <- skl$linear_model$LogisticRegressionCV(
  penalty = "l1",
  solver = "liblinear",
  n_jobs = int(n),
  max_iter = int(1000)
  )

## finalize ----
for(stat in treat_num_stat){
  list_1 <- imp_dml_dats_spat_regfe[[stat]]
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
          if(treat == "any_inforce1"){
            py_set_seed(15275)
            spec <- dml$DoubleMLPLR(
              obj_dml_data = dml_dat_i,
              ml_l = ml_l,
              ml_m = ml_m_classif,
              n_folds = int(10),
              n_rep = int(5)
              )
            fit <- spec$fit()
            imp_dml_fits_spat_regfe[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
          }
          else{
            py_set_seed(15275)
            spec <- dml$DoubleMLPLR(
              obj_dml_data = dml_dat_i,
              ml_l = ml_l,
              ml_m = ml_m_regr,
              n_folds = int(10),
              n_rep = int(5)
              )
            fit <- spec$fit()
            imp_dml_fits_spat_regfe[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
          }
        }
      }
    }
  }
}

# convert to python object ----
imp_dml_fits_spat_regfe <- imp_dml_fits_spat_regfe |> 
  r_to_py()

# save fits ----
## note: compressing w/ joblib & .gz ext.
imp_dml_fits_spat_regfe |> 
  jlb$dump(filename = here("data/ch3/results/fits/dml_lasso/full_dat/imp_dml_fits_spat_regfe.gz"), compress = int(3))
