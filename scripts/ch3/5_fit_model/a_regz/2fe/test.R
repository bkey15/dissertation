# Fit ridge models using double machine learning (DML). Includes tuning for the ridge lambda parameter via v-fold cross-validation.

# load packages ----
library(here)
library(reticulate)
library(parallel)
library(rlang)
library(tictoc)

# set seed ----
## important: set seed before running prep script
py_set_seed(15275)

# run prep script ----
source(here("scripts/ch3/4_prep_model/a_regz/2fe/test.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
jlb$parallel_backend("threading", n_jobs = int(n))

# enable metadata routing ----
skl$set_config(enable_metadata_routing = TRUE)

# prep learners ----
## note: standardizing covariates to replicate glmnet's auto preprocessing
group_rsmp <- skl$model_selection$GroupKFold(
  n_splits = int(5),
  shuffle = TRUE
  )
alphas_spec <- np$logspace(int(-8), int(8), int(1000))

ml_tune_spec <- skl$pipeline$make_pipeline(
  skl$preprocessing$StandardScaler(),
  skl$linear_model$RidgeCV(
    alphas = alphas_spec,
    cv = group_rsmp,
    scoring = "neg_mean_squared_error"
    )
  )

lrn_spec <- skl$pipeline$make_pipeline(
  skl$preprocessing$StandardScaler(),
  skl$linear_model$Ridge()
  )

# fit models ----
## get initial specs ----
imp_dml_fits_2fe_regr <- list()
interact_stat <- names(imp_dml_dats_2fe)

## finalize ----
for(stat in interact_stat){
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
          spec <- dml$DoubleMLPLR(
            obj_dml_data = list_4[[i]],
            ml_l = lrn_spec,
            ml_m = lrn_spec,
            n_folds = int(5),
            n_rep = int(3)
            )
          
#          alphas_spec <- np$logspace(int(-4), int(4), int(100))*int(nrow(list_4[[i]]$data))
#          ml_tune_spec <- skl$pipeline$make_pipeline(
#            skl$preprocessing$StandardScaler(),
#            skl$linear_model$RidgeCV(
#              alphas = alphas_spec,
#              cv = group_rsmp
#              )
#            )
          
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
          
          ml_l_tune <- ml_tune_spec$fit(
            list_4[[i]]$x,
            list_4[[i]]$y,
            groups = list_4[[i]]$cluster_vars[,1]
            )
          ml_m_tune <- ml_tune_spec$fit(
            list_4[[i]]$x,
            list_4[[i]]$d,
            groups = list_4[[i]]$cluster_vars[,1]
            )
          
          spec$set_ml_nuisance_params("ml_l", as.character(treat), dict(ridge__alpha = ml_l_tune$named_steps["ridgecv"]$alpha_))
          spec$set_ml_nuisance_params("ml_m", as.character(treat), dict(ridge__alpha = ml_m_tune$named_steps["ridgecv"]$alpha_))
          
          fit <- spec$fit(store_models=T)
          toc(log = TRUE)
          imp_dml_fits_2fe_regr[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_2fe_regr <- tic.log()

# save fits ----
imp_dml_fits_2fe_regr |> 
  save(file = here("data/ch3/results/fits/dml_regularize/full_dat/imp_dml_fits_2fe_regr.rda"))
tictoc_imp_dml_fits_2fe_regr |> 
  save(file = here("data/ch3/results/fits/dml_regularize/runtimes/tictoc_imp_dml_fits_2fe_regr.rda"))
