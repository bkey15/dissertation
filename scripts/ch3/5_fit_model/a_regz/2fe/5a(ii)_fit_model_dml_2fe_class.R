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
source(here("scripts/ch3/4_prep_model/a_regz/2fe/4a(ii)_prep_model_dml_2fe_class.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
jlb$parallel_backend("threading", n_jobs = int(n))

# enable metadata routing ----
skl$set_config(enable_metadata_routing = TRUE)

# prep learners ----
## note: standardizing covariates to replicate glmnet's auto preprocessing
group_rsmp <- skl$model_selection$GroupShuffleSplit(n_splits = int(5))
reg_strength_spec <- np$logspace(int(-4), int(4), int(20))

ml_l_tune_spec <- skl$pipeline$make_pipeline(
  skl$preprocessing$StandardScaler(),
  skl$linear_model$RidgeCV(
    alphas = reg_strength_spec,
    cv = group_rsmp
    )
  )
ml_m_tune_class_spec <- skl$pipeline$make_pipeline(
  skl$preprocessing$StandardScaler(),
  skl$linear_model$LogisticRegressionCV(
    Cs = reg_strength_spec,
    cv = group_rsmp,
    penalty = "l2",
    scoring = "average_precision",
    solver = "newton-cholesky",
    max_iter = int(10e5)
    )
  )
ml_m_tune_regr_spec <- skl$pipeline$make_pipeline(
  skl$preprocessing$StandardScaler(),
  skl$linear_model$RidgeCV(
    alphas = reg_strength_spec,
    cv = group_rsmp
    )
  )

lrn_ridge_regr <- skl$linear_model$Ridge()
lrn_ridge_class <- skl$linear_model$LogisticRegression(
  penalty = "l2",
  solver = "newton-cholesky",
  max_iter = int(10e5)
  )

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
            spec <- dml$DoubleMLPLR(
              obj_dml_data = dml_dat_i,
              ml_l = lrn_ridge_regr,
              ml_m = lrn_ridge_class,
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
            
            ml_l_tune <- ml_l_tune_spec$fit(
              dml_dat_i$x,
              dml_dat_i$y,
              groups = dml_dat_i$cluster_vars[,1]
              )
            ml_m_tune <- ml_m_tune_class_spec$fit(
              dml_dat_i$x,
              dml_dat_i$d,
              groups = dml_dat_i$cluster_vars[,1]
              )
            
            spec$set_ml_nuisance_params("ml_l", as.character(treat), dict(alpha = ml_l_tune$named_steps["ridgecv"]$alpha_))
            spec$set_ml_nuisance_params("ml_m", as.character(treat), dict(C = ml_m_tune$named_steps["logisticregressioncv"]$C_[1]))
            
            fit <- spec$fit()
            toc(log = TRUE)
            imp_dml_fits_2fe_class[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
          }
          else{
            spec <- dml$DoubleMLPLR(
              obj_dml_data = dml_dat_i,
              ml_l = lrn_ridge_regr,
              ml_m = lrn_ridge_regr,
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
            
            ml_l_tune <- ml_l_tune_spec$fit(
              dml_dat_i$x,
              dml_dat_i$y,
              groups = dml_dat_i$cluster_vars[,1]
              )
            ml_m_tune <- ml_m_tune_regr_spec$fit(
              dml_dat_i$x,
              dml_dat_i$d,
              groups = dml_dat_i$cluster_vars[,1]
              )
            
            spec$set_ml_nuisance_params("ml_l", as.character(treat), dict(alpha = ml_l_tune$named_steps["ridgecv"]$alpha_))
            spec$set_ml_nuisance_params("ml_m", as.character(treat), dict(alpha = ml_m_tune$named_steps["ridgecv"]$alpha_))
            
            fit <- spec$fit()
            toc(log = TRUE)
            imp_dml_fits_2fe_class[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
          }
        }
      }
    }
  }
}

tictoc_imp_dml_fits_2fe_class <- tic.log()

# convert to python object ----
imp_dml_fits_2fe_class <- imp_dml_fits_2fe_class |> 
  r_to_py()

# save fits ----
## note: compressing w/ joblib & .gz ext.
imp_dml_fits_2fe_class |> 
  jlb$dump(filename = here("data/ch3/results/fits/dml_regularize/full_dat/imp_dml_fits_2fe_class.gz"), compress = int(3))
tictoc_imp_dml_fits_2fe_class |> 
  save(file = here("data/ch3/results/fits/dml_regularize/runtimes/tictoc_imp_dml_fits_2fe_class.rda"))
