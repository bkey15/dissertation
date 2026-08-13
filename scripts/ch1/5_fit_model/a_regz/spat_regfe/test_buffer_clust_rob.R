# Fit ridge models using double machine learning (DML). Includes tuning for the ridge lambda parameter via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(tidymodels)
library(spatialsample)
library(sf)
#library(mlr3spatiotempcv)
library(parallel)
library(future)
library(tictoc)

# set seed ----
## important: set seed before running prep script
set.seed(15275)

# run prep script ----
# note: using original 2fe data for now to test with country splits
# note: sf::st_union() may be used to create region polygons
#source(here("scripts/ch1/4_prep_model/a_regz/spat_regfe/4a(iv)_prep_regz_model_dml_spat_regfe_gen.R"))
source(here("scripts/ch1/4_prep_model/a_regz/2fe/4a(i)_prep_regz_model_dml_2fe_gen.R"))
load(here("data/ch1/preprocessed/world_base.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# prep learners ----
lrn_spec <- lrn(
  "regr.glmnet",
  parallel_predict = TRUE
)
default_s <- lts(
  lrn("regr.glmnet")
)$param_set$values$s$call
ps_spec <- lrn(
  "regr.glmnet",
  alpha = 0,
  nlambda = 1,
  s = eval(parse(text = default_s))
)$param_set

# set folds & reps ----
nf <- 10
nr <- 3

# make buffered + 2-way cluster-robust splits ----
#spatial_buffer_vfold_cv(v = 5, radius = NULL, buffer = 2000)
#purrr::walk(test_splits_cow$splits, function(x) print(autoplot(x)))
world_base <- world_base |> 
  mutate(cow = as.factor(cow))

interact_stat <- names(imp_dml_dats_2fe_gen)
splits_cow_all <- list()
splits_yr_all <- list()

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
          df <- list_4[[i]]$data |> 
            mutate(
              row_id = 1:nrow(list_4[[i]]$data),
              cow_yr = paste(cow, year, sep = "-")
              ) |> 
            select(row_id, cow, year, cow_yr)
          
          cow_df <- df$cow |> 
            unique()
          cow_df <- cow_df |> 
            tibble()
          cow_filter <- world_base |> 
            filter(cow %in% cow_df$cow_df)
          splits_cow <- cow_filter |> 
            spatial_buffer_vfold_cv(
              v = nf,
              repeats = nr+1,
              radius = NULL,
              buffer = 2000
              )
          
          yr_df <- df$year |> 
            unique()
          yr_df <- yr_df |> 
            tibble() |> 
            rename(year = yr_df)
          splits_yr <- yr_df |> 
            vfold_cv(
              v = nf,
              repeats = nr+1
              )
          
          rep_names <- splits_cow$id |> 
            unique()
          for(rep in rep_names){
            splits_cow_rep <- splits_cow |> 
              filter(id == rep)
            splits_yr_rep <- splits_yr |> 
              filter(id == rep)
            
            nf_seq <- 1:nrow(splits_cow_rep)
            for(fold in nf_seq){
              test_splits_cow <- splits_cow_rep$splits[[fold]] |> 
                assessment() |> 
                st_drop_geometry()
              train_splits_cow <- splits_cow_rep$splits[[fold]] |> 
                analysis() |> 
                st_drop_geometry()
              
              test_splits_yr <- splits_yr_rep$splits[[fold]] |> 
                assessment()
              
              splits_cow_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(fold)]] <- list(test = test_splits_cow, train = train_splits_cow)
              splits_yr_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(fold)]] <- list(test = test_splits_yr)
            }
          }
        }
      }
    }
  }
}

# note: cross-join not only cow test sets, but also cow training sets, with year (test) splits. therefore, can build final training sets that share original training-set ids but that can also exclude years in test set. this will help preserve spatial buffer.

# fit models ----
imp_dml_fits_spat_regfe_gen <- list()
interact_stat <- names(imp_dml_dats_spat_regfe_gen)

for(stat in interact_stat){
  list_1 <- imp_dml_dats_spat_regfe_gen[[stat]]
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
            ml_l = lrn_spec,
            ml_m = lrn_spec,
            n_folds = nf,
            n_rep = nr
          )
          
          rsmp_task <- as_task_regr(
            list_4[[i]]$data,
            target = "hr_score"
          )
          rsmp_task$set_col_roles(
            cols = "region",
            roles = "group"
          )
          rsmp_set <- rsmp(
            "cv",
            folds = 5
          )
          rsmp_set$instantiate(rsmp_task)
          
          tune_sets <- list(
            terminator = trm(
              "none"
            ),
            algorithm = tnr(
              "grid_search",
              resolution = 20,
              batch_size = 20
            ),
            rsmp_tune = rsmp_set,
            measure = list(
              "ml_l" = msr("regr.mse"),
              "ml_m" = msr("regr.mse")
            )
          )
          par_grids <- list(
            "ml_l" = ps_spec,
            "ml_m" = ps_spec
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
          fit <- spec$fit(store_predictions = TRUE)
          toc(log = TRUE)
          imp_dml_fits_spat_regfe_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_spat_regfe_gen <- tic.log()
