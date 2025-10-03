# Fit ridge models using double machine learning (DML). Includes tuning for the ridge lambda parameter via v-fold cross-validation.

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
source(here("scripts/ch1/4_prep_model/a_regz/spat_regfe/4a(iv)_prep_regz_model_dml_spat_regfe_gen.R"))

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
#ps_spec <- lrn(
#  "regr.glmnet",
#  alpha = 0,
#  nlambda = 1,
#  s = eval(parse(text = default_s))
#  )$param_set

ps_spec <- ps(
  regr.glmnet.alpha = p_dbl(0, 1),
  regr.glmnet.nlambda = p_dbl(1),
  regr.glmnet.s = p_dbl(0)
  )
ps_spec$values <- list(
  regr.glmnet.alpha = 0,
  regr.glmnet.nlambda = 1,
  regr.glmnet.s = eval(parse(text = default_s))
  )

# prep pipe ----
PipeOpGroupSet <- R6::R6Class(
  "PipeOpGroupSet",
  inherit =  mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "group_set"){
      super$initialize(id = id)
    }
  ),
  private = list(
    .transform = function(task) {
      task$set_col_roles(
        cols = "dup_region",
        roles = "group"
        )
      task
      }
    )
  )

graph <- PipeOpGroupSet$new() %>>% 
  po(
    "select",
    selector = selector_grep(
      "^(?!dup_).*",
      perl = TRUE
      )
    ) %>>% 
  po("learner", lrn_spec)

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
            ml_l = as_learner(graph),
            ml_m = as_learner(graph),
            n_folds = 5,
            n_rep = 3
            )
          
          #rsmp_task <- as_task_regr(
            #list_4[[i]]$data,
            #target = "hr_score"
            #)
          #rsmp_task$set_col_roles(
            #cols = "region",
            #roles = "group"
            #)
          rsmp_set <- rsmp(
            "cv",
            folds = 5
            )
          #rsmp_set$instantiate(rsmp_task)
          
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
          fit <- spec$fit(
            store_predictions = TRUE,
            store_models = TRUE
            )
          toc(log = TRUE)
          imp_dml_fits_spat_regfe_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
        }
      }
    }
  }
}

tictoc_imp_dml_fits_spat_regfe_gen <- tic.log()

# save fits ----
imp_dml_fits_spat_regfe_gen |> 
  save(file = here("data/ch1/results/fits/dml_regularize/full_dat/imp_dml_fits_spat_regfe_gen.rda"))
tictoc_imp_dml_fits_spat_regfe_gen |> 
  save(file = here("data/ch1/results/fits/dml_regularize/runtimes/tictoc_imp_dml_fits_spat_regfe_gen.rda"))
