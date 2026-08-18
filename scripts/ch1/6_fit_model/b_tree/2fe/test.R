# Fit tree models using double machine learning (DML). Includes tuning of hyperparameters via v-fold cross-validation.

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
source(here("scripts/ch1/4_prep_model/b_tree/2fe/test.R"))

# set cores ----
## check
n <- detectCores() - 1

## set
plan(strategy = "multisession", workers = n)

# prep learners ----
lrn_spec <- lrn(
  "regr.ranger",
  parallel_predict = TRUE
  )

# prep pipe ----
PipeOpResid <- R6::R6Class(
  "PipeOpResid",
  inherit =  mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "residualize"){
      super$initialize(
        id = id,
        packages = "fixest"
        )
      }
    ),
  private = list(
    .train_task = function(task){
      fe_names <- c("dup_cow", "dup_year")
      covar_names <- task$feature_names[!task$feature_names %in% c(fe_names, "year")]
      out_name <- task$target_names
      data <- task$data(
        cols = c(
          fe_names,
          covar_names,
          out_name
          )
        )
      dm_train <- fixest::demean(
        X = data[, c(out_name, covar_names), with = FALSE],
        f = data[, fe_names, with = FALSE]
        )
      self$state <- list(
        out_name = out_name,
        covar_names  = covar_names,
        fe_names = fe_names,
        resid_data = dm_train
        )
      task_new <- task$clone()$cbind(dm_train)
      task_new
      },
    .predict_task = function(task){
      data <- task$data(
        cols = c(
          self$state$out_name,
          self$state$covar_names,
          self$state$fe_names
          )
        )
      dm_test <- fixest::demean(
        data[, c(self$state$out_name, self$state$covar_names), with = FALSE],
        data[, self$state$fe_names, with = FALSE]
        )
      task_new <- task$clone()$cbind(dm_test)
      task_new
      }
    )
  )

graph <- PipeOpResid$new() %>>% 
  po(
    "select",
    selector = selector_grep(
      "^(?!dup_).*",
      perl = TRUE
      )
    ) %>>% 
  po("learner", lrn_spec)

ps_spec <- lrn(
  "regr.ranger",
  num.threads = n,
  num.trees = 50,
  mtry.ratio = to_tune(0.25, 0.75),
  replace = FALSE,
  sample.fraction = 0.632
  )$param_set

ps_spec <- ps(
  regr.ranger.num.trees = p_int(50, 50),
  regr.ranger.mtry.ratio = p_dbl(0.25, 0.75),
  regr.ranger.replace = p_lgl(default = FALSE),
  regr.ranger.sample.fraction = p_dbl(0.25, 0.75)
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
          spec <- DoubleMLPLR$new(
            data = list_4[[i]],
            ml_l = as_learner(graph, store_backends = T),
            ml_m = as_learner(graph, store_backends = T),
            n_folds = 5,
            n_rep = 3
            )
          
          rsmp_task <- as_task_regr(
            list_4[[i]]$data,
            target = "hr_score"
            )
          rsmp_task$set_col_roles(
            cols = "cow",
            roles = "group"
            )
          rsmp_set <- rsmp(
            "cv",
            folds = 5
            )
          rsmp_set$instantiate(rsmp_task)
          
          tune_sets <- list(
            terminator = trm(
              "evals",
              n_evals = 10
              ),
            algorithm = tnr(
              "random_search",
              batch_size = 10
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
          fit <- spec$fit(store_predictions = TRUE, store_models = TRUE)
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
