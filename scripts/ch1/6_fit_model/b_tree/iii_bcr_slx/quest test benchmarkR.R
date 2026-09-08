# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(parallel)
library(future)
library(tictoc)

# set seed ----
## note: not needed at the moment since splitting occurs prior & search strategy for tuning grid is non-random; but could be useful if one decided to use a random search
set.seed(15275)

# run prep script ----
source(here("scripts/ch1/4_prep_model/a_regz/ii_slx/4a(iv)_prep_regz_model_dml_slx_gen.R"))

# load splits ----
load(here("data/ch1/results/splits/bcr_slx/splits_imp_dml_dats_bcr_slx_gen.rda"))

ps_spec <- lrn(
  "regr.xgboost",
  eta = to_tune(0.01, 0.2, logscale = TRUE),
  nrounds = to_tune(300, 900),
  max_depth = to_tune(1, 3),
  min_child_weight = to_tune(0.5, 20, logscale = TRUE),
  subsample = to_tune(0.6, 1),
  colsample_bytree = to_tune(0.6, 1),
  lambda = to_tune(0.1, 10, logscale = TRUE)
)$param_set

par_grids <- list(
  ml_l = ps_spec,
  ml_m = ps_spec
)

# n_resolution <- 10
# n_resolution <- 20
n_resolution <- 12

run_xgb_benchmark <- function(n_workers, xgb_threads) {
  
  # Outer parallelization
  if (n_workers == 1) {
    plan(sequential)
  } else {
    plan(multisession, workers = n_workers)
  }
  
  # XGBoost learner used by DoubleML
  lrn_spec <- lrn(
    "regr.xgboost",
    nthread = xgb_threads,
    booster = "gbtree",
    tree_method = "hist",
    colsample_bylevel = 1,
    alpha = 1
  )
  
  # Fresh DoubleML object
  spec <- DoubleMLPLR$new(
    data = imp_dml_dats_slx_gen$
      no_interactions$start_1977$l1$lech_hr_mean$`1`,
    ml_l = lrn_spec,
    ml_m = lrn_spec
  )
  
  # DML sample splitting
  smpls <- splits_imp_dml_dats_bcr_slx_gen$
    no_interactions$start_1977$l1$lech_hr_mean$`1`[-1]
  
  spec$set_sample_splitting(smpls)
  
  # Task used for tuning
  rsmp_task <- as_task_regr(
    imp_dml_dats_slx_gen$
      no_interactions$start_1977$l1$lech_hr_mean$`1`$data_model,
    target = "hr_score"
  )
  
  # Custom tuning resampling
  rsmp_set <- rsmp("custom")
  
  rsmp_set$instantiate(
    rsmp_task,
    train_sets =
      splits_imp_dml_dats_bcr_slx_gen$
      no_interactions$start_1977$l1$lech_hr_mean$`1`[[1]]$train_ids,
    test_sets =
      splits_imp_dml_dats_bcr_slx_gen$
      no_interactions$start_1977$l1$lech_hr_mean$`1`[[1]]$test_ids
  )
  
  # Tuning settings
  # tune_sets <- list(
  #   terminator = trm(
  #     "evals",
  #     n_evals = n_resolution
  #   ),
  #   algorithm = tnr("mbo"),
  #   rsmp_tune = rsmp_set,
  #   measure = list(
  #     ml_l = msr("regr.mse"),
  #     ml_m = msr("regr.mse")
  #   )
  # )
#   tune_sets <- list(
#   terminator = trm(
#     "evals",
#     n_evals = n_resolution
#   ),
#   algorithm = tnr(
#     "mbo",
#     args = list(init_design_size = 4)
#   ),
#   rsmp_tune = rsmp_set,
#   measure = list(
#     ml_l = msr("regr.mse"),
#     ml_m = msr("regr.mse")
#   )
# )
  tune_sets <- list(
    terminator = trm(
      "evals",
      n_evals = n_resolution
    ),
    algorithm = tnr(
      "random_search",
      batch_size = n_resolution
      ),
    rsmp_tune = rsmp_set,
    measure = list(
      ml_l = msr("regr.mse"),
      ml_m = msr("regr.mse")
    )
  )
  
  # Time tuning
  tune_time <- system.time({
    spec$tune(
      param_set = par_grids,
      tune_settings = tune_sets
    )
  })
  
  # Time final fit
  fit_time <- system.time({
    fit <- spec$fit(store_predictions = TRUE)
  })
  
  # Clean up workers before returning
  plan(sequential)
  
  list(
    workers = n_workers,
    xgb_threads = xgb_threads,
    tune_elapsed = as.numeric(tune_time["elapsed"]),
    fit_elapsed = as.numeric(fit_time["elapsed"]),
    total_elapsed =
      as.numeric(tune_time["elapsed"]) +
      as.numeric(fit_time["elapsed"]),
    spec = spec,
    fit = fit
  )
}

# configs <- list(
#   c(workers = 12, threads = 1),
#   c(workers = 6,  threads = 2),
#   c(workers = 4,  threads = 3),
#   c(workers = 3,  threads = 4)
# )

# configs <- list(c(workers = 4, threads = 6))
# configs <- list(c(workers = 6, threads = 4))
# configs <- list(c(workers = 12, threads = 2))
# configs <- list(c(workers = 24, threads = 1))

# configs <- list(c(workers = 48, threads = 1))
# configs <- list(c(workers = 24, threads = 2))
# configs <- list(c(workers = 12, threads = 4))
configs <- list(c(workers = 12, threads = 1))
configs <- list(c(workers = 24, threads = 1))

lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

results <- lapply(
  configs,
  function(x) {
    run_xgb_benchmark(
      n_workers = x[["workers"]],
      xgb_threads = x[["threads"]]
    )
  }
)

timings <- do.call(
  rbind,
  lapply(results, function(x) {
    data.frame(
      workers = x$workers,
      xgb_threads = x$xgb_threads,
      tune = x$tune_elapsed,
      fit = x$fit_elapsed,
      total = x$total_elapsed
    )
  })
)

timings |> 
  save(file = here("scripts/ch1/6_fit_model/b_tree/iii_bcr_slx/timings.rda"))
