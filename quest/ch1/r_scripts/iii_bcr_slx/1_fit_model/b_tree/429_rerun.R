# Fit rf models using double machine learning (DML). Includes tuning of hyperparameters via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(parallel)
library(future)
library(tictoc)

# set seed ----
## important: needed for random search of hyperparameter configs
set.seed(15275 + 429)

# load data ----
load(here("quest/429.rda"))

dml_data <- model_info$data
stat <- model_info$stat
year <- model_info$year
lag <- model_info$lag
treat <- model_info$treat
i <- model_info$i

# load splits ----
load(here("data/ch1/results/splits/bcr_slx/splits_imp_dml_dats_bcr_slx_south.rda"))

splits <- splits_imp_dml_dats_bcr_slx_south[[stat]][[year]][[lag]][[treat]][[i]]

# set cores ----
## check
n <- detectCores()

## set
plan(multisession, workers = n)

# prep learners ----
lrn_spec <- lrn(
  "regr.ranger",
  num.threads = 1
  )
default_mtry.ratio <- lts(
  lrn("regr.ranger")
  )$param_set$values$mtry.ratio$call
default_replace <- lts(
  lrn("regr.ranger")
  )$param_set$values$replace$call

ps_spec <- lrn(
  "regr.ranger",
  mtry.ratio = eval(parse(text = default_mtry.ratio)),
  replace = eval(parse(text = default_replace)),
  num.trees = 500,
  sample.fraction = to_tune(p_dbl(0.1, 0.6))
  )$param_set

n_resolution <- 12

# fit models ----
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

spec <- DoubleMLPLR$new(
  data = dml_data,
  ml_l = lrn_spec,
  ml_m = lrn_spec
  )

smpls <- splits[-1]
spec$set_sample_splitting(smpls)
          
rsmp_task <- as_task_regr(
  dml_data$data_model,
  target = "hr_score"
  )
rsmp_set <- rsmp("custom")
rsmp_set$instantiate(rsmp_task, train_sets = splits[[1]]$train_ids, test_sets = splits[[1]]$test_ids)

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

tictoc_fit <- tic.log()

# save fits ----
fit_metadata <- list(
  task_id = 429L,
  stat = stat,
  year = year,
  lag = lag,
  treat = treat,
  i = i
  )

save(
  fit,
  fit_metadata,
  file = here("quest/429_fit/full_dat/429.rda")
  )
save(
  tictoc_fit,
  fit_metadata,
  file = here("quest/429_fit/runtime/429.rda")
  )
