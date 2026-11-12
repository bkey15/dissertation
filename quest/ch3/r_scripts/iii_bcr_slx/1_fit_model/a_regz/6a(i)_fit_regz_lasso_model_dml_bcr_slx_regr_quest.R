# Fit lasso models using double machine learning (DML). Includes tuning of hyperparameters via v-fold cross-validation.

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mlr3verse)
library(parallel)
library(future)
library(tictoc)

# get task id ----
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

if (is.na(task_id) || task_id < 1 || task_id > 40) {
  stop("Invalid SLURM_ARRAY_TASK_ID: ", task_id)
}

# load data ----
load(
  file = here(
    "data/ch3/dml_dat/imp_dml_dats_slx_regr",
    sprintf("%03d.rda", task_id)
    )
  )

dml_data <- model_info$data
stat <- model_info$stat
year <- model_info$year
lag <- model_info$lag
treat <- model_info$treat
i <- model_info$i

## check task ID
stopifnot(model_info$task_id == task_id)

# load splits ----
load(here("data/ch3/splits/splits_imp_dml_dats_bcr_slx_regr.rda"))

splits <- splits_imp_dml_dats_bcr_slx_regr[[stat]][[year]][[lag]][[treat]][[i]]

# set cores ----
## check
n <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))

## set
plan(multisession, workers = n)

# prep learners ----
lrn_spec <- lrn("regr.glmnet")
default_s <- lts(
  lrn("regr.glmnet")
  )$param_set$values$s$call
ps_spec <- lrn(
  "regr.glmnet",
  nlambda = 1,
  s = eval(parse(text = default_s))
  )$param_set
n_resolution <- 10

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
  terminator = trm("none"),
  algorithm = tnr(
    "grid_search",
    resolution = n_resolution,
    batch_size = 1
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
  task_id = task_id,
  stat = stat,
  year = year,
  lag = lag,
  treat = treat,
  i = i
  )

save(
  fit,
  fit_metadata,
  file = here(
    "data/ch3/fits/dml_regz/full_dat/imp_dml_fits_bcr_slx_regr_regz_lasso",
    sprintf("%03d.rda", task_id)
    )
  )
save(
  tictoc_fit,
  fit_metadata,
  file = here(
    "data/ch3/fits/dml_regz/runtimes/imp_dml_fits_bcr_slx_regr_regz_lasso",
    sprintf("%03d.rda", task_id)
    )
  )
