# Fit lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(tools)
library(mice)
library(glmnet)
library(selectiveInference)
library(hdi)

# load data ----
## hr_score ----
load(here("data/ch1/results/imputations/dep_list.rda"))

## covars ----
covars <- dir("data/ch1/results/imputations", pattern = "covars", full.names = TRUE)

for(file in covars){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}

## lambdas ----
lambdas <- dir("data/ch1/results/tunes", pattern = "lams_mean", full.names = TRUE)

for(file in lambdas){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}

## cv results ----
cvs <- dir("data/ch1/results/tunes", pattern = "cv_res", full.names = TRUE)

for(file in cvs){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}

# test lasso ----
## get betas ----
beta <- coef.glmnet(
  object = imp_1_cv_res[["both_gdppc_mean"]][[1]],
  s = imp_2_lams_mean[["both_gdppc_mean"]]/length(dep_list[[3]]))[-1]

infer <- fixedLassoInf(
  x = imp_2_covars[["both_gdppc_mean"]][[1]],
  y = dep_list[[2]],
  beta = beta2,
  lambda = imp_2_lams_mean[["both_gdppc_mean"]]
  )
