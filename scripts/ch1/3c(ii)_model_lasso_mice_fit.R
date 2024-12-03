# Fit lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(tools)
library(mice)
library(glmnet)
library(selectiveInference)
library(hdi)
library(parallel)

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
lambdas <- dir("data/ch1/results/tunes", pattern = "\\lams.rda$", full.names = TRUE)

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
## regular ----
coef(imp_1_cv_res[["both_gdppc_mean"]][[1]], s = "lambda.min")
coef(imp_2_cv_res[["both_gdppc_mean"]][[1]], s = "lambda.min")
coef(imp_3_cv_res[["both_gdppc_mean"]][[1]], s = "lambda.min")

fit <- glmnet(
  x = imp_1_covars[["both_gdppc_mean"]][[1]],
  y = dep_list[["dep_1"]],
  lambda = imp_1_lams[["both_gdppc_mean"]][[1]],
  thresh = 1e-11
  )

## get betas ----
beta1 <- coef.glmnet(
  object = fit,
  x = imp_1_covars[["both_gdppc_mean"]][[1]],
  y = dep_list[[1]],
  s = imp_1_lams[["both_gdppc_mean"]][[1]]/length(dep_list[[1]]),
  exact = TRUE
  )[-1]

beta2 <- coef.glmnet(
  object = imp_2_cv_res[["both_gdppc_mean"]][[1]],
  s = imp_2_lams[["both_gdppc_mean"]][[1]]/length(dep_list[[1]]))[-1]

## infer ----
test <- fixedLassoInf(
  x = imp_1_covars[["both_gdppc_mean"]][[1]],
  y = dep_list[["dep_1"]],
  beta = beta1,
  lambda = imp_1_lams[["both_gdppc_mean"]][[1]]
  )
fixedLassoInf(
  x = imp_2_covars[["both_gdppc_mean"]][[1]],
  y = dep_list[["dep_2"]],
  beta = beta2,
  lambda = imp_2_lams[["both_gdppc_mean"]][[1]]
  )


n <- detectCores() - 1
infer <- lasso.proj(
  x = imp_1_covars[["both_gdppc_mean"]][[1]],
  y = dep_list[[1]],
  ncores = n
)

