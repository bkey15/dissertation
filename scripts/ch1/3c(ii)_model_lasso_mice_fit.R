# Fit lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(tools)
library(mice)
library(glmnet)

# load data ----
## hr_score ----
load(here("data/ch1/results/imputations/dep_1.rda"))
load(here("data/ch1/results/imputations/dep_2.rda"))
load(here("data/ch1/results/imputations/dep_3.rda"))

## covars ----
covars <- dir("data/ch1/results/imputations", pattern = "covars", full.names = TRUE)

for(file in covars){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}

## lambdas ----
lambdas <- dir("data/ch1/results/tunes", full.names = TRUE)

for(file in lambdas){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}


