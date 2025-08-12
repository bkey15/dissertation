# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch3/results/imputations/imp_1990_t_lags.rda"))

# get imputed data ----
## 1990 ----
m <- 1:imp_1990_t_lags[[1]]$m
lag_names <- names(imp_1990_t_lags)
imp_1990_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1990_t_lags[[lag]]
  for(i in m){
    imp_df <- imp_dat |> 
      mice::complete(
        action = "long",
        include = TRUE
        ) |> 
      filter(.imp == i) |> 
      select(
        -last_col(),
        -last_col(offset = 1)
        )
  
    imp_1990_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
    }
}

## combine ----
## note: code below is drawn from earlier chapters; can be used in the event other start-dates are utilized
imp_dfs_all <- list(
  start_1990 = imp_1990_dfs
  )

# get main specs ----
## treat names ----
treat_names <- c("n_ems", "any_inforce1")

## interact names ----
interact_names <- imp_1990_dfs[[1]][[1]] |> 
  select(contains("_x_")) |> 
  names()

## covar names ----
## important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lasso
## important: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
### 1990 ----
covar_names_1990 <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1990_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -1,
      -contains(
        c(
          "n_ems",
          "any_inforce",
          "hr_score"
          )
        )
      )|> 
    names()
  
  covar_names_1990[[as.character(lag)]] <- covar_names
}

### combine ----
covar_names_all <- list(
  start_1990 = covar_names_1990
  )

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()
start_yrs <- imp_dfs_all |> 
  names()

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_dfs_all[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(treat in treat_names){
        no_interactions[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = covar_names,
            d_cols = treat,
            y_col = "hr_score"
            )
      }
    }
  }
}

### check for zero variance ----
zerovar_1990 <- caret::nearZeroVar(
  no_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

## has interactions ----
### get initial specs ----
has_interactions <- list()

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_dfs_all[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names)){
        k <- treat_names[[j]]
        l <- interact_names[[j]]
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = covar_names,
            d_cols = c(k, l),
            y_col = "hr_score"
            )
      }
    }
  }
}

### check for zero variance ----
zerovar_1990 <- caret::nearZeroVar(
  no_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_2fe <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_2fe_gen"))
