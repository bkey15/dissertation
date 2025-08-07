# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch1/results/imputations/imp_1968_t_lags.rda"))
load(here("data/ch1/results/imputations/imp_1977_t_lags.rda"))
load(here("data/ch1/results/imputations/imp_1990_t_lags.rda"))

# get imputed data ----
## 1968 ----
### important: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
m <- 1:imp_1968_t_lags[[1]]$m
lag_names <- names(imp_1968_t_lags)
imp_1968_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1968_t_lags[[lag]]
  for(i in m){
    imp_df <- imp_dat |> 
      mice::complete(
        action = "long",
        include = TRUE
        ) |> 
      filter(.imp == i) |> 
      select(
        -contains(c("ss_", "ns_", "nn_")),
        -glb_s,
        -inforce,
        -last_col(),
        -last_col(offset = 1)
        )
    
    imp_1968_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

## 1977 ----
imp_1977_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1977_t_lags[[lag]]
  for(i in m){
    imp_df <- imp_dat |> 
      mice::complete(
        action = "long",
        include = TRUE
        ) |> 
      filter(.imp == i) |> 
      select(
        -contains(c("ss_", "ns_", "nn_")),
        -glb_s,
        -inforce,
        -last_col(),
        -last_col(offset = 1)
        )
    
    imp_1977_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

## 1990 ----
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
        -contains(c("ss_", "ns_", "nn_")),
        -glb_s,
        -inforce,
        -last_col(),
        -last_col(offset = 1)
        )
    
    imp_1990_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

## combine ----
### IMPORTANT: leaving out start_1968 date for now (reduce computation time)
imp_dfs_all <- list(
  start_1977 = imp_1977_dfs,
  start_1990 = imp_1990_dfs
  )

# get main specs ----
## treat names ----
treat_names_lech <- imp_1968_dfs[[1]][[1]] |> 
  select(
    starts_with("lech_hr"),
    -contains("pop")
    ) |> 
  names()

treat_names_cpr <- imp_1968_dfs[[1]][[1]] |> 
  select(
    starts_with("cpr_"),
    -contains("pop")
    ) |> 
  names()

treat_names_esr <- imp_1968_dfs[[1]][[1]] |> 
  select(
    starts_with("esr_"),
    -contains("pop")
    ) |> 
  names()

## interact names ----
interact_names_lech <- imp_1968_dfs[[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_lech"),
    -contains("pop")
    ) |> 
  names()

interact_names_cpr <- imp_1968_dfs[[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_cpr"),
    -contains("pop")
    ) |> 
  names()

interact_names_esr <- imp_1968_dfs[[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_esr"),
    -contains("pop")
    ) |> 
  names()

## covar names ----
## important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lassos.
### 1968 ----
covar_names_1968 <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1968_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -c(
        1,
        hr_score,
        n_ptas,
        ends_with("_mean")
        )
      ) |> 
    names()
  
  covar_names_1968[[as.character(lag)]] <- covar_names
}

### 1977 ----
covar_names_1977 <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1977_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -c(
        1,
        hr_score,
        n_ptas,
        ends_with("_mean")
        )
      ) |> 
    names()
  
  covar_names_1977[[as.character(lag)]] <- covar_names
}

### 1990 ----
covar_names_1990 <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1990_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -c(
        1,
        hr_score,
        n_ptas,
        ends_with("_mean")
        )
      ) |> 
    names()
  
  covar_names_1990[[as.character(lag)]] <- covar_names
}

### combine ----
### IMPORTANT: leaving out start_1962 date for now (reduce computation time)
covar_names_all <- list(
  start_1977 = covar_names_1977,
  start_1990 = covar_names_1990
  )

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()
start_yrs <- imp_dfs_all |> 
  names()

### finalize ----
#### lechner ----
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
      for(treat in treat_names_lech){
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

#### cpr & esr ----
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
      for(j in seq_along(treat_names_cpr)){
        k <- treat_names_cpr[[j]]
        l <- treat_names_esr[[j]]
        
        no_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |> 
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
zerovar_1977 <- caret::nearZeroVar(
  no_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

zerovar_1990 <- caret::nearZeroVar(
  no_interactions[[2]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

## has interactions ----
### get initial specs ----
has_interactions <- list()

### finalize ----
#### lechner ----
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
      for(j in seq_along(treat_names_lech)){
        k <- treat_names_lech[[j]]
        l <- interact_names_lech[[j]]
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

#### cpr & esr ----
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
      for(j in seq_along(treat_names_cpr)){
        k <- treat_names_cpr[[j]]
        l <- treat_names_esr[[j]]
        n <- interact_names_cpr[[j]]
        o <- interact_names_esr[[j]]
        
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
          double_ml_data_from_data_frame(
            x_cols = covar_names,
            d_cols = c(k, l, n, o),
            y_col = "hr_score"
          )
      }
    }
  }
}

### check for zero variance ----
zerovar_1977 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

zerovar_1990 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_2fe_gen <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )
