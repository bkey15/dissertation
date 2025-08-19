# Prep lasso models across all imputed datasets.
# IMPORTANT: we're not using the spatially-lagged versions of the treatments as covariates (for now) because this introduces the same endogeneity problems from which SAR suffers, specifically when the treatment(s) is/are regressed on the covariates (d ~ X).
# IMPORTANT: difference here is we're including a region fixed effect (following Wimpy, Whitten, Williams)

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch1/results/imputations/imp_sp_t_lags.rda"))

# truncate data ----
## IMPORTANT: leaving out start_1968 date for now (reduces computation time & helps computer memory)
imp_sp_t_lags <- imp_sp_t_lags[-1]

# get imputed datasets ----
m <- 1:imp_sp_t_lags[[1]][[1]]$m
start_yrs <- names(imp_sp_t_lags)
imp_sp_t_dfs <- list()

for(year in start_yrs){
  lags <- imp_sp_t_lags[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    imp_dat <- lags[[lag]]
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
          -any_inforcece,
          -n_ptas,
          -cow,
          -last_col(),
          -last_col(offset = 1)
          )
  
      imp_sp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
    }
  }
}

# get main specs ----
## treat names ----
treat_names_lech <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("lech_hr"),
    -contains(c("pop", "sp_lag"))
    ) |> 
  names()

treat_names_cpr <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("cpr_"),
    -contains(c("pop", "sp_lag"))
    ) |> 
  names()

treat_names_esr <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("esr_"),
    -contains(c("pop", "sp_lag"))
    ) |> 
  names()

## interact names ----
interact_names_lech <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_lech"),
    -contains(c("pop", "sp_lag"))
    ) |> 
  names()

interact_names_cpr <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_cpr"),
    -contains(c("pop", "sp_lag"))
    ) |> 
  names()

interact_names_esr <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_esr"),
    -contains(c("pop", "sp_lag"))
    ) |> 
  names()

## covar names ----
### important: dropping first column after creating matrix to ensure first level of factor (region) isn't included in the lasso
covar_names_all <- list()

for(year in start_yrs){
  lags <- imp_sp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names <- model.matrix(
      ~ . - 1,
      data = imp_sp_t_dfs[[year]][[lag]][[1]]
      ) |> 
      as_tibble() |> 
      select(
        -1,
        -contains(c("hr_score", "mean"))
        ) |> 
      names()
    
    covar_names_all[[as.character(year)]][[as.character(lag)]] <- covar_names
  }
}

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()

### finalize ----
#### lechner ----
for(year in start_yrs){
  year_dfs <- imp_sp_t_dfs[[year]]
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
  year_dfs <- imp_sp_t_dfs[[year]]
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
  year_dfs <- imp_sp_t_dfs[[year]]
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
  year_dfs <- imp_sp_t_dfs[[year]]
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
imp_dml_dats_spat_regfe_gen <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_spat_regfe_gen"))
