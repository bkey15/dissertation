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
load(here("data/ch3/results/imputations/imp_1990_sp_t_lags.rda"))

# get imputed data ----
m <- 1:imp_1990_sp_t_lags[[1]]$m
lag_names <- names(imp_1990_sp_t_lags)
imp_1990_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1990_sp_t_lags[[lag]]
  for(i in m){
    imp_df <- imp_dat |> 
      mice::complete(
        action = "long",
        include = TRUE
        ) |> 
      filter(.imp == i) |> 
      select(
        -last_col(),
        -last_col(offset = 1),
        -cow
        )
    
    imp_1990_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

# initialize data backend ----
## no interactions ----
### 1990 ----
#### get initial specs ----
#### important: dropping first column after creating matrix to ensure first level of factor (region) isn't included in the lassos.
covar_names_1990 <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1990_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -1,
      -hr_score,
      -contains(
        c("n_ems", "any_inforce")
        )
      )|> 
    names()
  
  covar_names_1990[[as.character(lag)]] <- covar_names
}

treat_names <- c("n_ems", "any_inforce1")

start_1990 <- list()

#### finalize ----
for(lag in lag_names){
  lag_df <- imp_1990_dfs[[lag]]
  covar_names <- covar_names_1990[[lag]]
  for(i in m){
    df <- lag_df[[i]]
    df <- model.matrix(
      ~ . - 1,
      data = df
      ) |> 
      as.data.table()
    for(treat in treat_names){
      start_1990[[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names,
          d_cols = treat,
          y_col = "hr_score"
          )
    }
  }
}

### check for zero variance
zerovar_1990 <- caret::nearZeroVar(
  start_1990[[8]][[1]][[1]]$data_model,
  saveMetrics = T
  )

### combine ----
no_interactions <- list(
  start_1990 = start_1990
  )

## has interactions ----
### 1990 ----
#### get initial specs ----
interact_names <- imp_1990_dfs[[1]][[1]] |> 
  select(contains("_x_")) |> 
  names()

start_1990 <- list()

#### finalize ----
for(lag in lag_names){
  lag_df <- imp_1990_dfs[[lag]]
  covar_names <- covar_names_1990[[lag]]
  for(i in m){
    df <- lag_df[[i]]
    df <- model.matrix(
      ~ . - 1,
      data = df
      ) |> 
      as.data.table()
    for(j in seq_along(treat_names)){
      k <- treat_names[[j]]
      l <- interact_names[[j]]
      
      start_1990[[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names,
          d_cols = c(k, l),
          y_col = "hr_score"
          )
    }
  }
}

### combine ----
has_interactions <- list(
  start_1990 = start_1990
  )

# all combine ----
imp_dml_dats_spat_regfe <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# save initialized data ----
imp_dml_dats_spat_regfe |> 
  save(file = here("data/ch3/results/fits/dml_lasso/dml_initial/imp_dml_dats_spat_regfe.rda"))
