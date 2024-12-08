# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch1/results/imputations/imp_1_l1.rda"))
load(here("data/ch1/results/imputations/imp_2_l1.rda"))
load(here("data/ch1/results/imputations/imp_3_l1.rda"))

# get imputed datasets ----
## imp_1 (no start year) ----
imp_1_dfs <- list()
m <- 1:5

for(i in m){
  imp_df <- imp_1_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
      ) |> 
    filter(.imp == i) |> 
    select(
      -last_col(),
      -last_col(offset = 1)
      )
  
  imp_1_dfs[[as.character(i)]] <- imp_df
}

## imp_2 (1968) ----
imp_2_dfs <- list()
m <- 1:5

for(i in m){
  imp_df <- imp_2_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(.imp == i) |> 
    select(
      -last_col(),
      -last_col(offset = 1)
    )
  
  imp_2_dfs[[as.character(i)]] <- imp_df
}

## imp_3 (1977) ----
imp_3_dfs <- list()
m <- 1:5

for(i in m){
  imp_df <- imp_3_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(.imp == i) |> 
    select(
      -last_col(),
      -last_col(offset = 1)
    )
  
  imp_3_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## imp_1 ----
### get initial specs ----
covar_names <- model.matrix(
  ~ . - 1,
  data = imp_1_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    starts_with("cow"),
    starts_with("year"),
    264:299
    ) |> 
  names()
m <- 1:5
imp_1_dml_dats_2fe <- list()

### cpr & esr ----
treat_names <- imp_1_dfs[[1]] |> 
  select(5:7, 9:11) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_1_dfs[[i]]
    ) |> 
    as.data.table()
  for(name in treat_names){
    imp_1_dml_dats_2fe[[as.character(name)]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = name,
        y_col = "hr_score"
        )
  }
}

### both ----
#### cpr_mean & esr_mean
treat_names <- imp_1_dfs[[1]] |> 
  select(5, 9) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_1_dfs[[i]]
    ) |> 
    as.data.table()
  imp_1_dml_dats_2fe[["both_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
      )
}

#### cpr_gdp & esr_gdp
treat_names <- imp_1_dfs[[1]] |> 
  select(6, 10) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_1_dfs[[i]]
  ) |> 
    as.data.table()
  imp_1_dml_dats_2fe[["both_gdp_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

#### cpr_gdppc & esr_gdppc
treat_names <- imp_1_dfs[[1]] |> 
  select(7, 11) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_1_dfs[[i]]
  ) |> 
    as.data.table()
  imp_1_dml_dats_2fe[["both_gdppc_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

## imp_2 ----
### get initial specs ----
covar_names <- model.matrix(
  ~ . - 1,
  data = imp_2_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    starts_with("cow"),
    starts_with("year"),
    241:276
  ) |> 
  names()
m <- 1:5
imp_2_dml_dats_2fe <- list()

### cpr & esr ----
treat_names <- imp_2_dfs[[1]] |> 
  select(5:7, 9:11) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_2_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names){
    imp_2_dml_dats_2fe[[as.character(name)]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = name,
        y_col = "hr_score"
      )
  }
}

### both ----
#### cpr_mean & esr_mean
treat_names <- imp_2_dfs[[1]] |> 
  select(5, 9) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_2_dfs[[i]]
  ) |> 
    as.data.table()
  imp_2_dml_dats_2fe[["both_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

#### cpr_gdp & esr_gdp
treat_names <- imp_2_dfs[[1]] |> 
  select(6, 10) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_2_dfs[[i]]
  ) |> 
    as.data.table()
  imp_2_dml_dats_2fe[["both_gdp_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

#### cpr_gdppc & esr_gdppc
treat_names <- imp_2_dfs[[1]] |> 
  select(7, 11) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_2_dfs[[i]]
  ) |> 
    as.data.table()
  imp_2_dml_dats_2fe[["both_gdppc_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

## imp_3 ----
### get initial specs ----
covar_names <- model.matrix(
  ~ . - 1,
  data = imp_3_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    starts_with("cow"),
    starts_with("year"),
    231:266
  ) |> 
  names()
m <- 1:5
imp_3_dml_dats_2fe <- list()

### cpr & esr ----
treat_names <- imp_3_dfs[[1]] |> 
  select(5:7, 9:11) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names){
    imp_3_dml_dats_2fe[[as.character(name)]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = name,
        y_col = "hr_score"
      )
  }
}

### both ----
#### cpr_mean & esr_mean
treat_names <- imp_3_dfs[[1]] |> 
  select(5, 9) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  imp_3_dml_dats_2fe[["both_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

#### cpr_gdp & esr_gdp
treat_names <- imp_3_dfs[[1]] |> 
  select(6, 10) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  imp_3_dml_dats_2fe[["both_gdp_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

#### cpr_gdppc & esr_gdppc
treat_names <- imp_3_dfs[[1]] |> 
  select(7, 11) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  imp_3_dml_dats_2fe[["both_gdppc_mean"]][[as.character(i)]] <- df |>
    double_ml_data_from_data_frame(
      x_cols = covar_names,
      d_cols = treat_names,
      y_col = "hr_score"
    )
}

# save initialized data ----
imp_1_dml_dats_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_1_dml_dats_2fe.rda"))
imp_2_dml_dats_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_2_dml_dats_2fe.rda"))
imp_3_dml_dats_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_3_dml_dats_2fe.rda"))
