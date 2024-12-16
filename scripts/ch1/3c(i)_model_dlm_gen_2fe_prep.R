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
## imp_2 ----
### get initial specs ----
### IMPORTANT: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets
imp_2_df1 <- imp_2_dfs[[1]] |> 
  select(
    -starts_with("ss_"),
    -starts_with("ns_"),
    -starts_with("nn_"),
    -glb_s
  )

covar_names <- model.matrix(
  ~ . - 1,
  data = imp_2_df1
) |> 
  as_tibble() |> 
  select(
    starts_with("cow"),
    starts_with("year"),
    239:274
  ) |> 
  names()
m <- 1:5
imp_2_dml_dats_2fe <- list()

### cpr & esr ----
treat_names <- imp_2_dfs[[1]] |> 
  select(
    starts_with("cpr_"),
    starts_with("esr_"),
    -contains("pop_mean")
  ) |> 
  names()

for(i in m){
  df <- imp_2_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
treat_names <- c("cpr_mean", "esr_mean")

for(i in m){
  df <- imp_2_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
treat_names <- c("cpr_gdp_mean", "esr_gdp_mean")

for(i in m){
  df <- imp_2_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
treat_names <- c("cpr_gdppc_mean", "esr_gdppc_mean")

for(i in m){
  df <- imp_2_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
imp_3_df1 <- imp_3_dfs[[1]] |> 
  select(
    -starts_with("ss_"),
    -starts_with("ns_"),
    -starts_with("nn_"),
    -glb_s
  )

covar_names <- model.matrix(
  ~ . - 1,
  data = imp_3_df1
) |> 
  as_tibble() |> 
  select(
    starts_with("cow"),
    starts_with("year"),
    229:264
  ) |> 
  names()
m <- 1:5
imp_3_dml_dats_2fe <- list()

### cpr & esr ----
treat_names <- imp_3_dfs[[1]] |> 
  select(
    starts_with("cpr_"),
    starts_with("esr_"),
    -contains("pop_mean")
  ) |> 
  names()

for(i in m){
  df <- imp_3_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
treat_names <- c("cpr_mean", "esr_mean")

for(i in m){
  df <- imp_3_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
treat_names <- c("cpr_gdp_mean", "esr_gdp_mean")

for(i in m){
  df <- imp_3_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
treat_names <- c("cpr_gdppc_mean", "esr_gdppc_mean")

for(i in m){
  df <- imp_3_dfs[[i]] |> 
    select(
      -starts_with("ss_"),
      -starts_with("ns_"),
      -starts_with("nn_"),
      -glb_s
    )
  df <- model.matrix(
    ~ . - 1,
    data = df
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
imp_2_dml_dats_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_2_dml_dats_2fe.rda"))
imp_3_dml_dats_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_3_dml_dats_2fe.rda"))
