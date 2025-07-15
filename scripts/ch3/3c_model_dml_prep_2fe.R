# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch3/results/imputations/imp_1990_l1.rda"))

# get imputed data ----
## important: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.

m <- 1:imp_1990_l1$m
imp_1990_dfs <- list()

for(i in m){
  imp_df <- imp_1990_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(.imp == i) |> 
    select(
      -last_col(),
      -last_col(offset = 1)
    )
  
  imp_1990_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## get initial specs ----
### also important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lassos.
covar_names_1990 <- model.matrix(
  ~ . - 1, data = imp_1990_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -1,
    -c(n_ems, hr_score),
    -starts_with("any_inforce")
    )|> 
  names()

treat_names <- c("n_ems", "any_inforce1")

start_1990 <- list()

## finalize ----
for(i in m){
  df <- imp_1990_dfs[[i]]
  df <- model.matrix(
    ~ . - 1,
    data = df
  ) |> 
    as.data.table()
  for(name in treat_names){
    start_1990[[as.character(name)]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names_1990,
        d_cols = name,
        y_col = "hr_score"
      )
  }
}

### check for zero variance
zerovar_1990 <- caret::nearZeroVar(
  start_1990[[1]][[1]]$data_model,
  saveMetrics = T
  )

# combine ----
imp_dml_dats_2fe <- list(
  start_1990 = start_1990
  )

# save initialized data ----
imp_dml_dats_2fe |> 
  save(file = here("data/ch3/results/fits/dml_lasso/dml_initial/imp_dml_dats_2fe.rda"))
