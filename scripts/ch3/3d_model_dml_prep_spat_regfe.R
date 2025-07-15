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
load(here("data/ch3/results/imputations/imp_1990_sp_l1.rda"))

# get imputed datasets ----
m <- 1:imp_1990_sp_l1$m
imp_1990_dfs <- list()

for(i in m){
  imp_df <- imp_1990_sp_l1 |> 
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
  
  imp_1990_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## get initial specs ----
## important: dropping first column after creating matrix to ensure first level of factor (region) isn't included in the lassos.
covar_names_1990 <- model.matrix(
  ~ . - 1, data = imp_1990_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -c(1, n_ems),
    -contains(c("any_inforce", "hr_score"))
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
imp_dml_dats_spat_regfe <- list(
  start_1990 = start_1990
  )

# save initialized data ----
imp_dml_dats_spat_regfe |> 
  save(file = here("data/ch3/results/fits/dml_lasso/dml_initial/imp_dml_dats_spat_regfe.rda"))
