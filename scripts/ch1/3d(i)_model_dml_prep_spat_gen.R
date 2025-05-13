# Prep lasso models across all imputed datasets.
# IMPORTANT: we're not using the spatially-lagged versions of the treatments as covariates (for now) because this introduces the same endogeneity problems from which SAR suffers, specifically when the treatment(s) is/are regressed on the covariates (d ~ X).
## note: removing original n_ptas (i.e., the number of ptas a given country has acceded to) b/c it's unlikely to be causally related w/ the treatment(s) (though perhaps the outcome), and in any case doesn't capture "competitive pressures"

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch1/results/imputations/imp_1968_sp_l1.rda"))
load(here("data/ch1/results/imputations/imp_1977_sp_l1.rda"))

# get imputed datasets ----
## 1968 ----
imp_1968_dfs <- list()
m <- 1:imp_1968_sp_l1$m

for(i in m){
  imp_df <- imp_1968_sp_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
      ) |> 
    filter(.imp == i) |> 
    select(
      -starts_with(c("ss_", "ns_", "nn_")),
      -glb_s,
      -last_col(),
      -last_col(offset = 1),
      -cow,
      -region,
      -inforce,
      -n_ptas
    )
  
  imp_1968_dfs[[as.character(i)]] <- imp_df
}

## 1977 ----
imp_1977_dfs <- list()

for(i in m){
  imp_df <- imp_1977_sp_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(.imp == i) |> 
    select(
      -starts_with(c("ss_", "ns_", "nn_")),
      -glb_s,
      -last_col(),
      -last_col(offset = 1),
      -cow,
      -region,
      -inforce,
      -n_ptas
    )
  
  imp_1977_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## 1968 ----
### get initial specs ----
covar_names_1968 <- model.matrix(
  ~ . - 1,
  data = imp_1968_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -1,
    -starts_with(
      c(
        "hr_score",
        "cpr_",
        "esr_",
        "lech_",
        "ns_",
        "ss_"
        )
      )
    ) |> 
  names()

treat_names_lech <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("lech_hr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

treat_names_cpr <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

treat_names_esr <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("esr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

start_1968 <- list()

### lechner ----
for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_1968_dfs[[i]]
    ) |> 
    as.data.table()
  for(name in treat_names_lech){
    start_1968[[as.character(name)]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1968,
        d_cols = name,
        y_col = "hr_score"
      )
  }
}

### cpr & esr ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1968_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_cpr)){
    k <- treat_names_cpr[[j]]
    l <- treat_names_esr[[j]]
    
    start_1968[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1968,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

## 1977 ----
### get initial specs ----
covar_names_1977 <- model.matrix(
  ~ . - 1,
  data = imp_1977_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -1,
    -starts_with(
      c(
        "hr_score",
        "cpr_",
        "esr_",
        "lech_",
        "ns_",
        "ss_"
      )
    )
  ) |> 
  names()

start_1977 <- list()

### lechner ----
for(i in m){
  df <- model.matrix(
    ~ . - 1,
    data = imp_1977_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names_lech){
    start_1977[[as.character(name)]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1977,
        d_cols = name,
        y_col = "hr_score"
      )
  }
}

### cpr & esr ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1977_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_cpr)){
    k <- treat_names_cpr[[j]]
    l <- treat_names_esr[[j]]
    
    start_1977[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1977,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
    }
}

# check for zero variance ----
zerovar_1968 <- caret::nearZeroVar(
  start_1968[[1]][[1]]$data_model,
  saveMetrics = T
)

zerovar_1977 <- caret::nearZeroVar(
  start_1977[[1]][[1]]$data_model,
  saveMetrics = T
)

# combine ----
imp_dml_dats_spat_gen <- list(
  start_1968 = start_1968,
  start_1977 = start_1977
  )

# save initialized data ----
imp_dml_dats_spat_gen |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_dml_dats_spat_gen.rda"))
