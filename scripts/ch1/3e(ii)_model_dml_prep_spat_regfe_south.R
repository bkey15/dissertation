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
load(here("data/ch1/results/imputations/imp_1968_sp_l1.rda"))
load(here("data/ch1/results/imputations/imp_1977_sp_l1.rda"))

# get imputed datasets ----
## 1968 ----
## important: drop out unused region levels. Unwanted columns will appear after initializing model.matrix otherwise.
imp_1968_dfs <- list()
m <- 1:imp_1968_sp_l1$m

for(i in m){
  imp_df <- imp_1968_sp_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(
      glb_s == 1,
      .imp == i
    ) |> 
    select(
      -starts_with("nn_"),
      -glb_s,
      -last_col(),
      -last_col(offset = 1),
      -cow,
      -inforce,
      -n_ptas
    ) |> 
    mutate(region = droplevels(region))
  
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
    filter(
      glb_s == 1,
      .imp == i
    ) |> 
    select(
      -starts_with("nn_"),
      -glb_s,
      -last_col(),
      -last_col(offset = 1),
      -cow,
      -inforce,
      -n_ptas
    ) |> 
    mutate(region = droplevels(region))
  
  imp_1977_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## 1968 ----
### get initial specs ----
start_1968 <- list()

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

### general ----
#### lechner ----
treat_names_lech <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("lech_hr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

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

#### cpr & esr ----
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

### partner type ----
#### lechner south ----
treat_names_ss_lech <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("ss_lech"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

treat_names_ns_lech <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("ns_lech"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1968_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_lech)){
    k <- treat_names_ss_lech[[j]]
    l <- treat_names_ns_lech[[j]]
    start_1968[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1968,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

#### cpr ----
treat_names_ss_cpr <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("ss_cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

treat_names_ns_cpr <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("ns_cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1968_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    start_1968[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1968,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

#### esr ----
treat_names_ss_esr <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("ss_esr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

treat_names_ns_esr <- imp_1968_dfs[[1]] |> 
  select(
    starts_with("ns_esr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1968_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_esr)){
    k <- treat_names_ss_esr[[j]]
    l <- treat_names_ns_esr[[j]]
    start_1968[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1968,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

#### cpr & esr ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1968_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    n <- treat_names_ss_esr[[j]]
    o <- treat_names_ns_esr[[j]]
    start_1968[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names_1968,
        d_cols = c(k, l, n, o),
        y_col = "hr_score"
      )
  }
}

## 1977 ----
### get initial specs ----
start_1977 <- list()

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

### general ----
#### lechner ----
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

#### cpr & esr ----
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

### partner type ----
#### lechner south ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1977_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_lech)){
    k <- treat_names_ss_lech[[j]]
    l <- treat_names_ns_lech[[j]]
    start_1977[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1977,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

#### cpr ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1977_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    start_1977[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1977,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

#### esr ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1977_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_esr)){
    k <- treat_names_ss_esr[[j]]
    l <- treat_names_ns_esr[[j]]
    start_1977[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1977,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

#### cpr & esr ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1977_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    n <- treat_names_ss_esr[[j]]
    o <- treat_names_ns_esr[[j]]
    start_1977[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names_1977,
        d_cols = c(k, l, n, o),
        y_col = "hr_score"
      )
  }
}

# combine ----
imp_dml_dats_spat_regfe_south <- list(
  start_1968 = start_1968,
  start_1977 = start_1977
)

# save initialized data ----
imp_dml_dats_spat_regfe_south |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_dml_dats_spat_regfe_south.rda"))
