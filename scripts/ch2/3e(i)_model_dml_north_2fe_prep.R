# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch2/results/imputations/imp_1962_l1.rda"))
load(here("data/ch2/results/imputations/imp_1981_l1.rda"))
load(here("data/ch2/results/imputations/imp_1990_l1.rda"))

# get imputed datasets ----
## 1962 ----
## IMPORTANT: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
## IMPORTANT: drop out unused cow levels. Unwanted columns will appear after initializing model.matrix otherwise.
### also important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lassos.

imp_1962_dfs <- list()
m <- 1:imp_1962_l1$m

for(i in m){
  imp_df <- imp_1962_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
      ) |> 
    filter(
      glb_s == 0,
      .imp == i
      ) |> 
    select(
      -contains("ss_"),
      -glb_s,
      -last_col(),
      -last_col(offset = 1)
      ) |> 
    mutate(cow = droplevels(cow))
  
  imp_1962_dfs[[as.character(i)]] <- imp_df
}

## 1981 ----
imp_1981_dfs <- list()

for(i in m){
  imp_df <- imp_1981_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(
      glb_s == 0,
      .imp == i
    ) |> 
    select(
      -contains("ss_"),
      -glb_s,
      -last_col(),
      -last_col(offset = 1)
    ) |> 
    mutate(cow = droplevels(cow))
  
  imp_1981_dfs[[as.character(i)]] <- imp_df
}

## 1990 ----
imp_1990_dfs <- list()

for(i in m){
  imp_df <- imp_1990_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(
      glb_s == 0,
      .imp == i
    ) |> 
    select(
      -contains("ss_"),
      -glb_s,
      -last_col(),
      -last_col(offset = 1)
    ) |> 
    mutate(cow = droplevels(cow))
  
  imp_1990_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## no interactions ----
### 1962 ----
#### get initial specs ----
covar_names_1962 <- model.matrix(
  ~ . - 1, data = imp_1962_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -1,
    -contains(
      c(
        "n_bits",
        "partner",
        "hr_score",
        "any_inforce"
        )
      )
    )|> 
  names()

treat_names_gen <- imp_1962_dfs[[1]] |> 
  select(n_bits, starts_with("partner_")) |> 
  names()
treat_names_nn <- imp_1962_dfs[[1]] |> 
  select(starts_with("nn_")) |> 
  names()
treat_names_ns <- imp_1962_dfs[[1]] |> 
  select(starts_with("ns_")) |> 
  names()

start_1962 <- list()

#### finalize ----
##### general ----
##### i.e., no consideration for ns, nn bits
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
    ) |> 
    as.data.table()
  for(name in treat_names_gen){
    start_1962[[as.character(name)]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1962,
        d_cols = name,
        y_col = "hr_score"
        )
  }
}

##### partner-type (nn & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_nn)){
    k <- treat_names_nn[[j]]
    l <- treat_names_ns[[j]]
    start_1962[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1962,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### 1981 ----
#### get initial specs ----
covar_names_1981 <- model.matrix(
  ~ . - 1, data = imp_1981_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -1,
    -contains(
      c(
        "n_bits",
        "partner",
        "hr_score",
        "any_inforce"
        )
      )
    )|> 
  names()

start_1981 <- list()

#### finalize ----
##### general ----
##### i.e., no consideration for ns, nn bits
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names_gen){
    start_1981[[as.character(name)]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1981,
        d_cols = name,
        y_col = "hr_score"
      )
  }
}

##### partner-type (nn & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_nn)){
    k <- treat_names_nn[[j]]
    l <- treat_names_ns[[j]]
    start_1981[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1981,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### 1990 ----
#### get initial specs ----
covar_names_1990 <- model.matrix(
  ~ . - 1, data = imp_1990_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -1,
    -contains(
      c(
        "n_bits",
        "partner",
        "hr_score",
        "any_inforce"
        )
      )
    )|> 
  names()

start_1990 <- list()

#### finalize ----
##### general ----
##### i.e., no consideration for ns, nn bits
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names_gen){
    start_1990[[as.character(name)]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1990,
        d_cols = name,
        y_col = "hr_score"
      )
  }
}

##### partner-type (nn & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_nn)){
    k <- treat_names_nn[[j]]
    l <- treat_names_ns[[j]]
    start_1990[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1990,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### combine ----
no_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

## has interactions ----
### 1962 ----
#### get initial specs ----
interact_names_gen <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("e_polity2_x") & !contains(c("_nn_", "_ns_"))
    ) |> 
  names()

interact_names_nn <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("e_polity2_x_nn")
    ) |> 
  names()

interact_names_ns <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("e_polity2_x_ns")
  ) |> 
  names()

start_1962 <- list()

#### finalize ----
##### general ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_gen)){
    k <- treat_names_gen[[j]]
    l <- interact_names_gen[[j]]
    start_1962[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1962,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

##### partner-type (nn & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_nn)){
    k <- treat_names_nn[[j]]
    l <- treat_names_ns[[j]]
    n <- interact_names_nn[[j]]
    o <- interact_names_ns[[j]]
    start_1962[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names_1962,
        d_cols = c(k, l, n, o),
        y_col = "hr_score"
      )
  }
}

### 1981 ----
#### get initial specs ----
start_1981 <- list()

#### finalize ----
##### general ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_gen)){
    k <- treat_names_gen[[j]]
    l <- interact_names_gen[[j]]
    start_1981[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1981,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

##### partner-type (nn & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_nn)){
    k <- treat_names_nn[[j]]
    l <- treat_names_ns[[j]]
    n <- interact_names_nn[[j]]
    o <- interact_names_ns[[j]]
    start_1981[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names_1981,
        d_cols = c(k, l, n, o),
        y_col = "hr_score"
      )
  }
}

### 1990 ----
#### get initial specs ----
start_1990 <- list()

#### finalize ----
##### general ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_gen)){
    k <- treat_names_gen[[j]]
    l <- interact_names_gen[[j]]
    start_1990[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names_1990,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

##### partner-type (nn & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_nn)){
    k <- treat_names_nn[[j]]
    l <- treat_names_ns[[j]]
    n <- interact_names_nn[[j]]
    o <- interact_names_ns[[j]]
    start_1990[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names_1990,
        d_cols = c(k, l, n, o),
        y_col = "hr_score"
      )
  }
}

### combine ----
has_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# all combine ----
imp_dml_dats_2fe_north <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# save initialized data ----
imp_dml_dats_2fe_north |> 
  save(file = here("data/ch2/results/fits/dml_lasso/dml_initial/imp_dml_dats_2fe_north.rda"))
