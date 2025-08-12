# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch2/results/imputations/imp_1962_t_lags.rda"))
load(here("data/ch2/results/imputations/imp_1981_t_lags.rda"))
load(here("data/ch2/results/imputations/imp_1990_t_lags.rda"))

# get imputed datasets ----
## 1962 ----
## IMPORTANT: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
## IMPORTANT: drop out unused cow levels. Unwanted columns will appear after initializing model.matrix otherwise.

m <- 1:imp_1962_t_lags[[1]]$m
lag_names <- names(imp_1962_t_lags)
imp_1962_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1962_t_lags[[lag]]
  for(i in m){
    imp_df <- imp_dat |> 
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
        -any_inforce,
        -last_col(),
        -last_col(offset = 1)
        ) |> 
      mutate(cow = droplevels(cow))
    
    imp_1962_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

## 1981 ----
imp_1981_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1981_t_lags[[lag]]
  for(i in m){
    imp_df <- imp_dat |> 
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
        -any_inforce,
        -last_col(),
        -last_col(offset = 1)
        ) |> 
      mutate(cow = droplevels(cow))
    
    imp_1981_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
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
      filter(
        glb_s == 0,
        .imp == i
        ) |> 
      select(
        -contains("ss_"),
        -glb_s,
        -any_inforce,
        -last_col(),
        -last_col(offset = 1)
        ) |> 
      mutate(cow = droplevels(cow))
    
    imp_1990_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

## combine ----
### IMPORTANT: leaving out start_1962 date for now (reduce computation time)
imp_dfs_all <- list(
  start_1981 = imp_1981_dfs,
  start_1990 = imp_1990_dfs
  )

# get main specs ----
## treat names ----
treat_names_gen <- imp_1962_dfs[[1]][[1]] |> 
  select(
    n_bits,
    starts_with("partner_"),
    -ends_with("_pop")
    ) |> 
  names()

treat_names_nn <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("nn_"),
    -ends_with("_pop")
    ) |> 
  names()

treat_names_ns <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("ns_"),
    -ends_with("_pop")
    ) |> 
  names()

## interact names ----
interact_names_gen <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("e_polity2_x") & !contains(c("_nn_", "_ns_")),
    -ends_with("_pop")
    ) |> 
  names()

interact_names_nn <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("e_polity2_x_nn"),
    -ends_with("_pop")
    ) |> 
  names()

interact_names_ns <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("e_polity2_x_ns"),
    -ends_with("_pop")
    ) |> 
  names()

## covar names ----
## important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lasso
### 1962 ----
covar_names_1962 <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1962_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -1,
      -contains(
        c(
          "n_bits",
          "partner",
          "hr_score"
          )
        )
      )|> 
    names()
  
  covar_names_1962[[as.character(lag)]] <- covar_names
}

### 1981 ----
covar_names_1981 <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1981_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -1,
      -contains(
        c(
          "n_bits",
          "partner",
          "hr_score"
          )
        )
      )|> 
    names()
  
  covar_names_1981[[as.character(lag)]] <- covar_names
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
      -1,
      -contains(
        c(
          "n_bits",
          "partner",
          "hr_score"
          )
        )
      )|> 
    names()
  
  covar_names_1990[[as.character(lag)]] <- covar_names
}

### combine ----
### IMPORTANT: leaving out start_1962 date for now (reduce computation time)
covar_names_all <- list(
  start_1981 = covar_names_1981,
  start_1990 = covar_names_1990
  )

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()
start_yrs <- imp_dfs_all |> 
  names()

### finalize ----
#### general ----
#### i.e., no consideration for ns, nn bits
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
      for(treat in treat_names_gen){
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

#### partner-type (nn & ns) ----
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
      for(j in seq_along(treat_names_nn)){
        k <- treat_names_nn[[j]]
        l <- treat_names_ns[[j]]
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

## check for zero variance ----
zerovar_1981 <- caret::nearZeroVar(
  no_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

zerovar_1990 <- caret::nearZeroVar(
  no_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

## has interactions ----
### get initial specs ----
has_interactions <- list()

### finalize ----
#### general ----
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
      for(j in seq_along(treat_names_gen)){
        k <- treat_names_gen[[j]]
        l <- interact_names_gen[[j]]
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

#### partner-type (nn & ns) ----
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
      for(j in seq_along(treat_names_nn)){
        k <- treat_names_nn[[j]]
        l <- treat_names_ns[[j]]
        n <- interact_names_nn[[j]]
        o <- interact_names_ns[[j]]
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
zerovar_1981 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

zerovar_1990 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_2fe_north <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_2fe_north"))
