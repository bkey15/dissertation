# Prep lasso models across all imputed datasets.
# IMPORTANT: we're not using the spatially-lagged versions of the treatments as covariates (for now) because this introduces the same endogeneity problems from which SAR suffers, specifically when the treatment(s) is/are regressed on the covariates (d ~ X). This is also why we're not including the spatially-lagged n_bits when the treatment is itself/includes n_bits.
# IMPORTANT: difference here is we're including a region fixed effect (following Wimpy, Whitten, Williams)

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch2/results/imputations/imp_1962_sp_t_lags.rda"))
load(here("data/ch2/results/imputations/imp_1981_sp_t_lags.rda"))
load(here("data/ch2/results/imputations/imp_1990_sp_t_lags.rda"))

# get imputed datasets ----
## 1962 ----
## IMPORTANT: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
## IMPORTANT: drop out unused region levels. Unwanted columns will appear after initializing model.matrix otherwise.

m <- 1:imp_1962_sp_t_lags[[1]]$m
lag_names <- names(imp_1962_sp_t_lags)
imp_1962_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1962_sp_t_lags[[lag]]
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
        -cow,
        -last_col(),
        -last_col(offset = 1)
        ) |> 
      mutate(region = droplevels(region))
    
    imp_1962_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

## 1981 ----
imp_1981_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1981_sp_t_lags[[lag]]
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
        -cow,
        -last_col(),
        -last_col(offset = 1)
        ) |> 
      mutate(region = droplevels(region))
    
    imp_1981_dfs[[as.character(lag)]][[as.character(i)]] <- imp_df
  }
}

## 1990 ----
imp_1990_dfs <- list()

for(lag in lag_names){
  imp_dat <- imp_1990_sp_t_lags[[lag]]
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
        -cow,
        -last_col(),
        -last_col(offset = 1)
        ) |> 
      mutate(region = droplevels(region))
    
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
    -contains("sp_lag"),
    -ends_with("_pop")
    ) |> 
  names()

treat_names_nn <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("nn_"),
    -contains("sp_lag"),
    -ends_with("_pop")
    ) |> 
  names()

treat_names_ns <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("ns_"),
    -contains("sp_lag"),
    -ends_with("_pop")
    ) |> 
  names()

## interact names ----
interact_names_gen <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("e_polity2_x") & !contains(c("_nn_", "_ns_", "sp_lag")),
    -ends_with("_pop")
    ) |> 
  names()

interact_names_nn <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("e_polity2_x_nn"),
    -contains("sp_lag"),
    -ends_with("_pop")
    ) |> 
  names()

interact_names_ns <- imp_1962_dfs[[1]][[1]] |> 
  select(
    starts_with("e_polity2_x_ns"),
    -contains("sp_lag"),
    -ends_with("_pop")
    ) |> 
  names()

## covar names ----
### important: dropping first column after creating matrix to ensure first level of factor (region) isn't included in the lassos.
### 1962 ----
covar_names_1962 <- list()
covar_names_1962_sml <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1962_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -c(
        1,
        "n_bits",
        "e_polity2_x_n_bits",
        "e_polity2_x_n_bits_sp_lag"
        ),
      -contains(
        c(
          "partner",
          "hr_score",
          "nn_",
          "ns_"
          )
        )
      )|> 
    names()
  
  covar_names_1962[[as.character(lag)]] <- covar_names
  covar_names_1962_sml[[as.character(lag)]] <- covar_names[!covar_names == "n_bits_sp_lag"]
}

### 1981 ----
covar_names_1981 <- list()
covar_names_1981_sml <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1981_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -c(
        1,
        "n_bits",
        "e_polity2_x_n_bits",
        "e_polity2_x_n_bits_sp_lag"
        ),
      -contains(
        c(
          "partner",
          "hr_score",
          "nn_",
          "ns_"
          )
        )
      )|> 
    names()
  
  covar_names_1981[[as.character(lag)]] <- covar_names
  covar_names_1981_sml[[as.character(lag)]] <- covar_names[!covar_names == "n_bits_sp_lag"]
}

### 1990 ----
covar_names_1990 <- list()
covar_names_1990_sml <- list()

for(lag in lag_names){
  covar_names <- model.matrix(
    ~ . - 1,
    data = imp_1990_dfs[[lag]][[1]]
    ) |> 
    as_tibble() |> 
    select(
      -c(
        1,
        "n_bits",
        "e_polity2_x_n_bits",
        "e_polity2_x_n_bits_sp_lag"
        ),
      -contains(
        c(
          "partner",
          "hr_score",
          "nn_",
          "ns_"
          )
        )
      )|> 
    names()
  
  covar_names_1990[[as.character(lag)]] <- covar_names
  covar_names_1990_sml[[as.character(lag)]] <- covar_names[!covar_names == "n_bits_sp_lag"]
}

### combine ----
### IMPORTANT: leaving out start_1962 date for now (reduce computation time)
covar_names_all <- list(
  start_1981 = list(
    full = covar_names_1981,
    small = covar_names_1981_sml
    ),
  start_1990 = list(
    full = covar_names_1990,
    small = covar_names_1990_sml
    )
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
        if(treat == "n_bits"){
          covar_names <- covar_names_all[[year]][["small"]][[lag]]
          
          no_interactions[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = treat,
              y_col = "hr_score"
              )
        }
        else{
          covar_names <- covar_names_all[[year]][["full"]][[lag]]
          
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
}

#### partner-type (nn & ns) ----
for(year in start_yrs){
  year_dfs <- imp_dfs_all[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_nn)){
        k <- treat_names_nn[[j]]
        l <- treat_names_ns[[j]]
        if(k == "nn_n_bits"){
          covar_names <- covar_names_all[[year]][["small"]][[lag]]
          
          no_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = c(k, l),
              y_col = "hr_score"
              )
        }
        else{
          covar_names <- covar_names_all[[year]][["full"]][[lag]]
          
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
}

### check for zero variance ----
zerovar_1981 <- caret::nearZeroVar(
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
#### general ----
for(year in start_yrs){
  year_dfs <- imp_dfs_all[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_gen)){
        k <- treat_names_gen[[j]]
        l <- interact_names_gen[[j]]
        if(k == "n_bits"){
          covar_names <- covar_names_all[[year]][["small"]][[lag]]
          
          has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = c(k, l),
              y_col = "hr_score"
              )
        }
        else{
          covar_names <- covar_names_all[[year]][["full"]][[lag]]
          
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
}

#### partner-type (nn & ns) ----
for(year in start_yrs){
  year_dfs <- imp_dfs_all[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
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
        if(k == "nn_n_bits"){
          covar_names <- covar_names_all[[year]][["small"]][[lag]]
          
          has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = c(k, l, n, o),
              y_col = "hr_score"
              )
        }
        else{
          covar_names <- covar_names_all[[year]][["full"]][[lag]]
          
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
}

### check for zero variance ----
zerovar_1981 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

zerovar_1990 <- caret::nearZeroVar(
  has_interactions[[2]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_spat_regfe_north <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )
