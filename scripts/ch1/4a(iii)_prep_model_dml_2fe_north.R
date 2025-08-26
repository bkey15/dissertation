# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch1/results/imputations/imp_t_lags.rda"))

# truncate data ----
## IMPORTANT: leaving out start_1968 date for now (reduces computation time & helps computer memory)
imp_t_lags <- imp_t_lags[-1]

# get imputed datasets ----
## IMPORTANT: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
## IMPORTANT: drop out unused cow levels. Unwanted columns will appear after initializing model.matrix otherwise.

m <- 1:imp_t_lags[[1]][[1]]$m
start_yrs <- names(imp_t_lags)
imp_t_dfs <- list()

for(year in start_yrs){
  lags <- imp_t_lags[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    imp_dat <- lags[[lag]]
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
          -contains(
            c(
              "ss_",
              "cpr",
              "esr"
              )
            ),
          -glb_s,
          -last_col(),
          -last_col(offset = 1)
          ) |> 
        mutate(cow = droplevels(cow))
      
      imp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
    }
  }
}

# get main specs ----
## treat names ----
### gen ----
treat_names_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("lech_hr")) |> 
  names()

treat_names_rc <- c("n_ptas", "any_inforce1")

### partner-type (nn & ns) ----
treat_names_nn_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("nn_lech")) |> 
  names()

treat_names_ns_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("ns_lech")) |> 
  names()

treat_names_nn_rc <- c("nn_n_ptas", "nn_any_inforce1")

treat_names_ns_rc <- c("ns_n_ptas", "ns_any_inforce1")

## interact names ----
### gen ----
interact_names_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("v2x_polyarchy_x_lech")) |> 
  names()

interact_names_rc <- c(
  "v2x_polyarchy_x_n_ptas",
  "v2x_polyarchy_x_any_inforce"
  )

### partner-type (nn & ns) ----
interact_names_nn_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("v2x_polyarchy_x_nn_lech")) |> 
  names()

interact_names_ns_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("v2x_polyarchy_x_ns_lech")) |> 
  names()

interact_names_nn_rc <- c(
  "v2x_polyarchy_x_nn_n_ptas",
  "v2x_polyarchy_x_nn_any_inforce"
  )

interact_names_ns_rc <- c(
  "v2x_polyarchy_x_ns_n_ptas",
  "v2x_polyarchy_x_ns_any_inforce"
  )

## covar names ----
### get initial specs ----
dep_enf_ids <- c("mean", "gdp_mean", "gdppc_mean")

#### gen ----
depth_names_gen <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("depth")) |> 
  names()

enforce_names_gen <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("enforce")) |> 
  names()

covar_names_gen_sml <- list()
covar_names_gen_all <- list()

#### partner-type (nn & ns) ----
depth_names_nn <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("nn_depth")) |> 
  names()
depth_names_ns <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("ns_depth")) |> 
  names()

enforce_names_nn <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("nn_enforce")) |> 
  names()
enforce_names_ns <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("ns_enforce")) |> 
  names()

covar_names_pt_sml <- list()
covar_names_pt_all <- list()

### finalize ----
### important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lassos.
#### gen ----
for(year in start_yrs){
  lags <- imp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names <- model.matrix(
      ~ . - 1,
      data = imp_t_dfs[[year]][[lag]][[1]]
      ) |> 
      as_tibble() |> 
      select(
        -1,
        -contains(
          c(
            "hr_score",
            "mean",
            "n_ptas",
            "any_inforce"
            )
          )
        ) |> 
      names()
    for(j in seq_along(depth_names_gen)){
      k <- depth_names_gen[[j]]
      l <- enforce_names_gen[[j]]
      n <- dep_enf_ids[[j]]
      
      covar_names_gen_sml[[as.character(n)]] <- covar_names |> 
        append(c(k, l))
      
      covar_names_gen_all[[as.character(year)]][[as.character(lag)]] <- covar_names_gen_sml
    }
  }
}

#### partner-type (nn & ns) ----
for(year in start_yrs){
  lags <- imp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names <- model.matrix(
      ~ . - 1,
      data = imp_t_dfs[[year]][[lag]][[1]]
      ) |> 
      as_tibble() |> 
      select(
        -1,
        -contains(
          c(
            "hr_score",
            "mean",
            "n_ptas",
            "any_inforce"
            )
          )
        ) |> 
      names()
    for(j in seq_along(depth_names_gen)){
      k <- depth_names_ns[[j]]
      l <- enforce_names_ns[[j]]
      n <- dep_enf_ids[[j]]
      o <- depth_names_nn[[j]]
      p <- enforce_names_nn[[j]]
      
      covar_names_pt_sml[[as.character(n)]] <- covar_names |> 
        append(c(k, l, o, p))
      
      covar_names_pt_all[[as.character(year)]][[as.character(lag)]] <- covar_names_pt_sml
    }
  }
}

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()

### finalize ----
#### gen ----
#### i.e., no consideration for ns, nn bits
##### lechner ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_gen_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_lech)){
        k <- treat_names_lech[[j]]
        l <- covar_names[[j]]
        
        no_interactions[[as.character(year)]][[as.character(lag)]][[as.character(k)]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = l,
            d_cols = k,
            y_col = "hr_score"
            )
      }
    }
  }
}

##### rc treats ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_gen_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_rc)){
        k <- treat_names_rc[[j]]
        l <- covar_names[[1]]
        
        no_interactions[[as.character(year)]][[as.character(lag)]][[as.character(k)]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = l,
            d_cols = k,
            y_col = "hr_score"
            )
      }
    }
  }
}

#### partner-type (nn & ns) ----
##### lechner ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_pt_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_nn_lech)){
        k <- treat_names_nn_lech[[j]]
        l <- treat_names_ns_lech[[j]]
        n <- covar_names[[j]]
        
        no_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = n,
            d_cols = c(k, l),
            y_col = "hr_score"
            )
      }
    }
  }
}

##### rc treats ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_pt_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_nn_rc)){
        k <- treat_names_nn_rc[[j]]
        l <- treat_names_ns_rc[[j]]
        n <- covar_names[[1]]
        
        no_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = n,
            d_cols = c(k, l),
            y_col = "hr_score"
            )
      }
    }
  }
}

### check for zero variance ----
zerovar_1977 <- caret::nearZeroVar(
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
#### gen ----
##### lechner ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_gen_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_lech)){
        k <- treat_names_lech[[j]]
        l <- interact_names_lech[[j]]
        n <- covar_names[[j]]
        
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = n,
            d_cols = c(k, l),
            y_col = "hr_score"
            )
      }
    }
  }
}

##### rc treats ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_gen_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_rc)){
        k <- treat_names_rc[[j]]
        l <- interact_names_rc[[j]]
        n <- covar_names[[1]]
        
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
          double_ml_data_from_data_frame(
            x_cols = n,
            d_cols = c(k, l),
            y_col = "hr_score"
            )
      }
    }
  }
}

#### partner-type (nn & ns) ----
##### lechner ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_pt_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_nn_lech)){
        k <- treat_names_nn_lech[[j]]
        l <- treat_names_ns_lech[[j]]
        n <- interact_names_nn_lech[[j]]
        o <- interact_names_ns_lech[[j]]
        p <- covar_names[[j]]
        
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
          double_ml_data_from_data_frame(
            x_cols = p,
            d_cols = c(k, l, n, o),
            y_col = "hr_score"
            )
      }
    }
  }
}

##### rc treats ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_pt_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_nn_rc)){
        k <- treat_names_nn_rc[[j]]
        l <- treat_names_ns_rc[[j]]
        n <- interact_names_nn_rc[[j]]
        o <- interact_names_ns_rc[[j]]
        p <- covar_names[[1]]
        
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
          double_ml_data_from_data_frame(
            x_cols = p,
            d_cols = c(k, l, n, o),
            y_col = "hr_score"
            )
      }
    }
  }
}

### check for zero variance ----
zerovar_1977 <- caret::nearZeroVar(
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