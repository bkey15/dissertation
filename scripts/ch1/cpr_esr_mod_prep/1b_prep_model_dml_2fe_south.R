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
          glb_s == 1,
          .imp == i
          ) |> 
        select(
          -contains("nn_"),
          -glb_s,
          -any_inforce,
          -n_ptas,
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
  select(
    starts_with("lech_hr"),
    -contains("pop")
    ) |> 
  names()

treat_names_cpr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("cpr_"),
    -contains("pop")
    ) |> 
  names()

treat_names_esr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("esr_"),
    -contains("pop")
    ) |> 
  names()

### partner-type (ss & ns) ----
treat_names_ss_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ss_lech"),
    -ends_with("pop_mean")
    ) |> 
  names()

treat_names_ns_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ns_lech"),
    -ends_with("pop_mean")
    ) |> 
  names()

treat_names_ss_cpr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ss_cpr"),
    -ends_with("pop_mean")
    ) |> 
  names()

treat_names_ns_cpr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ns_cpr"),
    -ends_with("pop_mean")
    ) |> 
  names()

treat_names_ss_esr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ss_esr"),
    -ends_with("pop_mean")
    ) |> 
  names()

treat_names_ns_esr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ns_esr"),
    -ends_with("pop_mean")
    ) |> 
  names()

## interact names ----
### gen ----
interact_names_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_lech"),
    -contains("pop")
    ) |> 
  names()

interact_names_cpr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_cpr"),
    -contains("pop")
    ) |> 
  names()

interact_names_esr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_esr"),
    -contains("pop")
    ) |> 
  names()

### partner-type (ss & ns) ----
interact_names_ss_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_ss_lech"),
    -contains("pop")
    ) |> 
  names()

interact_names_ns_lech <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_ns_lech"),
    -contains("pop")
    ) |> 
  names()

interact_names_ss_cpr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_ss_cpr"),
    -contains("pop")
    ) |> 
  names()

interact_names_ns_cpr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_ns_cpr"),
    -contains("pop")
    ) |> 
  names()

interact_names_ss_esr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_ss_esr"),
    -contains("pop")
    ) |> 
  names()

interact_names_ns_esr <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_ns_esr"),
    -contains("pop")
    ) |> 
  names()

## covar names ----
### important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lassos.
covar_names_all <- list()

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
        -contains(c("hr_score", "mean"))
        ) |> 
      names()
  
    covar_names_all[[as.character(year)]][[as.character(lag)]] <- covar_names
  }
}

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()

### finalize ----
#### gen ----
#### i.e., no consideration for ns, ss bits
##### lechner ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(treat in treat_names_lech){
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

##### cpr & esr ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_cpr)){
        k <- treat_names_cpr[[j]]
        l <- treat_names_esr[[j]]
        
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

#### partner-type (ss & ns) ----
##### lechner ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_ss_lech)){
        k <- treat_names_ss_lech[[j]]
        l <- treat_names_ns_lech[[j]]
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

##### cpr & esr ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_ss_cpr)){
        k <- treat_names_ss_cpr[[j]]
        l <- treat_names_ns_cpr[[j]]
        n <- treat_names_ss_esr[[j]]
        o <- treat_names_ns_esr[[j]]
        no_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
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
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_lech)){
        k <- treat_names_lech[[j]]
        l <- interact_names_lech[[j]]
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

##### cpr & esr ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_cpr)){
        k <- treat_names_cpr[[j]]
        l <- treat_names_esr[[j]]
        n <- interact_names_cpr[[j]]
        o <- interact_names_esr[[j]]
        
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

#### partner-type (ss & ns) ----
##### lechner ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_ss_lech)){
        k <- treat_names_ss_lech[[j]]
        l <- treat_names_ns_lech[[j]]
        n <- interact_names_ss_lech[[j]]
        o <- interact_names_ns_lech[[j]]
        
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

##### cpr & esr ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(j in seq_along(treat_names_ss_cpr)){
        k <- treat_names_ss_cpr[[j]]
        l <- treat_names_ns_cpr[[j]]
        n <- treat_names_ss_esr[[j]]
        o <- treat_names_ns_esr[[j]]
        p <- interact_names_ss_cpr[[j]]
        q <- interact_names_ns_cpr[[j]]
        r <- interact_names_ss_esr[[j]]
        s <- interact_names_ns_esr[[j]]
        
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), as.character(n), as.character(o), as.character(p), as.character(q), as.character(r), as.character(s), sep = "_AND_")]][[as.character(i)]] <- df |> 
          double_ml_data_from_data_frame(
            x_cols = covar_names,
            d_cols = c(k, l, n, o, p, q, r, s),
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
imp_dml_dats_2fe_south <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_2fe_south"))
