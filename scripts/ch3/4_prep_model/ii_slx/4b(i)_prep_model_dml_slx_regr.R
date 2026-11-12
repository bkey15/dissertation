# Prep models across all imputed datasets.
# Model type: SLX (regr: treat == n_ems)

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(tidymodels)
library(data.table)
library(janitor)

# load data ----
load(here("data/ch3/results/imputations/imp_t_lags.rda"))

# get imputed data ----
## note: code below is drawn from earlier chapters; can be used in the event other start-dates are utilized
## note: filtering out ordinal polyarchy (e_v2x_polyarchy_5C) vars. no need to use these at the moment (see `notes` for original intent).
## important: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
m <- seq_len(imp_t_lags[[1]][[1]]$m)
start_yrs <- names(imp_t_lags)
imp_t_dfs <- list()

for(year in start_yrs){
  lags <- imp_t_lags[[year]]
  # note: reducing lags for now
  lag_names <- names(lags)[1:4]
  for(lag in lag_names){
    imp_dat <- lags[[lag]]
    for(i in m){
      imp_df <- imp_dat |> 
        mice::complete(
          action = "long",
          include = TRUE
          ) |> 
        filter(.imp == i) |> 
        select(
          -contains(
            c(
              "e_v2x_polyarchy",
              "any_inforce"
              )
            ),
          -glb_s,
          -region,
          -last_col(),
          -last_col(offset = 1)
          )
      
      imp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
    }
  }
}

# get main specs ----
## treat names ----
treat_names <- "n_ems"

## interact names ----
interact_names <- "v2x_polyarchy_x_n_ems"

## covar names ----
### get initial specs ----
#### treats
treat_names_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with(treat_names) & ends_with("cre") & -contains("sp_lag")
    ) |> 
  names()

treat_names_sp_lag <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with(treat_names) & ends_with("sp_lag")
    ) |> 
  names()

treat_names_sp_lag_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with(treat_names) & ends_with("sp_lag_cre")
    ) |> 
  names()

#### interact
interact_names_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with(interact_names) & ends_with("cre") & -contains("sp_lag")
    ) |> 
  names()

interact_names_sp_lag <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with(interact_names) & ends_with("sp_lag")
    ) |> 
  names()

interact_names_sp_lag_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with(interact_names) & ends_with("sp_lag_cre")
    ) |> 
  names()

### finalize ----
covar_names_all <- list()

covar_names_sml <- imp_t_dfs$start_1990$l1$`1` |> 
  select(
    -c(cow, year)
    ) |> 
  recipe(hr_score ~ .) |> 
  step_dummy(all_nominal_predictors()) |> 
  prep() |> 
  bake(new_data = NULL) |> 
  select(
    -contains(
      c(treat_names, interact_names)
      ),
    -hr_score,
    -hr_score_cre
    ) |> 
  names()

for(j in seq_along(treat_names)){
  k <- treat_names_cre[[j]]
  l <- treat_names_sp_lag[[j]]
  n <- treat_names[[j]]
  o <- treat_names_sp_lag_cre[[j]]
  
  covar_names_all[[as.character(n)]] <- covar_names_sml |> 
        append(c(k, l, o))
}

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()
y_name <- "hr_score"
cl_names <- c("cow", "year")

### finalize ----
#### note: am keeping cow & year for id purposes, not to use them in models
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df <- lag_df[[i]] |> 
        recipe(hr_score ~ .) |> 
        update_role(c(cow, year), new_role = "id") |> 
        step_dummy(all_nominal_predictors()) |> 
        prep() |> 
        bake(new_data = NULL) |> 
        as.data.table()
      for(j in seq_along(treat_names)){
        k <- treat_names[[j]]
        l <- covar_names_all[[j]]
        
        no_interactions[[as.character(year)]][[as.character(lag)]][[as.character(k)]][[as.character(i)]] <- df |>
          select(
            all_of(
              c(y_name, cl_names, k, l)
              )
            ) |> 
          double_ml_data_from_data_frame(
            x_cols = l,
            d_cols = k,
            y_col = y_name
            )
      }
    }
  }
}

### check for zero variance ----
zerovar_1990 <- caret::nearZeroVar(
  no_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

## has interactions ----
### get initial specs ----
has_interactions <- list()

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df <- lag_df[[i]] |> 
        recipe(hr_score ~ .) |> 
        update_role(c(cow, year), new_role = "id") |> 
        step_dummy(all_nominal_predictors()) |> 
        prep() |> 
        bake(new_data = NULL) |> 
        as.data.table()
      for(j in seq_along(treat_names)){
        k <- treat_names[[j]]
        l <- interact_names[[j]]
        n <- covar_names_all[[j]]
        o <- interact_names_cre[[j]]
        p <- interact_names_sp_lag[[j]]
        q <- interact_names_sp_lag_cre[[j]]
        
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
          select(
            all_of(
              c(y_name, cl_names, k, l, n, o, p, q)
              )
            ) |> 
          double_ml_data_from_data_frame(
            x_cols = c(n, o, p, q),
            d_cols = c(k, l),
            y_col = y_name
            )
      }
    }
  }
}

### check for zero variance ----
zerovar_1990 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_slx_regr <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_slx_regr"))
gc()
