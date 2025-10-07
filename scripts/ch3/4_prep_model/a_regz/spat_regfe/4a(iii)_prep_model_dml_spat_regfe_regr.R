# Prep ridge models across all imputed datasets.
# IMPORTANT: we're not using the spatially-lagged versions of the treatments as covariates (for now) because this introduces the same endogeneity problems from which SAR suffers, specifically when the treatment(s) is/are regressed on the covariates (d ~ X).
# IMPORTANT: difference here is we're including a region fixed effect (following Wimpy, Whitten, Williams)

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(tidymodels)
library(data.table)
library(janitor)

# load data ----
load(here("data/ch3/results/imputations/imp_sp_t_lags.rda"))

# get imputed data ----
## note: code below is drawn from earlier chapters; can be used in the event other start-dates are utilized
m <- 1:imp_sp_t_lags[[1]][[1]]$m
start_yrs <- names(imp_sp_t_lags)
imp_sp_t_dfs <- list()

for(year in start_yrs){
  lags <- imp_sp_t_lags[[year]]
  lag_names <- names(lags)
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
          -last_col(),
          -last_col(offset = 1)
          )
      
      imp_sp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
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
covar_names_all <- list()

### finalize ----
#### standard ----
for(year in start_yrs){
  lags <- imp_sp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names <- imp_sp_t_dfs[[year]][[lag]][[1]] |> 
      recipe(hr_score ~ .) |> 
      step_dummy(all_nominal_predictors()) |> 
      prep() |> 
      bake(new_data = NULL) |> 
      select(
        -contains(
          c(
            "n_ems",
            "any_inforce",
            "hr_score"
            )
          )
        )|> 
    names()
    
    covar_names_all[[as.character(year)]][[as.character(lag)]] <- covar_names
  }
}

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()
y_name <- "hr_score"
cl_names <- c("region", "year")

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_sp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[[year]][[lag]]
    for(i in m){
      df_cow_yr <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        select(region, year, cow_yr)
      df_new <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        recipe(hr_score ~ .) |> 
        step_dummy(all_nominal_predictors(), -cow_yr) |> 
        prep() |> 
        bake(new_data = NULL)
      df <- df_cow_yr |> 
        left_join(df_new) |> 
        select(-cow_yr) |> 
        as.data.table()
      for(treat in treat_names){
        no_interactions[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |>
          select(
            all_of(
              c(y_name, cl_names, treat, covar_names)
              )
            ) |> 
          double_ml_data_from_data_frame(
            x_cols = covar_names,
            d_cols = treat,
            y_col = y_name,
            cluster_cols = cl_names
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
  year_dfs <- imp_sp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df_cow_yr <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        select(region, year, cow_yr)
      df_new <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        recipe(hr_score ~ .) |> 
        step_dummy(all_nominal_predictors(), -cow_yr) |> 
        prep() |> 
        bake(new_data = NULL)
      df <- df_cow_yr |> 
        left_join(df_new) |> 
        select(-cow_yr) |> 
        as.data.table()
      for(j in seq_along(treat_names)){
        k <- treat_names[[j]]
        l <- interact_names[[j]]
        has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |> 
          select(
            all_of(
              c(y_name, cl_names, k, l, covar_names)
              )
            ) |> 
          double_ml_data_from_data_frame(
            x_cols = covar_names,
            d_cols = c(k, l),
            y_col = y_name,
            cluster_cols = cl_names
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

# all combine ----
imp_dml_dats_spat_regfe <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_spat_regfe"))
