# Prep ridge models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(tidymodels)
library(data.table)
library(janitor)
library(reticulate)

# load data ----
load(here("data/ch3/results/imputations/imp_t_lags.rda"))

# load python modules ----
py_require(packages = "doubleml")
py_require(packages = "scikit-learn")
py_require(packages = "joblib")
py_require(packages = "numpy")

dml <- import("doubleml")
skl <- import("sklearn")
jlb <- import("joblib")
np <- import("numpy")

# get imputed data ----
## note: code below is drawn from earlier chapters; can be used in the event other start-dates are utilized
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
        filter(.imp == i) |> 
        select(
          -last_col(),
          -last_col(offset = 1)
          )
      
      imp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
    }
  }
}

# get main specs ----
## treat names ----
treat_names <- c(
  "any_inforce_X1",
  "v2x_polyarchy_x_any_inforce"
  )

## covar names ----
### get initial specs ----
covar_names_all <- list()

### finalize ----
#### standard ----
for(year in start_yrs){
  lags <- imp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names <- imp_t_dfs[[year]][[lag]][[1]] |> 
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
    
    covar_names_all[["std"]][[as.character(year)]][[as.character(lag)]] <- covar_names
  }
}

#### multi treat manual ----
man_stat <- covar_names_all |> 
  names()

for(stat in man_stat){
  years <- covar_names_all[[stat]]
  year_names <- names(years)
  for(year in year_names){
    lags <- years[[year]]
    lag_names <- names(lags)
    for(lag in lag_names){
      lag_covars <- lags[[lag]]
      for(treat in treat_names){
        if(str_detect(treat, "inforce_X1")){
          covar_names <- lag_covars |> 
            append(str_subset(treat_names, "polyarchy"))
          covar_names_all[["multi_man"]][[as.character(treat)]][[as.character(year)]][[as.character(lag)]] <- covar_names
        }
        else if(str_detect(treat, "polyarchy")){
          covar_names <- lag_covars |> 
            append(str_subset(treat_names, "inforce_X1"))
          covar_names_all[["multi_man"]][[as.character(treat)]][[as.character(year)]][[as.character(lag)]] <- covar_names
        }
      }
    }
  }
}

# initialize data backend ----
## single treat ----
### get initial specs ----
single_treat <- list()
y_name <- "hr_score"
cl_names <- c("cow", "year")

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[["std"]][[year]][[lag]]
    for(i in m){
      df_cow_yr <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        select(cow, year, cow_yr)
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
        if(str_detect(treat, "inforce_X1")){
        single_treat[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |> 
          select(
            all_of(
              c(y_name, cl_names, treat, covar_names)
              )
            ) |> 
          dml$DoubleMLClusterData(
            x_cols = covar_names,
            d_cols = treat,
            y_col = y_name,
            cluster_cols = cl_names
            )
        }
      }
    }
  }
}

### check for zero variance ----
zerovar_1990 <- caret::nearZeroVar(
  single_treat[[1]][[1]][[1]][[1]][["_X"]],
  saveMetrics = T
  )

## multi treat ----
### get initial specs ----
multi_treat <- list()

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df_cow_yr <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        select(cow, year, cow_yr)
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
          covar_names <- covar_names_all[["multi_man"]][[treat]][[year]][[lag]]
          multi_treat[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |> 
            select(
              all_of(
                c(y_name, cl_names, treat, covar_names)
                )
              ) |> 
            dml$DoubleMLClusterData(
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
  multi_treat[[1]][[1]][[1]][[1]][["_X"]],
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_2fe <- list(
  single_treat = single_treat,
  multi_treat = multi_treat
  )

# clear glb env ----
rm(list = setdiff(ls(), c("imp_dml_dats_2fe", "dml", "skl", "jlb", "np")))
