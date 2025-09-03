# Prep lasso models across all imputed datasets.
# IMPORTANT: we're not using the spatially-lagged versions of the treatments as covariates (for now) because this introduces the same endogeneity problems from which SAR suffers, specifically when the treatment(s) is/are regressed on the covariates (d ~ X).
# IMPORTANT: difference here is we're including a region fixed effect (following Wimpy, Whitten, Williams)

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(data.table)
library(reticulate)

# load data ----
load(here("data/ch3/results/imputations/imp_sp_t_lags.rda"))

# load python modules ----
py_require(packages = "doubleml")
py_require(packages = "scikit-learn")
py_require(packages = "joblib")

dml <- import("doubleml")
skl <- import("sklearn")
jlb <- import("joblib")

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
          -last_col(offset = 1),
          -cow
          )
      
      imp_sp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
    }
  }
}

# get main specs ----
## treat names ----
treat_names <- c(
  "n_ems",
  "any_inforce1",
  "v2x_polyarchy_x_any_inforce"
  )

## interact names ----
interact_names <- "v2x_polyarchy_x_n_ems"

## covar names ----
### get initial specs ----
covar_names_all <- list()

### finalize ----
#### important: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
#### important: dropping first column after creating matrix to ensure first level of factor (cow) isn't included in the lasso
#### standard ----
for(year in start_yrs){
  lags <- imp_sp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names <- model.matrix(
      ~ . - 1,
      data = imp_sp_t_dfs[[year]][[lag]][[1]]
      ) |> 
      as_tibble() |> 
      select(
        -1,
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
        if(str_detect(treat, "inforce1")){
          covar_names <- lag_covars |> 
            append(str_subset(treat_names, "polyarchy"))
          covar_names_all[["multi_man"]][[as.character(treat)]][[as.character(year)]][[as.character(lag)]] <- covar_names
        }
        else if(str_detect(treat, "polyarchy")){
          covar_names <- lag_covars |> 
            append(str_subset(treat_names, "inforce1"))
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

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_sp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_all[["std"]][[year]][[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(treat in treat_names){
        if(str_detect(treat, "polyarchy", negate = TRUE)){
        single_treat[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |>
          dml$DoubleMLData(
            x_cols = covar_names,
            d_cols = treat,
            y_col = "hr_score"
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
  year_dfs <- imp_sp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df <- model.matrix(
        ~ . - 1,
        data = lag_df[[i]]
        ) |> 
        as.data.table()
      for(treat in treat_names){
        if(treat == "n_ems"){
          covar_names <- covar_names_all[["std"]][[year]][[lag]]
          k <- treat
          l <- interact_names
          multi_treat[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
            dml$DoubleMLData(
              x_cols = covar_names,
              d_cols = c(k, l),
              y_col = "hr_score"
              )
        }
        else{
          covar_names <- covar_names_all[["multi_man"]][[treat]][[year]][[lag]]
          multi_treat[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |>
            dml$DoubleMLData(
              x_cols = covar_names,
              d_cols = treat,
              y_col = "hr_score"
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

# all combine ----
imp_dml_dats_spat_regfe <- list(
  single_treat = single_treat,
  multi_treat = multi_treat
  )

# clear glb env ----
rm(list = setdiff(ls(), c("imp_dml_dats_spat_regfe", "dml", "skl", "jlb")))
