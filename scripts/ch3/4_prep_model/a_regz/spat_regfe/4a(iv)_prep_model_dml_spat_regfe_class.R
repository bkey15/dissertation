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
treat_names <- "any_inforce_X1"

## interact names ----
interact_names <- c(
  "v2x_polyarchy_x_any_inforce",
  "e_v2x_polyarchy_5C_x_any_inforce"
  )

## poly_names ----
poly_names <- c(
  "v2x_polyarchy",
  "e_v2x_polyarchy_5C"
  )

## poly_lag_names ----
poly_lag_names <- c(
  "v2x_polyarchy_sp_lag",
  "e_v2x_polyarchy_5C_sp_lag"
  )

## covar names ----
### get initial specs ----
single_treat_covars_all <- list()
multi_treat_covars_all <- list()

### finalize ----
#### standard ----
for(year in start_yrs){
  lags <- imp_sp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names_base <- imp_sp_t_dfs[[year]][[lag]][[1]] |> 
      select(-cow) |> 
      recipe(hr_score ~ .) |> 
      step_dummy(all_nominal_predictors()) |> 
      prep() |> 
      bake(new_data = NULL) |> 
      select(
        -contains(
          c(
            "n_ems",
            "any_inforce",
            "polyarchy",
            "hr_score"
            )
          )
        )|> 
      names()
    for(j in seq_along(poly_names)){
      k <- treat_names
      l <- poly_names[[j]]
      o <- poly_lag_names[[j]]
      
      covar_names <- covar_names_base |> 
        append(c(l, o))
      single_treat_covars_all[[as.character(year)]][[as.character(lag)]][[as.character(k)]][[as.character(l)]] <- covar_names
    }
  }
}

#### multi treat (manual) ----
##### main treat ----
for(year in start_yrs){
  lags <- single_treat_covars_all[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    lag_covars <- lags[[lag]]
    treats <- names(lag_covars)
    for(treat in treat_names){
      treat_covars <- lag_covars[[treat]]
      poly_stat <- names(treat_covars)
      for(j in seq_along(poly_stat)){
        k <- interact_names[[j]]
        covar_names <- treat_covars[[j]] |> 
          append(k)
        multi_treat_covars_all[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(k)]] <- covar_names
      }
    }
  }
}

##### interactions ----
for(year in start_yrs){
  lags <- single_treat_covars_all[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    lag_covars <- lags[[lag]]
    treats <- names(lag_covars)
    for(treat in treats){
      treat_covars <- lag_covars[[treat]]
      poly_stat <- names(treat_covars)
      for(j in seq_along(poly_stat)){
        k <- interact_names[[j]]
        covar_names <- treat_covars[[j]] |> 
          append(treat)
        multi_treat_covars_all[[as.character(year)]][[as.character(lag)]][[as.character(k)]][[as.character(treat)]] <- covar_names
      }
    }
  }
}

# initialize data backend ----
## single treat ----
### get initial specs ----
single_treat <- list()
y_name <- "hr_score"
cl_names <- c("region", "year")

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
        select(-cow) |> 
        recipe(hr_score ~ .) |> 
        step_dummy(all_nominal_predictors(), -cow_yr) |> 
        prep() |> 
        bake(new_data = NULL)
      df <- df_cow_yr |> 
        left_join(df_new) |> 
        select(-cow_yr) |> 
        as.data.table()
      for(treat in treat_names){
        treat_covars <- single_treat_covars_all[[year]][[lag]][[treat]]
        poly_stat <- names(treat_covars)
        for(j in seq_along(poly_stat)){
          k <- treat_covars[[j]]
          l <- poly_names[[j]]
          
          single_treat[[as.character(year)]][[as.character(lag)]][[paste(as.character(treat), as.character(l), sep = "_WITH_")]][[as.character(i)]] <- df |> 
            select(
              all_of(
                c(y_name, cl_names, treat, k)
                )
              ) |> 
            double_ml_data_from_data_frame(
              x_cols = k,
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
  single_treat[[1]][[1]][[1]][[1]]$data_model,
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
    treat_covars <- multi_treat_covars_all[[year]][[lag]]
    treats <- names(treat_covars)
    for(i in m){
      df_cow_yr <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        select(region, year, cow_yr)
      df_new <- lag_df[[i]] |> 
        mutate(cow_yr = paste(cow, year, sep = "-")) |> 
        select(-cow) |> 
        recipe(hr_score ~ .) |> 
        step_dummy(all_nominal_predictors(), -cow_yr) |> 
        prep() |> 
        bake(new_data = NULL)
      df <- df_cow_yr |> 
        left_join(df_new) |> 
        select(-cow_yr) |> 
        as.data.table()
      for(treat in treats){
        if(str_detect(treat, "inforce_X1")){
          poly_int_covars <- treat_covars[[treat]]
          poly_interacts <- names(poly_int_covars)
          for(j in seq_along(poly_interacts)){
            k <- poly_int_covars[[j]]
            l <- poly_names[[j]]
            
            multi_treat[[as.character(year)]][[as.character(lag)]][[paste(as.character(treat), as.character(l), sep = "_WITH_")]][[as.character(i)]] <- df |> 
              select(
                all_of(
                  c(y_name, cl_names, treat, k)
                  )
                ) |> 
              double_ml_data_from_data_frame(
                x_cols = k,
                d_cols = treat,
                y_col = y_name,
                cluster_cols = cl_names
                )
          }
        }
        else{
          covar_names <- treat_covars[[treat]][[1]]
          
          multi_treat[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |> 
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
}

### check for zero variance ----
zerovar_1990 <- caret::nearZeroVar(
  multi_treat[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_spat_regfe <- list(
  single_treat = single_treat,
  multi_treat = multi_treat
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_spat_regfe"))
