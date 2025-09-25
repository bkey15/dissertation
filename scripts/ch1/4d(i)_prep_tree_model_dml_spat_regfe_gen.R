# Prep tree models across all imputed datasets.
# IMPORTANT: we're not using the spatially-lagged versions of the treatments as covariates (for now) because this introduces the same endogeneity problems from which SAR suffers, specifically when the treatment(s) is/are regressed on the covariates (d ~ X).
# IMPORTANT: difference here is we're including a region fixed effect (following Wimpy, Whitten, Williams)

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch1/results/imputations/imp_sp_t_lags.rda"))

# truncate data ----
## IMPORTANT: leaving out start_1968 date for now (reduces computation time & helps computer memory)
imp_sp_t_lags <- imp_sp_t_lags[-1]

# get imputed data ----
## important: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
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
          -contains(
            c(
              "ss_",
              "ns_",
              "nn_",
              "cpr",
              "esr"
              )
            ),
          -glb_s,
          -cow,
          -last_col(),
          -last_col(offset = 1)
          )
      
      imp_sp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
    }
  }
}

# get main specs ----
## treat names ----
treat_names <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("lech_hr"),
    -ends_with("sp_lag")
    ) |> 
  names()

## interact names ----
interact_names <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(starts_with("v2x_polyarchy_x_lech")) |> 
  names()

## covar names ----
### get initial specs ----
dep_enf_ids <- c("mean", "gdp_mean", "gdppc_mean")

depth_names_gen_orig <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("depth"),
    -ends_with("sp_lag")
    ) |> 
  names()

depth_names_gen_sp_lag <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("depth") & ends_with("sp_lag")
    ) |> 
  names()

enforce_names_gen_orig <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("enforce"),
    -ends_with("sp_lag")
    ) |> 
  names()

enforce_names_gen_sp_lag <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("enforce") & ends_with("sp_lag")
    ) |> 
  names()

covar_names_gen_sml <- list()
covar_names_gen_all <- list()

### finalize ----
### important: dropping first column after creating matrix to ensure first level of factor (region) isn't included in the trees.
for(year in start_yrs){
  lags <- imp_sp_t_dfs[[year]]
  lag_names <- names(lags)
  for(lag in lag_names){
    covar_names <- imp_sp_t_dfs[[year]][[lag]][[1]] |> 
      select(
        -contains(
          c(
            "hr_score",
            "mean",
            "v2x_polyarchy_x"
            )
          )
        ) |> 
      names()
    for(j in seq_along(depth_names_gen_orig)){
      k <- depth_names_gen_orig[[j]]
      l <- enforce_names_gen_orig[[j]]
      n <- dep_enf_ids[[j]]
      o <- depth_names_gen_sp_lag[[j]]
      p <- enforce_names_gen_sp_lag[[j]]
      
      covar_names_gen_sml[[as.character(n)]] <- covar_names |> 
        append(c(k, l, o, p))
      
      covar_names_gen_all[[as.character(year)]][[as.character(lag)]] <- covar_names_gen_sml
    }
  }
}

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()

### finalize ----
for(year in start_yrs){
  year_dfs <- imp_sp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_gen_all[[year]][[lag]]
    for(i in m){
      df <- lag_df[[i]]
      for(j in seq_along(treat_names)){
        k <- treat_names[[j]]
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

### check for zero variance ----
zerovar_1977 <- caret::nearZeroVar(
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
for(year in start_yrs){
  year_dfs <- imp_sp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    covar_names <- covar_names_gen_all[[year]][[lag]]
    for(i in m){
      df <- lag_df[[i]]
      for(j in seq_along(treat_names)){
        k <- treat_names[[j]]
        l <- interact_names[[j]]
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
imp_dml_dats_spat_regfe_gen <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_spat_regfe_gen"))
