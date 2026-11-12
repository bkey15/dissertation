# Prep ridge models across all imputed datasets.
# Model type: SLX

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(tidymodels)
library(data.table)
library(janitor)

# load data ----
load(here("data/ch1/results/imputations/imp_t_lags.rda"))

# truncate data ----
## IMPORTANT: leaving out start_1968 date for now (reduces computation time & helps computer memory)
imp_t_lags <- imp_t_lags[-1]

# get imputed data ----
## important: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
m <- 1:imp_t_lags[[1]][[1]]$m
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
          # note: code helps prevent vars like `v2xeg_eqaccess_cre` from being dropped
          -starts_with(
            c(
              "ss_",
              "ns_",
              "nn_"
              )
            ),
          -contains(
            c(
              "_ss_",
              "_ns_",
              "_nn_",
              "cpr",
              "esr",
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
treat_names_gen <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("lech_hr"),
    -ends_with(c("cre", "sp_lag"))
    ) |> 
  names()

## interact names ----
interact_names_gen <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_lech"),
    -ends_with(c("cre", "sp_lag"))
    ) |> 
  names()

## covar names ----
### get initial specs ----
treat_ids <- c("mean", "gdp_mean", "gdppc_mean")

#### depth
depth_names_gen <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("depth"),
    -ends_with(c("cre", "sp_lag"))
    ) |> 
  names()

depth_names_gen_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("depth") & ends_with("cre") & -contains("sp_lag")
    ) |> 
  names()

depth_names_gen_sp_lag <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("depth") & ends_with("sp_lag")
    ) |> 
  names()

depth_names_gen_sp_lag_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("depth") & ends_with("sp_lag_cre")
    ) |> 
  names()

#### enforce
enforce_names_gen <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("enforce"),
    -ends_with(c("cre", "sp_lag"))
    ) |> 
  names()

enforce_names_gen_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("enforce") & ends_with("cre") & -contains("sp_lag")
    ) |> 
  names()

enforce_names_gen_sp_lag <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("enforce") & ends_with("sp_lag")
    ) |> 
  names()

enforce_names_gen_sp_lag_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("enforce") & ends_with("sp_lag_cre")
    ) |> 
  names()

#### lech
lech_names_gen_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("lech") & ends_with("cre") & -contains("sp_lag")
    ) |> 
  names()

lech_names_gen_sp_lag <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("lech") & ends_with("sp_lag")
    ) |> 
  names()

lech_names_gen_sp_lag_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("lech") & ends_with("sp_lag_cre")
    ) |> 
  names()

#### interact
interact_names_gen_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_lech") & ends_with("cre") & -contains("sp_lag")
    ) |> 
  names()

interact_names_gen_sp_lag <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_lech") & ends_with("sp_lag")
    ) |> 
  names()

interact_names_gen_sp_lag_cre <- imp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("v2x_polyarchy_x_lech") & ends_with("sp_lag_cre")
    ) |> 
  names()

### finalize ----
covar_names_gen_all <- list()

covar_names_sml <- imp_t_dfs$start_1977$l1$`1` |> 
  select(
    -c(cow, year)
    ) |> 
  select(
    -contains(
      c("mean", "v2x_polyarchy_x")
      ),
    -hr_score,
    -hr_score_cre
    ) |> 
  names()

for(j in seq_along(treat_ids)){
  k <- depth_names_gen[[j]]
  l <- enforce_names_gen[[j]]
  n <- treat_ids[[j]]
  o <- depth_names_gen_cre[[j]]
  p <- enforce_names_gen_cre[[j]]
  q <- lech_names_gen_cre[[j]]
  r <- depth_names_gen_sp_lag[[j]]
  s <- enforce_names_gen_sp_lag[[j]]
  t <- lech_names_gen_sp_lag[[j]]
  u <- depth_names_gen_sp_lag_cre[[j]]
  v <- enforce_names_gen_sp_lag_cre[[j]]
  w <- lech_names_gen_sp_lag_cre[[j]]
  
  covar_names_gen_all[[as.character(n)]] <- covar_names_sml |> 
        append(c(k, l, o, p, q, r, s, t, u, v, w))
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
        as.data.table()
      for(j in seq_along(treat_names_gen)){
        k <- treat_names_gen[[j]]
        l <- covar_names_gen_all[[j]]
        
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
  year_dfs <- imp_t_dfs[[year]]
  for(lag in lag_names){
    lag_df <- year_dfs[[lag]]
    for(i in m){
      df <- lag_df[[i]] |> 
        as.data.table()
      for(j in seq_along(treat_names_gen)){
        k <- treat_names_gen[[j]]
        l <- interact_names_gen[[j]]
        n <- covar_names_gen_all[[j]]
        o <- interact_names_gen_cre[[j]]
        p <- interact_names_gen_sp_lag[[j]]
        q <- interact_names_gen_sp_lag_cre[[j]]
        
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
zerovar_1977 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

zerovar_1990 <- caret::nearZeroVar(
  has_interactions[[1]][[1]][[1]][[1]]$data_model,
  saveMetrics = T
  )

# all combine ----
imp_dml_dats_slx_gen <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_slx_gen"))
gc()
