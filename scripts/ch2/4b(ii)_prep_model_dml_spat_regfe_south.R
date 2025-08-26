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
load(here("data/ch2/results/imputations/imp_sp_t_lags.rda"))

# truncate data ----
## IMPORTANT: leaving out start_1968 date for now (reduces computation time & helps computer memory)
imp_sp_t_lags <- imp_sp_t_lags[-1]

# get imputed datasets ----
## IMPORTANT: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
## IMPORTANT: drop out unused region levels. Unwanted columns will appear after initializing model.matrix otherwise.
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
        filter(
          glb_s == 1,
          .imp == i
          ) |> 
        select(
          -contains("nn_"),
          -glb_s,
          -cow,
          -last_col(),
          -last_col(offset = 1)
          ) |> 
        mutate(region = droplevels(region))
    
      imp_sp_t_dfs[[as.character(year)]][[as.character(lag)]][[as.character(i)]] <- imp_df
    }
  }
}

# get main specs ----
## treat names ----
treat_names_gen <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    n_bits,
    starts_with("partner_"),
    -contains("sp_lag")
    ) |> 
  names()

treat_names_ss <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ss_"),
    -contains("sp_lag")
    ) |> 
  names()

treat_names_ns <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("ns_"),
    -contains("sp_lag")
    ) |> 
  names()

## interact names ----
interact_names_gen <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("e_polity2_x") & !contains(c("_ss_", "_ns_", "sp_lag"))
    ) |> 
  names()

interact_names_ss <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("e_polity2_x_ss"),
    -contains("sp_lag")
    ) |> 
  names()

interact_names_ns <- imp_sp_t_dfs[[1]][[1]][[1]] |> 
  select(
    starts_with("e_polity2_x_ns"),
    -contains("sp_lag")
    ) |> 
  names()

## covar names ----
### get initial specs ----
covar_names_all <- list()
covar_names_sml <- list()


### finalize ----
#### important: dropping first column after creating matrix to ensure first level of factor (region) isn't included in the lassos.
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
            "any_inforce",
            "ss_",
            "ns_"
            )
          )
        )|> 
      names()
  
    covar_names_all[[year]][[as.character(lag)]] <- covar_names
    covar_names_sml[[year]][[as.character(lag)]] <- covar_names[!covar_names == "n_bits_sp_lag"]
  }
}

# initialize data backend ----
## no interactions ----
### get initial specs ----
no_interactions <- list()
start_yrs <- imp_sp_t_dfs |> 
  names()

### finalize ----
#### general ----
#### i.e., no consideration for ns, ss bits
for(year in start_yrs){
  year_dfs <- imp_sp_t_dfs[[year]]
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
          covar_names <- covar_names_sml[[year]][[lag]]
          
          no_interactions[[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = treat,
              y_col = "hr_score"
              )
        }
        else{
          covar_names <- covar_names_all[[year]][[lag]]
          
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

#### partner-type (ss & ns) ----
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
      for(j in seq_along(treat_names_ss)){
        k <- treat_names_ss[[j]]
        l <- treat_names_ns[[j]]
        if(k == "ss_n_bits"){
          covar_names <- covar_names_sml[[year]][[lag]]
          
          no_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = c(k, l),
              y_col = "hr_score"
            )
        }
        else{
          covar_names <- covar_names_all[[year]][[lag]]
          
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
  year_dfs <- imp_sp_t_dfs[[year]]
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
          covar_names <- covar_names_sml[[year]][[lag]]
          
          has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = c(k, l),
              y_col = "hr_score"
              )
        }
        else{
          covar_names <- covar_names_all[[year]][[lag]]
          
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

#### partner-type (ss & ns) ----
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
      for(j in seq_along(treat_names_ss)){
        k <- treat_names_ss[[j]]
        l <- treat_names_ns[[j]]
        n <- interact_names_ss[[j]]
        o <- interact_names_ns[[j]]
        if(k == "ss_n_bits"){
          covar_names <- covar_names_sml[[year]][[lag]]
          
          has_interactions[[as.character(year)]][[as.character(lag)]][[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |>
            double_ml_data_from_data_frame(
              x_cols = covar_names,
              d_cols = c(k, l, n, o),
              y_col = "hr_score"
            )
        }
        else{
          covar_names <- covar_names_all[[year]][[lag]]
          
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
imp_dml_dats_spat_regfe_south <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# clear glb env ----
rm(list = setdiff(ls(), "imp_dml_dats_spat_regfe_south"))
