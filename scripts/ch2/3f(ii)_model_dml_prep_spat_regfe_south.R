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
load(here("data/ch2/results/imputations/imp_1962_sp_l1.rda"))
load(here("data/ch2/results/imputations/imp_1981_sp_l1.rda"))
load(here("data/ch2/results/imputations/imp_1990_sp_l1.rda"))

# get imputed datasets ----
## 1962 ----
## IMPORTANT: filter out unused treatments before initializing covar_names. These are sources of high missingness that will cause model.matrix to produce empty sets.
## IMPORTANT: drop out unused region levels. Unwanted columns will appear after initializing model.matrix otherwise.

imp_1962_dfs <- list()
m <- 1:imp_1962_sp_l1$m

for(i in m){
  imp_df <- imp_1962_sp_l1 |> 
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
      -cow,
      -last_col(),
      -last_col(offset = 1)
    ) |> 
    mutate(region = droplevels(region))
  
  imp_1962_dfs[[as.character(i)]] <- imp_df
}

## 1981 ----
imp_1981_dfs <- list()

for(i in m){
  imp_df <- imp_1981_sp_l1 |> 
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
      -cow,
      -last_col(),
      -last_col(offset = 1)
    ) |> 
    mutate(region = droplevels(region))
  
  imp_1981_dfs[[as.character(i)]] <- imp_df
}

## 1990 ----
imp_1990_dfs <- list()

for(i in m){
  imp_df <- imp_1990_sp_l1 |> 
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
      -cow,
      -last_col(),
      -last_col(offset = 1)
    ) |> 
    mutate(region = droplevels(region))
  
  imp_1990_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## no interactions ----
### 1962 ----
#### get initial specs ----
### important: dropping first column after creating matrix to ensure first level of factor (region) isn't included in the lassos.
covar_names_1962 <- model.matrix(
  ~ . - 1, data = imp_1962_dfs[[1]]
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
        "ss_",
        "ns_"
        )
      )
    )|> 
  names()

covar_names_1962_sml <- covar_names_1962[!covar_names_1962 == "n_bits_sp_lag"]

treat_names_gen <- imp_1962_dfs[[1]] |> 
  select(
    n_bits,
    starts_with("partner_"),
    -contains("sp_lag")
    ) |> 
  names()

treat_names_ss <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("ss_"),
    -contains("sp_lag")
    ) |> 
  names()

treat_names_ns <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("ns_"),
    -contains("sp_lag")
    ) |> 
  names()

start_1962 <- list()

#### finalize ----
##### general ----
##### i.e., no consideration for ns, ss bits
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names_gen){
    if(name == "n_bits"){
      start_1962[[as.character(name)]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962_sml,
          d_cols = name,
          y_col = "hr_score"
        )
      }
    else{
      start_1962[[as.character(name)]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962,
          d_cols = name,
          y_col = "hr_score"
        )
    }
  }
  }

##### partner-type (ss & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss)){
    k <- treat_names_ss[[j]]
    l <- treat_names_ns[[j]]
    if(k == "ss_n_bits"){
      start_1962[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962_sml,
          d_cols = c(k, l),
          y_col = "hr_score"
          )
    }
    else{
      start_1962[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962,
          d_cols = c(k, l),
          y_col = "hr_score"
      )
    }
  }
}

### 1981 ----
#### get initial specs ----
covar_names_1981 <- model.matrix(
  ~ . - 1, data = imp_1981_dfs[[1]]
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
        "ss_",
        "ns_"
      )
    )
  )|> 
  names()

covar_names_1981_sml <- covar_names_1981[!covar_names_1981 == "n_bits_sp_lag"]

treat_names_gen <- imp_1981_dfs[[1]] |> 
  select(
    n_bits,
    starts_with("partner_"),
    -contains("sp_lag")
  ) |> 
  names()

treat_names_ss <- imp_1981_dfs[[1]] |> 
  select(
    starts_with("ss_"),
    -contains("sp_lag")
  ) |> 
  names()

treat_names_ns <- imp_1981_dfs[[1]] |> 
  select(
    starts_with("ns_"),
    -contains("sp_lag")
  ) |> 
  names()

start_1981 <- list()

#### finalize ----
##### general ----
##### i.e., no consideration for ns, ss bits
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names_gen){
    if(name == "n_bits"){
      start_1981[[as.character(name)]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981_sml,
          d_cols = name,
          y_col = "hr_score"
        )
    }
    else{
      start_1981[[as.character(name)]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981,
          d_cols = name,
          y_col = "hr_score"
        )
    }
  }
}

##### partner-type (ss & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss)){
    k <- treat_names_ss[[j]]
    l <- treat_names_ns[[j]]
    if(k == "ss_n_bits"){
      start_1981[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981_sml,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
    else{
      start_1981[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
  }
}

### 1990 ----
#### get initial specs ----
covar_names_1990 <- model.matrix(
  ~ . - 1, data = imp_1990_dfs[[1]]
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
        "ss_",
        "ns_"
      )
    )
  )|> 
  names()

covar_names_1990_sml <- covar_names_1990[!covar_names_1990 == "n_bits_sp_lag"]

treat_names_gen <- imp_1990_dfs[[1]] |> 
  select(
    n_bits,
    starts_with("partner_"),
    -contains("sp_lag")
  ) |> 
  names()

treat_names_ss <- imp_1990_dfs[[1]] |> 
  select(
    starts_with("ss_"),
    -contains("sp_lag")
  ) |> 
  names()

treat_names_ns <- imp_1990_dfs[[1]] |> 
  select(
    starts_with("ns_"),
    -contains("sp_lag")
  ) |> 
  names()

start_1990 <- list()

#### finalize ----
##### general ----
##### i.e., no consideration for ns, ss bits
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(name in treat_names_gen){
    if(name == "n_bits"){
      start_1990[[as.character(name)]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990_sml,
          d_cols = name,
          y_col = "hr_score"
        )
    }
    else{
      start_1990[[as.character(name)]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990,
          d_cols = name,
          y_col = "hr_score"
        )
    }
  }
}

##### partner-type (ss & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss)){
    k <- treat_names_ss[[j]]
    l <- treat_names_ns[[j]]
    if(k == "ss_n_bits"){
      start_1990[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990_sml,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
    else{
      start_1990[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
  }
}

### check for zero variance ----
zerovar_1962 <- caret::nearZeroVar(
  start_1962[[1]][[1]]$data_model,
  saveMetrics = T
)

zerovar_1981 <- caret::nearZeroVar(
  start_1981[[1]][[1]]$data_model,
  saveMetrics = T
)

zerovar_1990 <- caret::nearZeroVar(
  start_1990[[1]][[1]]$data_model,
  saveMetrics = T
)

### combine ----
no_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

## has interactions ----
### 1962 ----
#### get initial specs ----
interact_names_gen <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("e_polity2_x") & !contains(c("_ss_", "_ns_", "sp_lag"))
  ) |> 
  names()

interact_names_ss <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("e_polity2_x_ss"),
    -contains("sp_lag")
  ) |> 
  names()

interact_names_ns <- imp_1962_dfs[[1]] |> 
  select(
    starts_with("e_polity2_x_ns"),
    -contains("sp_lag")
  ) |> 
  names()

start_1962 <- list()

#### finalize ----
##### general ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_gen)){
    k <- treat_names_gen[[j]]
    l <- interact_names_gen[[j]]
    if(k == "n_bits"){
      start_1962[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962_sml,
          d_cols = c(k, l),
          y_col = "hr_score"
          )
    }
    else{
      start_1962[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
  }
  }

##### partner-type (ss & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1962_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss)){
    k <- treat_names_ss[[j]]
    l <- treat_names_ns[[j]]
    n <- interact_names_ss[[j]]
    o <- interact_names_ns[[j]]
    if(k == "ss_n_bits"){
      start_1962[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962_sml,
          d_cols = c(k, l, n, o),
          y_col = "hr_score"
        )
    }
    else{
      start_1962[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names_1962,
          d_cols = c(k, l, n, o),
          y_col = "hr_score"
        )
    }
  }
  }

### 1981 ----
#### get initial specs ----
start_1981 <- list()

#### finalize ----
##### general ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_gen)){
    k <- treat_names_gen[[j]]
    l <- interact_names_gen[[j]]
    if(k == "n_bits"){
      start_1981[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981_sml,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
    else{
      start_1981[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
  }
}

##### partner-type (ss & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1981_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss)){
    k <- treat_names_ss[[j]]
    l <- treat_names_ns[[j]]
    n <- interact_names_ss[[j]]
    o <- interact_names_ns[[j]]
    if(k == "ss_n_bits"){
      start_1981[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981_sml,
          d_cols = c(k, l, n, o),
          y_col = "hr_score"
        )
    }
    else{
      start_1981[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names_1981,
          d_cols = c(k, l, n, o),
          y_col = "hr_score"
        )
    }
  }
}

### 1990 ----
#### get initial specs ----
start_1990 <- list()

#### finalize ----
##### general ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_gen)){
    k <- treat_names_gen[[j]]
    l <- interact_names_gen[[j]]
    if(k == "n_bits"){
      start_1990[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990_sml,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
    else{
      start_1990[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990,
          d_cols = c(k, l),
          y_col = "hr_score"
        )
    }
  }
}

##### partner-type (ss & ns) ----
for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_1990_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss)){
    k <- treat_names_ss[[j]]
    l <- treat_names_ns[[j]]
    n <- interact_names_ss[[j]]
    o <- interact_names_ns[[j]]
    if(k == "ss_n_bits"){
      start_1990[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990_sml,
          d_cols = c(k, l, n, o),
          y_col = "hr_score"
        )
    }
    else{
      start_1990[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
        double_ml_data_from_data_frame(
          x_cols = covar_names_1990,
          d_cols = c(k, l, n, o),
          y_col = "hr_score"
        )
    }
  }
}

### check for zero variance ----
zerovar_1962 <- caret::nearZeroVar(
  start_1962[[1]][[1]]$data_model,
  saveMetrics = T
)

zerovar_1981 <- caret::nearZeroVar(
  start_1981[[1]][[1]]$data_model,
  saveMetrics = T
)

zerovar_1990 <- caret::nearZeroVar(
  start_1990[[1]][[1]]$data_model,
  saveMetrics = T
)

### combine ----
has_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# all combine ----
imp_dml_dats_spat_regfe_south <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )

# save initialized data ----
imp_dml_dats_spat_regfe_south |> 
  save(file = here("data/ch2/results/fits/dml_lasso/dml_initial/imp_dml_dats_spat_regfe_south.rda"))
