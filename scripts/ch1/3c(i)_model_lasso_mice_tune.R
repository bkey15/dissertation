# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(glmnet)
library(naniar)
library(parallel)
library(doMC)

# load data ----
load(here("data/ch1/results/imputations/imp_1_l1.rda"))
load(here("data/ch1/results/imputations/imp_2_l1.rda"))
load(here("data/ch1/results/imputations/imp_3_l1.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# get imputations ----
## imp_1 (no start year) ----
imp_1_dfs <- list()
m <- 1:5

for(i in m){
  imp_df <- imp_1_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
      ) |> 
    filter(.imp == i) |> 
    select(
      -last_col(),
      -last_col(offset = 1)
      )
  
  imp_1_dfs[[as.character(i)]] <- imp_df
}

## imp_2 (1968) ----
imp_2_dfs <- list()
m <- 1:5

for(i in m){
  imp_df <- imp_2_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(.imp == i) |> 
    select(
      -last_col(),
      -last_col(offset = 1)
    )
  
  imp_2_dfs[[as.character(i)]] <- imp_df
}

## imp_3 (1977) ----
imp_3_dfs <- list()
m <- 1:5

for(i in m){
  imp_df <- imp_3_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(.imp == i) |> 
    select(
      -last_col(),
      -last_col(offset = 1)
    )
  
  imp_3_dfs[[as.character(i)]] <- imp_df
}

# prep ----
## imp_1 ----
### get hr_score ----
dep_1 <- imp_1_dfs[[1]] |> 
  select(hr_score) |> 
  mutate(row = 1:nrow(imp_1_dfs[[1]]))

### get missing rows ----
imp_1.na <- imp_1_dfs[[1]] |> 
  where_na() |> 
  as_tibble() |> 
  select(row) |> 
  unique()

### filter out missing rows from hr_score ----
dep_1 <- dep_1 |> 
  anti_join(imp_1.na)
dep_1 <- dep_1$hr_score

### get covars ----
#### cpr & esr ----
treat_names <- imp_1_dfs[[1]] |> 
  select(5:7, 9:11) |> 
  names()
m <- 1:5
imp_1_covars <- list()

for(i in m){
  df <- imp_1_dfs[[i]]
  for(name in treat_names){
    df_small <- df |> 
      select(!3:4 & !8 & !12 & !setdiff(treat_names, name))
    imp_1_covars[[as.character(name)]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
  }
}

#### both ----
#### cpr_mean & esr_mean
for(i in m){
  df_small <- imp_1_dfs[[i]] |> 
    select(!3:4 & !6:8 & !10:12)
  imp_1_covars[["both_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

#### cpr_gdp & esr_gdp
for(i in m){
  df_small <- imp_1_dfs[[i]] |> 
    select(!3:5 & !7:9 & !11:12)
  imp_1_covars[["both_gdp_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

#### cpr_gdppc & esr_gdppc
for(i in m){
  df_small <- imp_1_dfs[[i]] |> 
    select(!3:6 & !8:10 & !12)
  imp_1_covars[["both_gdppc_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

## imp_2 ----
### get hr_score ----
dep_2 <- imp_2_dfs[[1]] |> 
  select(hr_score) |> 
  mutate(row = 1:nrow(imp_2_dfs[[1]]))

### get missing rows ----
imp_2.na <- imp_2_dfs[[1]] |> 
  select(-inforce) |> 
  where_na() |> 
  as_tibble() |> 
  select(row) |> 
  unique()

### filter out missing rows from hr_score ----
dep_2 <- dep_2 |> 
  anti_join(imp_2.na)
dep_2 <- dep_2$hr_score

### get covars ----
#### cpr & esr ----
treat_names <- imp_2_dfs[[1]] |> 
  select(5:7, 9:11) |> 
  names()
m <- 1:5
imp_2_covars <- list()

for(i in m){
  df <- imp_2_dfs[[i]]
  for(name in treat_names){
    df_small <- df |> 
      select(!3:4 & !8 & !12 & !setdiff(treat_names, name))
    imp_2_covars[[as.character(name)]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
  }
}

#### both ----
#### cpr_mean & esr_mean
for(i in m){
  df_small <- imp_2_dfs[[i]] |> 
    select(!3:4 & !6:8 & !10:12)
  imp_2_covars[["both_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

#### cpr_gdp & esr_gdp
for(i in m){
  df_small <- imp_2_dfs[[i]] |> 
    select(!3:5 & !7:9 & !11:12)
  imp_2_covars[["both_gdp_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

#### cpr_gdppc & esr_gdppc
for(i in m){
  df_small <- imp_2_dfs[[i]] |> 
    select(!3:6 & !8:10 & !12)
  imp_2_covars[["both_gdppc_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

## imp_3 ----
### get hr_score ----
dep_3 <- imp_3_dfs[[1]] |> 
  select(hr_score) |> 
  mutate(row = 1:nrow(imp_3_dfs[[1]]))

### get missing rows ----
imp_3.na <- imp_3_dfs[[1]] |> 
  select(-inforce) |> 
  where_na() |> 
  as_tibble() |> 
  select(row) |> 
  unique()

### filter out missing rows from hr_score ----
dep_3 <- dep_3 |> 
  anti_join(imp_3.na)
dep_3 <- dep_3$hr_score

### get covars ----
#### cpr & esr ----
treat_names <- imp_3_dfs[[1]] |> 
  select(5:7, 9:11) |> 
  names()
m <- 1:5
imp_3_covars <- list()

for(i in m){
  df <- imp_3_dfs[[i]]
  for(name in treat_names){
    df_small <- df |> 
      select(!3:4 & !8 & !12 & !setdiff(treat_names, name))
    imp_3_covars[[as.character(name)]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
  }
}

#### both ----
#### cpr_mean & esr_mean
for(i in m){
  df_small <- imp_3_dfs[[i]] |> 
    select(!3:4 & !6:8 & !10:12)
  imp_3_covars[["both_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

#### cpr_gdp & esr_gdp
for(i in m){
  df_small <- imp_3_dfs[[i]] |> 
    select(!3:5 & !7:9 & !11:12)
  imp_3_covars[["both_gdp_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

#### cpr_gdppc & esr_gdppc
for(i in m){
  df_small <- imp_3_dfs[[i]] |> 
    select(!3:6 & !8:10 & !12)
  imp_3_covars[["both_gdppc_mean"]][[as.character(i)]] <- model.matrix(~ . - 1, data = df_small)
}

# cross validate (lambda) ----
## imp_1 ----
imp_1_cv_res <- list()
covar_names <- names(imp_1_covars)
m <- 1:5

for(name in covar_names){
  list <- imp_1_covars[[name]]
  for(i in m){
    set.seed(15275)
    cv_res <- cv.glmnet(
      x = list[[i]],
      y = dep_1,
      alpha = 1
      )
    imp_1_cv_res[[as.character(name)]][[as.character(i)]] <- cv_res
  }
}

## imp_2 ----
imp_2_cv_res <- list()
covar_names <- names(imp_2_covars)
m <- 1:5

for(name in covar_names){
  list <- imp_2_covars[[name]]
  for(i in m){
    set.seed(15275)
    cv_res <- cv.glmnet(
      x = list[[i]],
      y = dep_2,
      alpha = 1
    )
    imp_2_cv_res[[as.character(name)]][[as.character(i)]] <- cv_res
  }
}

## imp_3 ----
imp_3_cv_res <- list()
covar_names <- names(imp_3_covars)
m <- 1:5

for(name in covar_names){
  list <- imp_3_covars[[name]]
  for(i in m){
    set.seed(15275)
    cv_res <- cv.glmnet(
      x = list[[i]],
      y = dep_3,
      alpha = 1
    )
    imp_3_cv_res[[as.character(name)]][[as.character(i)]] <- cv_res
  }
}

# get min lambdas ----
## load cv results if needed ----
load(here("data/ch1/results/tunes/imp_1_cv_res.rda"))
load(here("data/ch1/results/tunes/imp_2_cv_res.rda"))
load(here("data/ch1/results/tunes/imp_3_cv_res.rda"))

## imp_1 ----
imp_1_lams <- list()
covar_names <- names(imp_1_cv_res)
m <- 1:5

for(name in covar_names){
  list <- imp_1_cv_res[[name]]
  for(i in m){
    lam <- list[[i]]$lambda.min
    imp_1_lams[[as.character(name)]][[as.character(i)]] <- lam
  }
}

## imp_2 ----
imp_2_lams <- list()
covar_names <- names(imp_2_cv_res)
m <- 1:5

for(name in covar_names){
  list <- imp_2_cv_res[[name]]
  for(i in m){
    lam <- list[[i]]$lambda.min
    imp_2_lams[[as.character(name)]][[as.character(i)]] <- lam
  }
}

## imp_3 ----
imp_3_lams <- list()
covar_names <- names(imp_3_cv_res)
m <- 1:5

for(name in covar_names){
  list <- imp_3_cv_res[[name]]
  for(i in m){
    lam <- list[[i]]$lambda.min
    imp_3_lams[[as.character(name)]][[as.character(i)]] <- lam
  }
}

# standardize min lambdas ----
## imp_1 ----
imp_1_lams_mean <- list()
covar_names <- names(imp_1_lams)

for(name in covar_names){
  list <- imp_1_lams[[name]]
  lam_mean <- list |> 
    unlist() |>
    mean()
  imp_1_lams_mean[[as.character(name)]] <- lam_mean
}

## imp_2 ----
imp_2_lams_mean <- list()
covar_names <- names(imp_2_lams)

for(name in covar_names){
  list <- imp_2_lams[[name]]
  lam_mean <- list |> 
    unlist() |>
    mean()
  imp_2_lams_mean[[as.character(name)]] <- lam_mean
}

## imp_3 ----
imp_3_lams_mean <- list()
covar_names <- names(imp_3_lams)

for(name in covar_names){
  list <- imp_3_lams[[name]]
  lam_mean <- list |> 
    unlist() |>
    mean()
  imp_3_lams_mean[[as.character(name)]] <- lam_mean
}


# save prep data ----
## hr_score (dep) ----
dep_list <- list("dep_1" = dep_1, "dep_2" = dep_2, "dep_3" = dep_3)
dep_list |> save(file = here("data/ch1/results/imputations/dep_list.rda"))

## covars ----
imp_1_covars |> 
  save(file = here("data/ch1/results/imputations/imp_1_covars.rda"))
imp_2_covars |> 
  save(file = here("data/ch1/results/imputations/imp_2_covars.rda"))
imp_3_covars |> 
  save(file = here("data/ch1/results/imputations/imp_3_covars.rda"))

# save cv results ----
imp_1_cv_res |> 
  save(file = here("data/ch1/results/tunes/imp_1_cv_res.rda"))
imp_2_cv_res |> 
  save(file = here("data/ch1/results/tunes/imp_2_cv_res.rda"))
imp_3_cv_res |> 
  save(file = here("data/ch1/results/tunes/imp_3_cv_res.rda"))

# save lambdas ----
## full ----
imp_1_lams |> save(file = here("data/ch1/results/tunes/imp_1_lams.rda"))
imp_2_lams |> save(file = here("data/ch1/results/tunes/imp_2_lams.rda"))
imp_3_lams |> save(file = here("data/ch1/results/tunes/imp_3_lams.rda"))

## means ----
imp_1_lams_mean |> save(file = here("data/ch1/results/tunes/imp_1_lams_mean.rda"))
imp_2_lams_mean |> save(file = here("data/ch1/results/tunes/imp_2_lams_mean.rda"))
imp_3_lams_mean |> save(file = here("data/ch1/results/tunes/imp_3_lams_mean.rda"))
