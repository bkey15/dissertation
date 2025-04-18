# Impute missings vals.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(parallel)
library(doMC)
library(naniar)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))
load(here("data/ch1/preprocessed/ptas_1968.rda"))
load(here("data/ch1/preprocessed/ptas_1977.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# missingness check ----
# IMPORTANT: vars with more than 10% missingness in ptas_1968 will be excluded as predictors. See specify predicot cols section, below
miss_vars_nst <- miss_var_summary(ptas_final)
miss_vars_1968 <- miss_var_summary(ptas_1968)
miss_vars_1977 <- miss_var_summary(ptas_1977)

# specify imputation vals (T/F) ----
## no start year specified ----
imps_no_start <- ptas_final |> 
  select(-1) |> 
  mutate(
    across(
      c(1:53, glb_s, hras), ~ FALSE
      ),
    across(
      !c(1:53, glb_s, hras),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imps_no_start){
  is.logical(var) |> print()
}

## 1968 ----
imps_1968 <- ptas_1968 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:53, glb_s, hras), ~ FALSE
    ),
    across(
      !c(1:53, glb_s, hras),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
      )
    )
  )

### check success
for(var in imps_1968){
  is.logical(var) |> print()
}

## 1977 ----
imps_1977 <- ptas_1977 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:53, glb_s, hras), ~ FALSE
    ),
    across(
      !c(1:53, glb_s, hras),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
      )
    )
  )

### check success
for(var in imps_1977){
  is.logical(var) |> print()
}

# specify predictor cols ----
## get dimension names
pred_names <- names(imps_no_start)
dim_names <- list(pred_names, pred_names)

## initialize matrix
pred_mat <- matrix(
  nrow = 90,
  ncol = 90,
  dimnames = dim_names
  )

## convert to tibble, specify predictor cols, convert back to matrix
## note: leaving n_ptas out as predictor b/c I created it relatively late in project and don't want to re-run all the models. Also won't be used as predictor in non-spatial models.
pred_mat <- pred_mat |> 
  as_tibble() |> 
  mutate(
    across(
      everything(),
      ~ if_else(cur_column() == pred_names, 0, 1)
      ),
    across(
      c(
        starts_with("ss_"),
        starts_with("nn_"),
        contains("lech_hr"),
        bop_pct_gdp,
        wdi_trade,
        inv,
        n_ptas
        ),
      ~ 0
      ),
    row_name = pred_names
    ) |> 
  column_to_rownames(var = "row_name") |> 
  as.matrix(rownames.force = TRUE)

# impute ----
## prep data ----
### 1968 ----
ptas_mice_1968 <- ptas_1968 |> 
  select(-1) |> 
  mutate(
    across(
      c(cow, year, inforce, glb_s),
      ~ as_factor(.x)
    )
  )

### 1977 ----
ptas_mice_1977 <- ptas_1977 |> 
  select(-1) |> 
  mutate(
    across(
      c(cow, year, inforce, glb_s),
      ~ as_factor(.x)
    )
  )

## complete ----
### 1968 ----
set.seed(96243214)
imp_2 <- ptas_mice_1968 |> 
  mice(
    method = "rf",
    where = imps_1968,
    predictorMatrix = pred_mat
  )

### 1977 ----
set.seed(96243214)
imp_3 <- ptas_mice_1977 |> 
  mice(
    method = "rf",
    where = imps_1977,
    predictorMatrix = pred_mat
  )

### save ----
imp_2 |> 
  save(
    file = here("data/ch1/results/imputations/imp_2.rda")
    )

imp_3 |> 
  save(
    file = here("data/ch1/results/imputations/imp_3.rda")
    )
