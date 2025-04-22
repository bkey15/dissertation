# Impute missings vals.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(parallel)
library(naniar)

# load data ----
load(here("data/ch2/preprocessed/bits_1962.rda"))
load(here("data/ch2/preprocessed/bits_1981.rda"))
load(here("data/ch2/preprocessed/bits_1990.rda"))

# missingness check ----
# IMPORTANT: vars with more than 10% missingness in bits_1962 will be excluded as predictors. See specify prediction cols section, below
miss_vars_1962 <- miss_var_summary(bits_1962)
miss_vars_1981 <- miss_var_summary(bits_1981)
miss_vars_1990 <- miss_var_summary(bits_1990)

# specify imputation vals (T/F) ----
## 1962 ----
imps_1962 <- bits_1962 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:17, glb_s), ~ FALSE
      ),
    across(
      !c(1:17, glb_s),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imps_1962){
  is.logical(var) |> print()
}

## 1981 ----
imps_1981 <- bits_1981 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:17, glb_s), ~ FALSE
      ),
    across(
      !c(1:17, glb_s),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imps_1981){
  is.logical(var) |> print()
}

## 1990 ----
imps_1990 <- bits_1990 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:17, glb_s), ~ FALSE
      ),
    across(
      !c(1:17, glb_s),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imps_1990){
  is.logical(var) |> print()
}

# specify predictor cols ----
## get dimension names
pred_names <- names(imps_1962)
dim_names <- list(pred_names, pred_names)

## initialize matrix
pred_mat <- matrix(
  nrow = 55,
  ncol = 55,
  dimnames = dim_names
  )

## convert to tibble, specify predictor cols, convert back to matrix
## note: leaving n_bits out as predictor b/c I created it relatively late in project and don't want to re-run all the models. Also won't be used as predictor in non-spatial models.
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
        bop_pct_gdp,
        wdi_fdiout,
        wdi_fdiin,
        wdi_trade,
        n_bits
        ),
      ~ 0
      ),
    row_name = pred_names
    ) |> 
  column_to_rownames(var = "row_name") |> 
  as.matrix(rownames.force = TRUE)

# impute ----
## prep data ----
### 1962 ----
bits_mice_1962 <- bits_1962 |> 
  select(-1) |> 
  mutate(
    across(
      c(cow, year, any_inforce, glb_s),
      ~ as_factor(.x)
    )
  )

### 1981 ----
bits_mice_1981 <- bits_1981 |> 
  select(-1) |> 
  mutate(
    across(
      c(cow, year, any_inforce, glb_s),
      ~ as_factor(.x)
    )
  )

### 1990 ----
bits_mice_1990 <- bits_1990 |> 
  select(-1) |> 
  mutate(
    across(
      c(cow, year, any_inforce, glb_s),
      ~ as_factor(.x)
      )
    )

## complete ----
### note: n.core should = m, since n_cores of my machine is > m
### 1962 ----
set.seed(96243214)
imp_1 <- bits_mice_1962 |> 
  futuremice(
    n.core = 5,
    method = "rf",
    where = imps_1962,
    predictorMatrix = pred_mat
    )

### 1981 ----
set.seed(96243214)
imp_2 <- bits_mice_1981 |> 
  futuremice(
    n.core = 5,
    method = "rf",
    where = imps_1981,
    predictorMatrix = pred_mat
    )

### 1990 ----
set.seed(96243214)
imp_3 <- bits_mice_1990 |> 
  futuremice(
    n.core = 5,
    method = "rf",
    where = imps_1990,
    predictorMatrix = pred_mat
    )

### save ----
imp_1 |> 
  save(
    file = here("data/ch2/results/imputations/imp_1.rda")
    )

imp_2 |> 
  save(
    file = here("data/ch2/results/imputations/imp_2.rda")
    )

imp_3 |> 
  save(
    file = here("data/ch2/results/imputations/imp_3.rda")
    )
