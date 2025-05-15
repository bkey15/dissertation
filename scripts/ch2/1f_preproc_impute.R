# Impute missings vals.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(naniar)

# load data ----
load(here("data/ch2/preprocessed/bits_final.rda"))

# set min start date: 1962 (1st in-force BIT) ----
bits_1962 <- bits_final |> 
  filter(year > 1961)

# set NA --> 0 ----
## note: GDR is other country where n_bits is coerced to 0. This will help w/ imputations, but GDR will not feature in final dataset due to spatial lags (no polygon for it exists).
bits_1962 <- bits_1962 |> 
  mutate(
    across(
      c(7:9, starts_with("ns_"), n_bits),
      ~ if_else(
        is.na(.x), 0, .x
        )
      ),
    across(
      starts_with("ss_"),
      ~ if_else(
        glb_s == 1 & is.na(.x), 0, .x)
      ),
    across(
      starts_with("nn_"),
      ~ if_else(
        glb_s == 0 & is.na(.x), 0, .x)
      )
    )

# missingness check ----
# IMPORTANT: vars with more than 10% missingness in 1968 start-year will be excluded as predictors. See specify prediction cols section, below
miss_vars_1962 <- miss_var_summary(bits_1962)

# specify imputation vals (T/F) ----
imp_vals <- bits_1962 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:20, glb_s), ~ FALSE
      ),
    across(
      !c(1:20, glb_s),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imp_vals){
  is.logical(var) |> print()
}

# specify predictor cols ----
## get dimension names
pred_names <- names(imp_vals)
dim_names <- list(pred_names, pred_names)

## initialize matrix
pred_mat <- matrix(
  nrow = 58,
  ncol = 58,
  dimnames = dim_names
  )

## convert to tibble, specify predictor cols, convert back to matrix
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
        wdi_trade,
        wdi_fdiout,
        wdi_fdiin
        ),
      ~ 0
      ),
    row_name = pred_names
    ) |> 
  column_to_rownames(var = "row_name") |> 
  as.matrix(rownames.force = TRUE)

# impute ----
## prep data ----
bits_mice <- bits_1962 |> 
  select(-1) |> 
  mutate(
    across(
      c(cow, year, any_inforce, glb_s),
      ~ as_factor(.x)
    )
  )

## complete ----
## note: setting n.core == m (5), since imputed datasets are completed by core 
set.seed(96243214)
imp_base <- bits_mice |> 
  futuremice(
    n.core = 5,
    method = "rf",
    where = imp_vals,
    predictorMatrix = pred_mat
  )

### save ----
bits_1962 |> 
  save(
    file = here("data/ch2/preprocessed/bits_1962.rda")
    )

imp_base |> 
  save(
    file = here("data/ch2/results/imputations/imp_base.rda")
    )
