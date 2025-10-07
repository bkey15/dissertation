# load packages ----
library(tidyverse)
library(here)
library(mice)
library(naniar)

# load data ----
load(here("data/ch3/preprocessed/ems_final.rda"))

# missingness check ----
# IMPORTANT: vars with more than 10% missingness will be excluded as predictors. See specify prediction cols section, below
miss_vars <- miss_var_summary(ems_final)

# specify imputation vals (T/F) ----
imp_vals <- ems_final |> 
  mutate(
    across(
      c(
        cow,
        year,
        hr_score,
        n_ems,
        any_inforce,
        gov_kill,
        coup_success,
        cont_elect,
        glb_s,
        ends_with("_west")
        ), ~ FALSE
      ),
    across(
      c(
        starts_with(c("v2", "wdi_")),
        ends_with("_log10"),
        e_polity2,
        p_durable,
        bop_pct_gdp,
        pol_prox
        ),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imp_vals){
  is.logical(var) |> 
    print()
}

# specify predictor cols ----
## get dimension names
pred_names <- names(imp_vals)
dim_names <- list(pred_names, pred_names)

## initialize matrix
pred_mat <- matrix(
  nrow = 53,
  ncol = 53,
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
        bop_pct_gdp,
        wdi_trade,
        wdi_fdiout
        ),
      ~ 0
      ),
    row_name = pred_names
    ) |> 
  column_to_rownames(var = "row_name") |> 
  as.matrix(rownames.force = TRUE)

# impute ----
## prep data ----
ems_mice <- ems_final |> 
  mutate(
    across(
      c(
        cow,
        year,
        any_inforce,
        gov_kill,
        coup_success,
        cont_elect,
        glb_s,
        pta_west,
        bit_tip_west
        ),
      ~ as_factor(.x)
    )
  )

## complete ----
## note: setting n.core == m (5), since imputed datasets are completed by core 
set.seed(75894325)
imp_base <- ems_mice |> 
  futuremice(
    n.core = 5,
    method = "rf",
    where = imp_vals,
    predictorMatrix = pred_mat
    )

imp_base |> 
  save(
    file = here("data/ch3/results/imputations/imp_base.rda")
    )
