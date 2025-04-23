# Impute missings vals.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(naniar)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))

# set min start date: 1968 (1st ratified hra) ----
## note: selecting 1949 (1st inforce pta) as start year yields same results, b/c all countries have NAs on hras pre-1968 (am not imputing b/c no country could have logically received "treatment" prior to that point), hence why I don't provide the code for it
## note: will create a version of the dataset starting at 1977 (spilker & böhmelt) later

ptas_1968 <- ptas_final |> 
  filter(year > 1967)

# set NA --> 0 ----
## note: S. Sudan ptas don't appear until 2019 and are thus not included in the Lechner dataset. Need to coerce to 0 in order to compute spatial lags on n_ptas for S. Sudan's neighbors.
## note: GDR is other country where n_ptas is coerced to 0. This will help w/ imputations, but GDR will not feature in final dataset due to spatial lags (no polygon for it exists).

ptas_1968 <- ptas_1968 |> 
  mutate(
    across(
      c(7:18, starts_with("ns_"), n_ptas),
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

# drop high miss states ----
# note: these countries are Kosovo, Taiwan, S. Vietnam, S. Yemen. Their missing values can't be imputed b/c there's too much missingness across key variables, particularly ones from the UN/World Bank (hras, wdi_trade, etc., b/c these were generally partially recognized states w/o organizational membership). See notes for more.

ptas_1968 <- ptas_1968 |> 
  filter(
    cow != 347,
    cow != 680,
    cow != 713,
    cow != 817
    )

# missingness check ----
# IMPORTANT: vars with more than 10% missingness in 1968 start-year will be excluded as predictors. See specify prediction cols section, below
miss_vars_1968 <- miss_var_summary(ptas_1968)

# specify imputation vals (T/F) ----
imp_vals <- ptas_1968 |> 
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
for(var in imp_vals){
  is.logical(var) |> print()
}

# specify predictor cols ----
## get dimension names
pred_names <- names(imp_vals)
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
ptas_mice <- ptas_1968 |> 
  select(-1) |> 
  mutate(
    across(
      c(cow, year, inforce, glb_s),
      ~ as_factor(.x)
    )
  )

## complete ----
## note: setting n.core == m (5), since imputed datasets are completed by core 
set.seed(96243214)
imp_base <- ptas_mice |> 
  futuremice(
    n.core = 5,
    method = "rf",
    where = imp_vals,
    predictorMatrix = pred_mat
  )

### save ----
ptas_1968 |> 
  save(
    file = here("data/ch1/preprocessed/ptas_1968.rda")
    )

imp_base |> 
  save(
    file = here("data/ch1/results/imputations/imp_base.rda")
    )
