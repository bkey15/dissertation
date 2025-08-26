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
## note: removing pop_mean vars as predictors b/c I'm presently not using them as treatments.

ptas_1968 <- ptas_final |> 
  filter(year > 1967) |> 
  select(-ends_with("pop_mean"))

# set NA --> 0/min ----
## note: S. Sudan ptas don't appear until 2019 and are thus not included in the Lechner dataset. Need to coerce to 0 in order to compute spatial lags on n_ptas for S. Sudan's neighbors.
## note: GDR is other country where n_ptas is coerced to 0. This will help w/ imputations, but GDR will not feature in final models due to spatial lags (no polygon for it exists).
## note: breaking up tasks to ensure second set of mutations takes into account initial set (particularly any_inforce vars)
ptas_1968 <- ptas_1968 |> 
  mutate(
    across(
      c(
        starts_with(
          c(
            "cpr_",
            "esr_",
            "lech_",
            "ns_cpr_",
            "ns_esr_",
            "ns_lech_"
            )
          ),
        ns_any_inforce,
        n_ptas,
        ns_n_ptas
        ),
      ~ if_else(
        is.na(.x), 0, .x
        )
      ),
    across(
      c(
        starts_with(
          c(
            "ss_cpr_",
            "ss_esr_",
            "ss_lech_"
            )
          ),
        ss_any_inforce,
        ss_n_ptas
        ),
      ~ if_else(
        glb_s == 1 & is.na(.x), 0, .x
        )
      ),
    across(
      c(
        starts_with(
          c(
            "nn_cpr_",
            "nn_esr_",
            "nn_lech_"
            )
          ),
        nn_any_inforce,
        nn_n_ptas
        ),
      ~ if_else(
        glb_s == 0 & is.na(.x), 0, .x
        )
      )
    )

ptas_1968 <- ptas_1968 |> 
  mutate(
    across(
      starts_with("depth_"),
      ~ if_else(
        any_inforce == 0 & is.na(.x),
        min(.x, na.rm = TRUE),
        .x
        )
      ),
    across(
      starts_with("enforce_"),
      ~ if_else(
        any_inforce == 0 & is.na(.x),
        0,
        .x
        )
      ),
    across(
      starts_with("ns_depth_"),
      ~ if_else(
        ns_any_inforce == 0 & is.na(.x),
        min(.x, na.rm = TRUE),
        .x
        )
      ),
    across(
      starts_with("ns_enforce_"),
      ~ if_else(
        ns_any_inforce == 0 & is.na(.x),
        0,
        .x
        )
      ),
    across(
      starts_with("ss_depth_"),
      ~ if_else(
        glb_s == 1 & ss_any_inforce == 0 & is.na(.x),
        min(.x, na.rm = TRUE),
        .x
        )
      ),
    across(
      starts_with("ss_enforce_"),
      ~ if_else(
        glb_s == 1 & ss_any_inforce == 0 & is.na(.x),
        0,
        .x
        )
      ),
    across(
      starts_with("nn_depth_"),
      ~ if_else(
        glb_s == 0 & nn_any_inforce == 0 & is.na(.x),
        min(.x, na.rm = TRUE),
        .x
        )
      ),
    across(
      starts_with("nn_enforce_"),
      ~ if_else(
        glb_s == 0 & nn_any_inforce == 0 & is.na(.x),
        0,
        .x
        )
      )
    )

# missingness check ----
# IMPORTANT: vars with more than 10% missingness in 1968 start-year will be excluded as predictors. hras will also be excluded because it is systematically missing for some countries (e.g., Kosovo & Taiwan), leading to failed imputations for such countries on other variables . This will be implemented specify prediction cols section, below.
miss_vars_1968 <- miss_var_summary(ptas_1968)

# specify imputation vals (T/F) ----
## important: ensure glb_s is converted to logical at the very end (needs to be referred to in order to specify other vars)
imp_vals <- ptas_1968 |> 
  select(-1) |> 
  mutate(
    across(
      c(
        cow,
        year,
        hr_score,
        contains(
          c(
            "any_inforce",
            "n_ptas",
            "cpr",
            "esr",
            "lech"
            )
          ),
        starts_with("ns_"),
        hras
        ), ~ FALSE
      ),
    across(
      c(
        starts_with(
          c(
            "depth_",
            "enforce_",
            "v2x",
            "wdi_"
            )
          ),
        ends_with("_log10"),
        e_polity2,
        p_durable,
        inv,
        bop_pct_gdp
        ),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      ),
    across(
      starts_with(
        c("ss_depth_", "ss_enforce_")),
      ~ if_else(
        glb_s == 1 & is.na(.x),
        TRUE,
        FALSE
        )
      ),
    across(
      starts_with(
        c("nn_depth_", "nn_enforce_")),
      ~ if_else(
        glb_s == 0 & is.na(.x),
        TRUE,
        FALSE
        )
      ),
    across(
      glb_s,
      ~ FALSE
      )
    )

### check success
for(var in imp_vals){
  is.logical(var) |> print()
}

### note: ultimately, there are only a few cases where depth/enforce vars need to be imputed (namely, the US pre-1985)

# specify predictor cols ----
## get dimension names
pred_names <- names(imp_vals)
dim_names <- list(pred_names, pred_names)

## initialize matrix
pred_mat <- matrix(
  nrow = 108,
  ncol = 108,
  dimnames = dim_names
  )

## convert to tibble, specify predictor cols, convert back to matrix
### note: allowing gen & ns lech vars as predictors b/c rf not significantly affected by multicollinearity.
pred_mat <- pred_mat |> 
  as_tibble() |> 
  mutate(
    across(
      everything(),
      ~ if_else(cur_column() == pred_names, 0, 1)
      ),
    across(
      c(
        starts_with(c("nn_", "ss_")),
        bop_pct_gdp,
        wdi_trade,
        inv
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
      c(cow, year, glb_s, contains("any_inforce")),
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
imp_base |> 
  save(
    file = here("data/ch1/results/imputations/imp_base.rda")
    )
