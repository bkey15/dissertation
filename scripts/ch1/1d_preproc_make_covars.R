# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch1/preprocessed/merge_base.rda"))

# replicate Hafner-Burton (2005) vars ----
## investment (p. 617) ----
ptas_final <- merge_base |> 
  mutate(
    inv = case_when(
      is.na(wdi_fdiin) & !is.na(wdi_fdiout) ~ wdi_fdiout,
      !is.na(wdi_fdiin) & is.na(wdi_fdiout) ~ wdi_fdiin,
      .default = wdi_fdiin + wdi_fdiout
      )
    ) |> 
  select(-c(wdi_fdiin, wdi_fdiout))

## hras (p. 617) ----
ptas_final <- ptas_final |> 
  mutate(
    hras = case_when(
      is.na(cat_rat) & !is.na(iccpr_rat) ~ iccpr_rat,
      !is.na(cat_rat) & is.na(iccpr_rat) ~ cat_rat,
      .default = cat_rat + iccpr_rat
      )
    ) |> 
  select(
    -c(cat_rat, iccpr_rat)
    )

# make bop as % of GDP ----
ptas_final <- ptas_final |> 
  mutate(
    gdp_raw = gdp_mean*10000000,
    bop_pct_gdp = (bop_raw/gdp_raw)*100
    ) |> 
  select(-gdp_raw, -bop_raw)

# drop gdp_mean, gdppc_mean, and pop_mean ----
## note: doing this for now so they don't end up in imputations/lasso
ptas_final <- ptas_final |> 
  select(-gdp_mean, -gdppc_mean, -pop_mean)

# save ----
ptas_final |> 
  save(file = here("data/ch1/preprocessed/ptas_final.rda"))
