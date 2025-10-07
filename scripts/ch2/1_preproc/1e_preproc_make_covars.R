# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch2/preprocessed/merge_base.rda"))

# make bop as % of GDP ----
bits_final <- merge_base |> 
  mutate(
    gdp_raw = gdp_mean*10000000,
    bop_pct_gdp = (bop_raw/gdp_raw)*100
    ) |> 
  select(-gdp_raw, -bop_raw)

# drop gdp_mean, gdppc_mean, and pop_mean ----
## note: doing this for now so they don't end up in imputations/lasso
bits_final <- bits_final |> 
  select(-gdp_mean, -gdppc_mean, -pop_mean)

# save ----
bits_final |> 
  save(file = here("data/ch2/preprocessed/bits_final.rda"))
