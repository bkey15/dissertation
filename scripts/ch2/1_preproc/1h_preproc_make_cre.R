# Make conditional random effects (CREs)
## note: these will be used in lieu of country fixed effects

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch2/results/imputations/imp_base.rda"))
load(here("data/ch2/results/imputations/sp_lag_base.rda"))

# prep base data ----
## merge & drop select states ----
### note: drop states w/ no extant polygons (265, 511, 680, 817); see make_spatial_lags script for more
imp_base_1962 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id)

sp_lag_base <- sp_lag_base |> 
  mutate(
    cow = as.factor(cow),
    region = as.factor(region),
    year = as.factor(year)
    )

imp_base_1962 <- imp_base_1962 |> 
  filter(
    cow != "265",
    cow != "511",
    cow != "680",
    cow != "817"
    ) |> 
  left_join(sp_lag_base) |> 
  relocate(region, .after = cow) |> 
  mutate(cow = droplevels(cow))

## make interaction vars ----
imp_base_1962 <- imp_base_1962 |> 
  mutate(
    across(
      ends_with("any_inforce"),
      ~ as.numeric(levels(.x))[.x]
      ),
    across(
      ends_with(c("n_bits", "any_inforce")) | contains("partner_"),
      ~ .x * e_polity2,
      .names = "e_polity2_x_{.col}"
      )
    ) |> 
  select(-starts_with("e_polity2_x_e_polity2")) |> 
  relocate(
    contains("_x_") & !ends_with("sp_lag"),
    .before = v2x_polyarchy
    )

# make CREs ----
### note: creating summarize tbl to check success (rather than create CRE vars directly into dataset)
cres <- imp_base_1962 |> 
  mutate(
    .imp = as.factor(.imp),
    curr_crisis = as.numeric(levels(curr_crisis))[curr_crisis],
    imf_prog = as.numeric(levels(imf_prog))[imf_prog]
    ) |> 
  summarize(
    across(
      where(is.numeric) & !.id,
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_cre"
      ),
    .by = c(.imp, cow)
    ) |> 
  mutate(
    across(
      everything(),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      ),
    .imp = as.numeric(levels(.imp))[.imp]
    )

## final merge ----
imp_base_1962 <- imp_base_1962 |> 
  mutate(
    across(
      c(
        any_inforce,
        ss_any_inforce,
        ns_any_inforce,
        nn_any_inforce
        ),
      ~ as.factor(.x)
      )
    ) |> 
  left_join(cres)

# clear glb env ----
rm(list = setdiff(ls(), "imp_base_1962"))
gc()
