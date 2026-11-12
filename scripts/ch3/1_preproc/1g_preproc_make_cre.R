# Make conditional random effects (CREs)
## note: these will be used in lieu of country fixed effects

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch3/results/imputations/imp_base.rda"))
load(here("data/ch3/results/imputations/sp_lag_base.rda"))

# prep base data ----
## merge ----
imp_base_1990 <- imp_base |> 
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

imp_base_1990 <- imp_base_1990 |> 
  left_join(sp_lag_base) |> 
  relocate(region, .after = cow) |> 
  mutate(cow = droplevels(cow))

## make interaction vars ----
imp_base_1990 <- imp_base_1990 |> 
  mutate(
    any_inforce = as.numeric(levels(any_inforce))[any_inforce],
    across(
      c(n_ems, any_inforce),
      ~ .x * v2x_polyarchy,
      .names = "v2x_polyarchy_x_{.col}"
      ),
    across(
      c(n_ems, any_inforce),
      ~ .x * e_v2x_polyarchy_5C,
      .names = "e_v2x_polyarchy_5C_x_{.col}"
      )
    ) |> 
  relocate(
    contains("_x_") & !ends_with("sp_lag"),
    .after = any_inforce
    )

# make CREs ----
### note: creating summarize tbl to check success (rather than create CRE vars directly into dataset)
cres <- imp_base_1990 |> 
  mutate(
    .imp = as.factor(.imp),
    curr_crisis = as.numeric(levels(curr_crisis))[curr_crisis],
    imf_prog = as.numeric(levels(imf_prog))[imf_prog],
    gov_kill = as.numeric(levels(gov_kill))[gov_kill],
    coup_success = as.numeric(levels(coup_success))[coup_success],
    cont_elect = as.numeric(levels(cont_elect))[cont_elect],
    pta_west = as.numeric(levels(pta_west))[pta_west],
    bit_tip_west = as.numeric(levels(bit_tip_west))[bit_tip_west]
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
imp_base_1990 <- imp_base_1990 |> 
  mutate(
    any_inforce = as.factor(any_inforce)
    ) |> 
  left_join(cres)

# clear glb env ----
rm(list = setdiff(ls(), "imp_base_1990"))
gc()
