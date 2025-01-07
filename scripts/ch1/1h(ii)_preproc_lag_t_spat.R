# Lag covars
## Note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset
## Note: what I'm producing here is a temporally lagged spatial lag (TLSL). See Wimpy et al. (2021, p. 4)

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_2.rda"))
load(here("data/ch1/results/imputations/imp_3.rda"))
load(here("data/ch1/results/imputations/lags_dat_1968.rda"))
load(here("data/ch1/results/imputations/lags_dat_1977.rda"))

# merge data ----
## imp_2 ----
imp_2 <- imp_2 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id)

lags_dat_1968 <- lags_dat_1968 |> 
  mutate(
    cow = as.factor(cow),
    region = as.factor(region),
    year = as.factor(year)
    )

imp_2_sp_l1 <- imp_2 |> 
  left_join(lags_dat_1968) |> 
  filter(cow != "265") |> 
  relocate(region, .after = cow)

## imp_3 ----
imp_3 <- imp_3 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id)

lags_dat_1977 <- lags_dat_1977 |> 
  mutate(
    cow = as.factor(cow),
    region = as.factor(region),
    year = as.factor(year)
    )

imp_3_sp_l1 <- imp_3 |> 
  left_join(lags_dat_1977) |> 
  filter(cow != "265") |> 
  relocate(region, .after = cow)

# L1 (t-1) ----
## imp_2 (ptas_1968) ----
imp_2_sp_l1 <- imp_2_sp_l1 |> 
  group_by(cow, .imp) |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  as.mids()

## imp_3 (ptas_1977) ----
imp_3_sp_l1 <- imp_3_sp_l1 |> 
  group_by(cow, .imp) |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  as.mids()

# save ----
## L1 ----
### imp ----
imp_2_sp_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_2_sp_l1.rda"))
imp_3_sp_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_3_sp_l1.rda"))
