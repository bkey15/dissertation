# Lag covars
## Note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset
## Note: what I'm producing here is a temporally lagged spatial lag (TLSL). See Wimpy et al. (2021, p. 4)

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch3/results/imputations/imp_base.rda"))
load(here("data/ch3/results/imputations/sp_lag_base.rda"))

# prep base data ----
## note: creating interaction vars
imp_base <- imp_base |> 
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

imp_base <- imp_base |> 
  left_join(sp_lag_base) |> 
  relocate(region, .after = cow) |> 
  group_by(cow, .imp) |> 
  mutate(
    any_inforce = as.numeric(levels(any_inforce))[any_inforce],
    across(
      c(n_ems, any_inforce),
      ~ .x * v2x_polyarchy,
      .names = "v2x_polyarchy_x_{.col}"
      ),
    any_inforce = factor(
      any_inforce,
      levels = c("0", "1")
      )
    ) |> 
  relocate(
    contains("_x_"),
    .after = any_inforce
    )

# make lags ----
## L1 (t-1) ----
## note: re-leveling "year" to remove "2018" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: also re-leveling "cow" to remove any cow-levels dropping out of the dataset after lagging. This is only needed at L8 (S. Sudan), and doesn't matter as much for spatial models (using regional fixed effects). Still including the code for possible future use.
## note: including code to re-level "region" out of an abundance of caution (ultimately, no region drops out of the dataset, but still including code for possible future utility).

l1 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

## L2 (t-2) ----
l2 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x, n = 2)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

## L3 (t-3) ----
l3 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x, n = 3)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

## L4 (t-4) ----
l4 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x, n = 4)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

## L5 (t-5) ----
l5 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x, n = 5)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

## L6 (t-6) ----
l6 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x, n = 6)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

## L7 (t-7) ----
l7 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x, n = 7)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

## L8 (t-8) ----
l8 <- imp_base |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x, n = 8)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(
    year = droplevels(year),
    cow = droplevels(cow),
    region = droplevels(region)
    ) |> 
  as.mids()

# combine ----
imp_1990_sp_t_lags <- list(
  l1 = l1,
  l2 = l2,
  l3 = l3,
  l4 = l4,
  l5 = l5,
  l6 = l6,
  l7 = l7,
  l8 = l8
  )

# save ----
imp_1990_sp_t_lags |> 
  save(file = here("data/ch3/results/imputations/imp_1990_sp_t_lags.rda"))
