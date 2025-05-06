# Lag covars
## Note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset
## Note: what I'm producing here is a temporally lagged spatial lag (TLSL). See Wimpy et al. (2021, p. 4)

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch2/results/imputations/imp_base.rda"))
load(here("data/ch2/results/imputations/sp_lag_base.rda"))

# complete/prep base data ----
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

# merge data ----
## note: GDR, Zanzibar, S. Yemen, & S. Vietnam are in imp_base but not sp_lag_base (no extant polygons), meaning these countries have missing values on all spatially-laged variables
imp_1962_sp_l1 <- imp_base |> 
  left_join(sp_lag_base) |> 
  filter(
    cow != "265",
    cow != "511",
    cow != "680",
    cow != "817"
    ) |> 
  relocate(region, .after = cow)

# L1 (t-1) ----
## 1962 ----
## note: re-leveling "year" in imp_1962 chunk to remove "2019" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.

imp_1962_sp_l1 <- imp_1962_sp_l1 |> 
  group_by(cow, .imp) |> 
  mutate(
    across(
      contains("n_bits") | contains("partner_"),
      ~ .x * e_polity2,
      .names = "e_polity2_x_{.col}"
      ),
    across(
      !c(.id, hr_score),
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(year = as.numeric(levels(year))[year]) |> 
  mutate(year = as.factor(year))

## 1981 ----
## note: re-leveling cow codes to account for countries dropping out of the dataset
imp_1981_sp_l1 <- imp_1962_sp_l1 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1980) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow)
    )

## 1990 ----
imp_1990_sp_l1 <- imp_1962_sp_l1 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
  ) |> 
  filter(year > 1989) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow)
    )

## as.mids() ----
imp_1962_sp_l1 <- imp_1962_sp_l1 |> 
  as.mids()

imp_1981_sp_l1 <- imp_1981_sp_l1 |> 
  as.mids()

imp_1990_sp_l1 <- imp_1990_sp_l1 |> 
  as.mids()

# save ----
## L1 ----
### imp ----
imp_1962_sp_l1 |> 
  save(file = here("data/ch2/results/imputations/imp_1962_sp_l1.rda"))
imp_1981_sp_l1 |> 
  save(file = here("data/ch2/results/imputations/imp_1981_sp_l1.rda"))
imp_1990_sp_l1 |> 
  save(file = here("data/ch2/results/imputations/imp_1990_sp_l1.rda"))
