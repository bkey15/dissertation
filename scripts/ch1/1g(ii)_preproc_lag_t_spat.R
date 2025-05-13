# Lag covars
## Note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset
## Note: what I'm producing here is a temporally lagged spatial lag (TLSL). See Wimpy et al. (2021, p. 4)

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_base.rda"))
load(here("data/ch1/results/imputations/sp_lag_base.rda"))

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
## note: dropping high miss states and/or ones with no extant polygons
imp_1968_sp_l1 <- imp_base |> 
  left_join(sp_lag_base) |> 
  filter(
    cow != "265",
    cow != "347",
    cow != "680",
    cow != "713",
    cow != "817"
    ) |> 
  relocate(region, .after = cow) |> 
  mutate(cow = droplevels(cow))

# L1 (t-1) ----
## 1968 ----
## note: re-leveling "year" in imp_1968 chunk to remove "2019" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
imp_1968_sp_l1 <- imp_1968_sp_l1 |> 
  group_by(cow, .imp) |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  mutate(year = droplevels(year))

## 1977 ----
## note: re-leveling cow codes to account for countries dropping out of the dataset
## note: in this instance, no countries drop out
imp_1977_sp_l1 <- imp_1968_sp_l1 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1976) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow)
    )

## as.mids() ----
imp_1968_sp_l1 <- imp_1968_sp_l1 |> 
  as.mids()

imp_1977_sp_l1 <- imp_1977_sp_l1 |> 
  as.mids()

# save ----
## L1 ----
### imp ----
imp_1968_sp_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_1968_sp_l1.rda"))
imp_1977_sp_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_1977_sp_l1.rda"))
