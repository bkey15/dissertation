# Lag covars
## note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch3/results/imputations/imp_base.rda"))

# L1 (t-1) ----
## imp_1990 ----
## note: re-leveling "year" in imp_1962 chunk to remove "2019" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: also creating interaction vars.

imp_1990_l1 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id) |> 
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

## as.mids() ----
imp_1990_l1 <- imp_1990_l1 |> 
  as.mids()

# save ----
## L1 ----
imp_1990_l1 |> 
  save(file = here("data/ch3/results/imputations/imp_1990_l1.rda"))
