# Lag covars
## note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/preprocessed/ptas_1968.rda"))
load(here("data/ch1/preprocessed/ptas_1977.rda"))
load(here("data/ch1/results/imputations/imp_2.rda"))
load(here("data/ch1/results/imputations/imp_3.rda"))

# L1 (t-1) ----
## ptas_1968 ----
ptas_1968_l1 <- ptas_1968 |> 
  select(-1) |> 
  group_by(cow) |> 
  mutate(
    across(
      !hr_score,
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year))

## ptas_1977 ----
ptas_1977_l1 <- ptas_1977 |> 
  select(-1) |> 
  group_by(cow) |> 
  mutate(
    across(
      !hr_score,
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year))

## imp_2 (ptas_1968) ----
imp_2_l1 <- imp_2 |> 
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
  as.mids()

## imp_3 (ptas_1977) ----
imp_3_l1 <- imp_3 |> 
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
  as.mids()

# save ----
## L1 ----
### no imp ----
ptas_1968_l1 |> 
  save(file = here("data/ch1/preprocessed/ptas_1968_l1.rda"))
ptas_1977_l1 |> 
  save(file = here("data/ch1/preprocessed/ptas_1977_l1.rda"))

### imp ----
imp_2_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_2_l1.rda"))
imp_3_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_3_l1.rda"))
