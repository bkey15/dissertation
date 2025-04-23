# Lag covars
## note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/preprocessed/ptas_1968.rda"))
load(here("data/ch1/results/imputations/imp_base.rda"))

# L1 (t-1) ----
## 1968 ----
ptas_1968_l1 <- ptas_1968 |> 
  select(-1, -n_ptas) |> 
  group_by(cow) |> 
  mutate(
    across(
      !hr_score,
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year))

## 1977 ----
ptas_1977_l1 <- ptas_1968_l1  |> 
  filter(year > 1976)

## imp_1968 ----
imp_1968_l1 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(-n_ptas) |> 
  relocate(.imp, .id) |> 
  group_by(cow, .imp) |> 
  mutate(
    across(
      !c(.id, hr_score),
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year))

## imp_1977 ----
imp_1977_l1 <- imp_1968_l1 |> 
  mutate(
    year = as.numeric(levels(year))[year]
    ) |> 
  filter(year > 1976) |> 
  mutate(year = as.factor(year))

## as.mids() ----
imp_1968_l1 <- imp_1968_l1 |> 
  as.mids()

imp_1977_l1 <- imp_1977_l1 |> 
  as.mids()

# save ----
## L1 ----
### no imp ----
ptas_1968_l1 |> 
  save(file = here("data/ch1/preprocessed/ptas_1968_l1.rda"))
ptas_1977_l1 |> 
  save(file = here("data/ch1/preprocessed/ptas_1977_l1.rda"))

### imp ----
imp_1968_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_1968_l1.rda"))
imp_1977_l1 |> 
  save(file = here("data/ch1/results/imputations/imp_1977_l1.rda"))
