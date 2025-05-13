# Lag covars
## note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/preprocessed/ptas_1968.rda"))
load(here("data/ch1/results/imputations/imp_base.rda"))

# drop high miss states ----
# note: these countries are Kosovo, Taiwan, S. Vietnam, S. Yemen. Their missing values can't be imputed b/c there's too much missingness across key variables, particularly ones from the UN/World Bank (hras, wdi_trade, etc., b/c these were generally partially recognized states w/o organizational membership). Dropping these cases are important in preventing zero-variance variables at the dml-prep step. See notes for more.

imp_1968_l1 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(
    cow != "347",
    cow != "680",
    cow != "713",
    cow != "817"
    ) |> 
  mutate(cow = droplevels(cow))

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
## note: re-leveling "year" in imp_1962 chunk to remove "2019" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
imp_1968_l1 <- imp_1968_l1 |> 
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
  filter(!is.na(year)) |> 
  mutate(year = droplevels(year))

## imp_1977 ----
## note: re-leveling cow codes to account for countries dropping out of the dataset
## note: in this case, no countries drop out
imp_1977_l1 <- imp_1968_l1 |> 
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
