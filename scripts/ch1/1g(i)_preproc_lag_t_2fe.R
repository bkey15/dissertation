# Lag covars
## note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_base.rda"))

# drop high miss states ----
# note: these countries are Kosovo, Taiwan, S. Vietnam, S. Yemen. Their missing values can't be imputed b/c there's too much missingness across key variables, particularly ones from the UN/World Bank (hras, wdi_trade, etc., b/c these were generally partially recognized states w/o organizational membership). Dropping these cases are important in preventing zero-variance variables at the dml-prep step. See notes for more.

imp_base_1968 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id) |> 
  filter(
    cow != "347",
    cow != "680",
    cow != "713",
    cow != "817"
    ) |> 
  mutate(cow = droplevels(cow))

## make interaction vars ----
imp_base_1968 <- imp_base_1968 |> 
  mutate(
    across(
      ends_with("_mean"),
      ~ .x * v2x_polyarchy,
      .names = "v2x_polyarchy_x_{.col}"
      )
    ) |> 
  relocate(
    contains("_x_"),
    .before = v2x_polyarchy
  )

# make lags ----
## 1968 ----
## note: re-leveling "year" to remove "2018", "2017", etc. as levels, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: re-leveling cow codes to account for countries potentially dropping out of the dataset
start_1968 <- list()

for(i in seq(1:8)){
  lag_dat <- imp_base_1968 |> 
    group_by(cow, .imp) |> 
    mutate(
      across(
        !c(.id, hr_score),
        ~ lag(.x, n = i)
        )
      ) |> 
    ungroup() |> 
    filter(!is.na(year)) |> 
    mutate(
      year = droplevels(year),
      cow = droplevels(cow)
      ) |> 
    as.mids()
  
  start_1968[[as.character(paste0("l", i))]] <- lag_dat
}

## 1977 ----
imp_base_1977 <- imp_base_1968 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1976) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow)
    )

start_1977 <- list()

for(i in seq(1:8)){
  lag_dat <- imp_base_1977 |> 
    group_by(cow, .imp) |> 
    mutate(
      across(
        !c(.id, hr_score),
        ~ lag(.x, n = i)
        )
      ) |> 
    ungroup() |> 
    filter(!is.na(year)) |> 
    mutate(
      year = droplevels(year),
      cow = droplevels(cow)
      ) |> 
    as.mids()
  
  start_1977[[as.character(paste0("l", i))]] <- lag_dat
}

## 1990 ----
imp_base_1990 <- imp_base_1968 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1989) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow)
    )

start_1990 <- list()

for(i in seq(1:8)){
  lag_dat <- imp_base_1990 |> 
    group_by(cow, .imp) |> 
    mutate(
      across(
        !c(.id, hr_score),
        ~ lag(.x, n = i)
        )
      ) |> 
    ungroup() |> 
    filter(!is.na(year)) |> 
    mutate(
      year = droplevels(year),
      cow = droplevels(cow)
      ) |> 
    as.mids()
  
  start_1990[[as.character(paste0("l", i))]] <- lag_dat
}

# combine ----
imp_t_lags <- list(
  start_1968 = start_1968,
  start_1977 = start_1977,
  start_1990 = start_1990
  )

# save ----
imp_t_lags |> 
  save(file = here("data/ch1/results/imputations/imp_t_lags.rda"))
