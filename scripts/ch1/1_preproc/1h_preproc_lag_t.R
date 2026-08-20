# Temporally lag all covars
## Note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# run prep script ----
source(here("scripts/ch1/1_preproc/1g_preproc_make_cre.R"))

# make lags ----
## 1968 ----
## note: re-leveling "year" to remove "2018", "2017", etc. as levels, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: re-leveling cow codes to account for countries potentially dropping out of the dataset, although this doesn't occur. (doing so out of an abundance of caution.)
## note: including code to re-level "region" again out of an abundance of caution (ultimately, no region drops out of the dataset, but still including code for possible future utility).
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
      cow = droplevels(cow),
      region = droplevels(region)
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
      cow = droplevels(cow),
      region = droplevels(region)
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
      cow = droplevels(cow),
      region = droplevels(region)
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
