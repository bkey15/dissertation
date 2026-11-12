# Temporally lag all covars
## Note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# run prep script ----
source(here("scripts/ch3/1_preproc/1g_preproc_make_cre.R"))

# prep base data ----
## note: filtering out nearly all glb_s == 0 cases (these were aids in computing spatial lags but are no longer necessary), save for cases w/ sanctions where glb_s == 0 (Slovenia & Saudi Arabia).
inforce_glb_n <- imp_base_1990 |> 
  filter(any_inforce == "1" & glb_s == "0") |> 
  mutate(cow = droplevels(cow))
inforce_glb_n <- as.character(unique(inforce_glb_n$cow))

imp_base_1990 <- imp_base_1990 |> 
  filter(glb_s == "1" | cow %in% inforce_glb_n) |> 
  mutate(
    cow = droplevels(cow)
    )

# make lags ----
## 1990 ----
## note: re-leveling "year" to remove "2018", "2017", etc. as levels, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
### note: also re-leveling "cow" to remove any cow-levels dropping out of the dataset after lagging. This is only needed at L8 (S. Sudan), but am including the code for other lags for possible future use.
## note: including code to re-level "region" again out of an abundance of caution (ultimately, no region drops out of the dataset, but still including code for possible future utility).
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
  start_1990 = start_1990
  )

# save ----
imp_t_lags |> 
  save(file = here("data/ch3/results/imputations/imp_t_lags.rda"))
