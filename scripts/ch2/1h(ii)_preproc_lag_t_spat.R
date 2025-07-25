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

# prep base data ----
## merge & drop select states ----
## note: dropping high miss states and/or ones with no extant polygons
imp_base_1962 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id)

sp_lag_base_1962 <- sp_lag_base |> 
  mutate(
    cow = as.factor(cow),
    region = as.factor(region),
    year = as.factor(year)
    )

imp_base_1962 <- imp_base_1962 |> 
  left_join(sp_lag_base_1962) |> 
  filter(
    cow != "265",
    cow != "347",
    cow != "511",
    cow != "680",
    cow != "713",
    cow != "817"
    ) |> 
  relocate(region, .after = cow) |> 
  mutate(cow = droplevels(cow))

## make interaction vars ----
imp_base_1962 <- imp_base_1962 |> 
  mutate(
    across(
      contains("n_bits") | contains("partner_"),
      ~ .x * e_polity2,
      .names = "e_polity2_x_{.col}"
      )
    ) |> 
  relocate(
    contains("_x_"),
    .before = v2x_polyarchy
    )

# make lags ----
## imp 1962 ----
## note: re-leveling "year" in imp_1962 chunk to remove "2019" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: also re-leveling "cow" to remove any cow-levels dropping out of the dataset after lagging.
## note: including code to re-level "region" out of an abundance of caution (ultimately, no region drops out of the dataset, but still including code for possible future utility).

imp_1962_sp_t_lags <- list()

for(i in seq_along(1:8)){
  lag_dat <- imp_base_1962 |> 
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
  
  imp_1962_sp_t_lags[[as.character(paste0("l", i))]] <- lag_dat
}


## imp 1981 ----
## note: re-leveling cow codes to account for countries dropping out of the dataset
## note: in this case, no countries drop out
imp_base_1981 <- imp_base_1962 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1980) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow),
    region = droplevels(region)
    )

imp_1981_sp_t_lags <- list()

for(i in seq_along(1:8)){
  lag_dat <- imp_base_1981 |> 
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
  
  imp_1981_sp_t_lags[[as.character(paste0("l", i))]] <- lag_dat
}

## imp 1990 ----
## note: in this case, no countries drop out
imp_base_1990 <- imp_base_1962 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1989) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow),
    region = droplevels(region)
    )

imp_1990_sp_t_lags <- list()

for(i in seq_along(1:8)){
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
  
  imp_1990_sp_t_lags[[as.character(paste0("l", i))]] <- lag_dat
}

# save ----
imp_1962_sp_t_lags |> 
  save(file = here("data/ch2/results/imputations/imp_1962_sp_t_lags.rda"))
imp_1981_sp_t_lags |> 
  save(file = here("data/ch2/results/imputations/imp_1981_sp_t_lags.rda"))
imp_1990_sp_t_lags |> 
  save(file = here("data/ch2/results/imputations/imp_1990_sp_t_lags.rda"))
