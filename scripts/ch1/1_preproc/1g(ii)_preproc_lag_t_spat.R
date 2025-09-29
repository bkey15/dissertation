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

# prep base data ----
## merge & drop select states ----
### note: drop states w/ no extant polygons (265, 680, 817); see make_spatial_lags script for more
imp_base_1968 <- imp_base |> 
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

imp_base_1968 <- imp_base_1968 |> 
  filter(
    cow != "265",
    cow != "680",
    cow != "817"
    ) |> 
  left_join(sp_lag_base) |> 
  relocate(region, .after = cow) |> 
  mutate(cow = droplevels(cow))

## make interaction vars ----
imp_base_1968 <- imp_base_1968 |> 
  mutate(
    across(
      ends_with("any_inforce"),
      ~ as.numeric(levels(.x))[.x]
      ),
    across(
      ends_with(
        c(
          "n_ptas",
          "any_inforce",
          "_mean"
          )
        ),
      ~ .x * v2x_polyarchy,
      .names = "v2x_polyarchy_x_{.col}"
      ),
    across(
      c(
        any_inforce,
        ss_any_inforce,
        ns_any_inforce,
        nn_any_inforce
        ),
      ~ as.factor(.x)
      )
    ) |> 
  select(
    -starts_with("v2x_polyarchy_x_depth"),
    -starts_with("v2x_polyarchy_x_ns_depth"),
    -starts_with("v2x_polyarchy_x_ss_depth"),
    -starts_with("v2x_polyarchy_x_nn_depth"),
    -starts_with("v2x_polyarchy_x_enforce"),
    -starts_with("v2x_polyarchy_x_ns_enforce"),
    -starts_with("v2x_polyarchy_x_ss_enforce"),
    -starts_with("v2x_polyarchy_x_nn_enforce")
    ) |> 
  relocate(
    contains("_x_"),
    .before = v2x_polyarchy
    )

# make lags ----
## 1968 ----
## note: re-leveling "year" to remove "2018", "2017", etc. as levels, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: re-leveling cow codes to account for countries potentially dropping out of the dataset
## note: including code to re-level "region" out of an abundance of caution (ultimately, no region drops out of the dataset, but still including code for possible future utility).
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
imp_sp_t_lags <- list(
  start_1968 = start_1968,
  start_1977 = start_1977,
  start_1990 = start_1990
  )

# save ----
imp_sp_t_lags |> 
  save(file = here("data/ch1/results/imputations/imp_sp_t_lags.rda"))
