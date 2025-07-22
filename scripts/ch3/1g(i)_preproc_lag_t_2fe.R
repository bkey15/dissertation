# Lag covars
## note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch3/results/imputations/imp_base.rda"))

# prep base data ----
## note: creating interaction vars
imp_base_1990 <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  relocate(.imp, .id) |> 
  group_by(cow, .imp) |> 
  mutate(
    any_inforce = as.numeric(levels(any_inforce))[any_inforce],
    across(
      c(n_ems, any_inforce),
      ~ .x * v2x_polyarchy,
      .names = "v2x_polyarchy_x_{.col}"
      ),
    any_inforce = factor(
      any_inforce,
      levels = c("0", "1")
      )
    ) |> 
  relocate(
    contains("_x_"),
    .after = any_inforce
    )

# make lags ----
## note: re-leveling "year" to remove "2018" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: also re-leveling "cow" to remove any cow-levels dropping out of the dataset after lagging. This is only needed at L8 (S. Sudan), but am including the code for other lags for possible future use.
imp_1990_t_lags <- list()

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
      cow = droplevels(cow)
      ) |> 
    as.mids()
  
  imp_1990_t_lags[[as.character(paste0("l", i))]] <- lag_dat
}

# save ----
imp_1990_t_lags |> 
  save(file = here("data/ch3/results/imputations/imp_1990_t_lags.rda"))
