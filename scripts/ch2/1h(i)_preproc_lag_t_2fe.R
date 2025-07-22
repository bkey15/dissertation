# Lag covars
## note: when grouping by specified vars (e.g., by cow and .imp), these are treated as "outside" of the dataset, hence why the column positions called in the mutations refer to a truncated version of the dataset

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch2/preprocessed/bits_1962.rda"))
load(here("data/ch2/results/imputations/imp_base.rda"))

# prep base data ----
## drop high miss states ----
## note: these countries are Kosovo, Taiwan, S. Vietnam, S. Yemen. Their missing values can't be imputed b/c there's too much missingness across key variables, particularly ones from the UN/World Bank (hras, wdi_trade, etc., b/c these were generally partially recognized states w/o organizational membership). Dropping these cases are important in preventing zero-variance variables at the dml-prep step. See notes for more.

imp_base_1962 <- imp_base |> 
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

# 1962 ----
## L1 (t-1) ----
bits_1962_l1 <- bits_1962 |> 
  select(-1) |> 
  group_by(cow) |> 
  mutate(
    across(
      contains("n_bits") | contains("partner_"),
      ~ .x * e_polity2,
      .names = "e_polity2_x_{.col}"
      ),
    across(
      !hr_score,
      ~ lag(.x)
      )
    ) |> 
  ungroup() |> 
  filter(!is.na(year))

# 1981 ----
## L1 (t-1) ----
bits_1981_l1 <- bits_1962_l1  |> 
  filter(year > 1980)

# 1990 ----
## L1 (t-1) ----
bits_1990_l1 <- bits_1962_l1  |> 
  filter(year > 1989)

# imp 1962 ----
## note: re-leveling "year" in imp_1962 chunk to remove "2019" as a level, which won't have any "1" (i.e., non-zero) values after lag. Doing so is important for dml initialization step.
## note: also re-leveling "cow" to remove any cow-levels dropping out of the dataset after lagging.
imp_1962_t_lags <- list()

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
      cow = droplevels(cow)
      ) |> 
    as.mids()
  
  imp_1962_t_lags[[as.character(paste0("l", i))]] <- lag_dat
}

# imp 1981 ----
## note: re-leveling cow codes to account for countries dropping out of the dataset
imp_base_1981 <- imp_base_1962 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1980) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow)
    )

imp_1981_t_lags <- list()

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
      cow = droplevels(cow)
      ) |> 
    as.mids()
  
  imp_1981_t_lags[[as.character(paste0("l", i))]] <- lag_dat
}

# imp_1990 ----
imp_base_1990 <- imp_base_1962 |> 
  mutate(
    year = as.numeric(levels(year))[year],
    cow = as.numeric(levels(cow))[cow]
    ) |> 
  filter(year > 1989) |> 
  mutate(
    year = as.factor(year),
    cow = as.factor(cow)
    )

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
## L1 ----
### no imp ----
bits_1962_l1 |> 
  save(file = here("data/ch2/preprocessed/bits_1962_l1.rda"))
bits_1981_l1 |> 
  save(file = here("data/ch2/preprocessed/bits_1981_l1.rda"))
bits_1990_l1 |> 
  save(file = here("data/ch2/preprocessed/bits_1990_l1.rda"))

### imp ----
imp_1962_t_lags |> 
  save(file = here("data/ch2/results/imputations/imp_1962_t_lags.rda"))
imp_1981_t_lags |> 
  save(file = here("data/ch2/results/imputations/imp_1981_t_lags.rda"))
imp_1990_t_lags |> 
  save(file = here("data/ch2/results/imputations/imp_1990_t_lags.rda"))
