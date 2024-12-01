# Impute missings vals.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(parallel)
library(doMC)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))
load(here("data/ch1/preprocessed/ptas_1968.rda"))
load(here("data/ch1/preprocessed/ptas_1977.rda"))

# set cores ----
## check
n <- detectCores() - 1

## set
registerDoMC(cores = n)

# specify imputation vals (T/F) ----
## no start year specified ----
imps_no_start <- ptas_final |> 
  select(-1) |> 
  mutate(
    across(
      c(1:12, 47), ~ FALSE
      ),
    across(
      c(13:46, 48),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imps_no_start){
  is.logical(var) |> print()
}

## 1968 ----
imps_1968 <- ptas_1968 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:12, 47), ~ FALSE
      ),
    across(
      c(13:46, 48),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imps_1968){
  is.logical(var) |> print()
}

## 1977 ----
imps_1977 <- ptas_1977 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:12, 47), ~ FALSE
      ),
    across(
      c(13:46, 48),
      ~ if_else(
        is.na(.x),
        TRUE,
        FALSE
        )
      )
    )

### check success
for(var in imps_1977){
  is.logical(var) |> print()
}

# impute ----
## prep data ----
### no start ----
ptas_mice_no_start <- ptas_final |> 
  select(-1) |> 
  mutate(
    across(
      c(1:2, 4),
      ~ as_factor(.x)
    )
  )

### 1968 ----
ptas_mice_1968 <- ptas_1968 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:2, 4),
      ~ as_factor(.x)
    )
  )

### 1977 ----
ptas_mice_1977 <- ptas_1977 |> 
  select(-1) |> 
  mutate(
    across(
      c(1:2, 4),
      ~ as_factor(.x)
    )
  )

## complete ----
### no start ----
set.seed(96243214)
imp_1 <- ptas_mice_no_start |> 
  mice(
    method = "rf",
    where = imps_no_start
    )

### 1968 ----
set.seed(96243214)
imp_2 <- ptas_mice_1968 |> 
  mice(
    method = "rf",
    where = imps_1968
  )

### 1977 ----
set.seed(96243214)
imp_3 <- ptas_mice_1977 |> 
  mice(
    method = "rf",
    where = imps_1977
  )

### save ----
imp_1 |> 
  save(
    file = here("data/ch1/results/imputations/imp_1.rda")
    )

imp_2 |> 
  save(
    file = here("data/ch1/results/imputations/imp_2.rda")
    )

imp_3 |> 
  save(
    file = here("data/ch1/results/imputations/imp_3.rda")
    )
