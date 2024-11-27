# Replicate Hafner-Burton (2005) w/ two-way fixed effects & multiple imputation (`mice`). Impute missings vals.

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
  select(-c(1, 5)) |> 
  mutate(
    across(
      c(1:11, 49), ~ FALSE
      ),
    across(
      c(12:48, 50),
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
  select(-c(1, 5)) |> 
  mutate(
    across(
      c(1:11, 49), ~ FALSE
      ),
    across(
      c(12:48, 50),
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
  select(-c(1, 5)) |> 
  mutate(
    across(
      c(1:11, 49), ~ FALSE
      ),
    across(
      c(12:48, 50),
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
  select(-c(1, 5)) |> 
  mutate(
    across(
      1:2,
      ~ as_factor(.x)
    )
  )

### 1968 ----
ptas_mice_1968 <- ptas_1968 |> 
  select(-c(1, 5)) |> 
  mutate(
    across(
      1:2,
      ~ as_factor(.x)
      )
    )

### 1977 ----
ptas_mice_1977 <- ptas_1977 |> 
  select(-c(1, 5)) |> 
  mutate(
    across(
      1:2,
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

lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

## 1968 (1st inforce pta) ----
### note: selecting 1966 as start point yields same results, hence why I don't provide the code for it
ptas_1968 <- ptas_final |> 
  mutate(
    across(
      6:13,
      ~ if_else(
        year > 1948 & is.na(.x), 0, .x
        )
      )
    )

lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

## 1977 (spilker & böhmelt) ----
ptas_1977 <- ptas_final |> 
  mutate(
    across(
      6:13,
      ~ if_else(
        year > 1976 & is.na(.x), 0, .x
        )
      )
    )

lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1977) |> 
  summary()

# esr ----
## no start point ----
lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

## 1968 ----
lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

# Initial reaction: pretty much across the board, we see decreases in HR respect as treatment "dosage" increases, except for gdppc-weighted treatments, where we see increases in HR respect. May suggest that provisions only "work" when partner is rich.

# both ----
## no start point ----
lm(hr_score ~ lag(cpr_mean) + lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_final) |> 
  summary()

## 1968 ----
lm(hr_score ~ lag(cpr_mean) + lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdp_mean) + lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_gdppc_mean) + lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

lm(hr_score ~ lag(cpr_pop_mean) + lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(factor(cow)) + lag(factor(year)), data = ptas_1968) |> 
  summary()

# check dropped rows ----
drops1 <- ptas_final |> 
  select(hr_score, cpr_mean, e_polity2, p_durable, wdi_popden, wdi_trade, inv, gdppc_log10, cow, year) |> 
  filter(c(is.na(cow) | is.na(year) | is.na(hr_score) | is.na(cpr_mean) | is.na(e_polity2) | is.na(p_durable) | is.na(wdi_popden) | is.na(wdi_trade) | is.na(inv) | is.na(gdppc_log10)))

drops2 <- ptas_1968 |> 
  select(hr_score, cpr_mean, e_polity2, p_durable, wdi_popden, wdi_trade, inv, gdppc_log10, cow, year) |> 
  filter(c(is.na(cow) | is.na(year) | is.na(hr_score) | is.na(cpr_mean) | is.na(e_polity2) | is.na(p_durable) | is.na(wdi_popden) | is.na(wdi_trade) | is.na(inv) | is.na(gdppc_log10)))

