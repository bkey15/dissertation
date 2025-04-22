# Create modified datasets reflecting different "start years"

# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch2/preprocessed/bits_final.rda"))

# make modified datasets ----
## fix NAs ----
bits_final <- bits_final |> 
  mutate(
    across(
      c(7:9, starts_with("ns_"), n_bits),
      ~ if_else(
        is.na(.x), 0, .x
        )
      ),
    across(
      starts_with("ss_"),
      ~ if_else(
        glb_s == 1 & is.na(.x), 0, .x)
      ),
    across(
      starts_with("nn_"),
      ~ if_else(
        glb_s == 0 & is.na(.x), 0, .x)
      )
    )

## drop high miss states ----
## note: these countries are Kosovo, Taiwan, S. Vietnam, S. Yemen. Their missing values can't be imputed b/c there's too much missingness across key variables, particularly ones from the UN/World Bank (hras, wdi_trade, etc., b/c these were generally partially recognized states w/o organizational membership). See notes for more.
bits_final <- bits_final |> 
  filter(
    cow != 347,
    cow != 680,
    cow != 713,
    cow != 817
  )

## 1962 (1st in-force BIT) ----
bits_1962 <- bits_final |> 
  filter(year > 1961)

## 1981 (bodea & ye) ----
bits_1981 <- bits_final |> 
  filter(year > 1980)

## 1990 (bodea & ye, rob. check) ----
bits_1990 <- bits_final |> 
  filter(year > 1989)

## save ----
bits_1962 |> 
  save(file = here("data/ch2/preprocessed/bits_1962.rda"))

bits_1981 |> 
  save(file = here("data/ch2/preprocessed/bits_1981.rda"))

bits_1990 |> 
  save(file = here("data/ch2/preprocessed/bits_1990.rda"))
