# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch3/results/imputations/imp_base.rda"))
load(here("data/ch3/results/imputations/sp_lag_base.rda"))

# eda ----
eda_dat <- imp_base |> 
  mice::complete(
    action = "long",
    include = TRUE
    )

eda_dat |> 
  filter(n_ems > 0) |> 
  ggplot(aes(x = n_ems)) +
  geom_freqpoly(binwidth = 1)

eda_dat |> 
  filter(n_ems > 0) |> 
  ggplot(aes(x = n_ems)) +
  geom_boxplot()

n_ems_n <- eda_dat |> 
  summarize(n = n(), .by = n_ems) |> 
  arrange(n_ems)

n_ems_n |> 
  filter(n_ems > 4) |> 
  select(n) |> 
  sum()

n_ems_n |> 
  filter(n_ems > 0 & n_ems < 5) |> 
  select(n) |> 
  sum()

test <- eda_dat |> 
  filter(.imp == 0) |> 
  select(v2x_polyarchy, e_v2x_polyarchy_5C) |> 
  mutate(
    man_5c = case_when(
      v2x_polyarchy <= 0.2 ~ 0,
      v2x_polyarchy > 0.2 & v2x_polyarchy <= 0.4 ~ 0.25,
      v2x_polyarchy > 0.4 & v2x_polyarchy <= 0.6 ~ 0.5,
      v2x_polyarchy > 0.6 & v2x_polyarchy <= 0.8 ~ 0.75,
      v2x_polyarchy > 0.8 & v2x_polyarchy <= 1 ~ 1
      )
    )

test2 <- test |> 
  filter(e_v2x_polyarchy_5C != man_5c)

test |> 
  count(man_5c)
