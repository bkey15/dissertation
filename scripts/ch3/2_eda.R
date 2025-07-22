# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch3/results/imputations/imp_base.rda"))
load(here("data/ch3/results/imputations/sp_lag_base.rda"))
load(here("data/ch3/results/imputations/imp_1990_t_lags.rda"))
load(here("data/ch3/results/imputations/imp_1990_sp_t_lags.rda"))

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
