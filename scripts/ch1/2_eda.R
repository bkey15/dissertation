# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))

# cpr viz.s ----
ptas_final |> 
  ggplot(aes(x = cpr_mean, y = hr_score)) +
  geom_point()

ptas_final |> 
  ggplot(aes(x = cpr_gdp_mean, y = hr_score)) +
  geom_point()

ptas_final |> 
  ggplot(aes(x = cpr_gdppc_mean, y = hr_score)) +
  geom_point()

# esr viz.s ----
ptas_final |> 
  ggplot(aes(x = esr_mean, y = hr_score)) +
  geom_point()

ptas_final |> 
  ggplot(aes(x = esr_gdp_mean, y = hr_score)) +
  geom_point()

ptas_final |> 
  ggplot(aes(x = esr_gdppc_mean, y = hr_score)) +
  geom_point()
