# load packages ----
library(tidyverse)
library(here)
library(naniar)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))

# missingness check ----
## no start point ----
### tbl ----
miss_tbl <- ptas_final |> 
  miss_var_summary()

### plot ----
gg_miss_var(ptas_final)

ptas_final |> 
  gg_miss_upset()

## start point == 1949 (1st inforce pta) ----
### tbl ----
miss_tbl2 <- ptas_final |>
  filter(year > 1948) |> 
  miss_var_summary()

### plots ----
ptas_final |> 
  filter(year > 1948) |> 
  gg_miss_var()

ptas_final |> 
  filter(year > 1948) |> 
  gg_miss_upset()

## start point == 1966 (1st ratified hra) ----
### tbl ----
miss_tbl3 <- ptas_final |>
  filter(year > 1965) |> 
  miss_var_summary()

### plots ----
ptas_final |> 
  filter(year > 1965) |> 
  gg_miss_var()

ptas_final |> 
  filter(year > 1965) |> 
  gg_miss_upset()

## start point == 1977 (spilker & böhmelt) ----
### tbl ----
miss_tbl4 <- ptas_final |>
  filter(year > 1977) |> 
  miss_var_summary()

### plots ----
ptas_final |> 
  filter(year > 1977) |> 
  gg_miss_var()

ptas_final |> 
  filter(year > 1977) |> 
  gg_miss_upset()

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
