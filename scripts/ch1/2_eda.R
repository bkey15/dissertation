# load packages ----
library(tidyverse)
library(here)
library(naniar)

# load data ----
load(here("data/ch1/preprocessed/ptas_panel.rda"))
load(here("data/ch1/preprocessed/ptas_final.rda"))
load(here("data/ch1/preprocessed/ptas_1968_l1.rda"))
load(here("data/ch1/results/imputations/imp_1968_l1.rda"))
load(here("data/ch1/results/imputations/imp_1977_l1.rda"))

# quick mean checks ----
## south-south ----
ptas_1968_l1 |> 
  filter(glb_s == 1) |> 
  summarize(mean = mean(ss_cpr_mean, na.rm = T))
ptas_1968_l1 |> 
  filter(glb_s == 1) |> 
  summarize(mean = mean(ss_esr_mean, na.rm = T))

## south-north ----
ptas_1968_l1 |> 
  filter(glb_s == 1) |> 
  summarize(mean = mean(ns_cpr_mean, na.rm = T))
ptas_1968_l1 |> 
  filter(glb_s == 1) |> 
  summarize(mean = mean(ns_esr_mean, na.rm = T))

## north-south ----
ptas_1968_l1 |> 
  filter(glb_s == 0) |> 
  summarize(mean = mean(ns_cpr_mean, na.rm = T))
ptas_1968_l1 |> 
  filter(glb_s == 0) |> 
  summarize(mean = mean(ns_esr_mean, na.rm = T))

## north-north ----
ptas_1968_l1 |> 
  filter(glb_s == 0) |> 
  summarize(mean = mean(nn_cpr_mean, na.rm = T))
ptas_1968_l1 |> 
  filter(glb_s == 0) |> 
  summarize(mean = mean(nn_esr_mean, na.rm = T))

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
  geom_point() +
  geom_smooth(method = "lm")

ptas_final |> 
  ggplot(aes(x = cpr_gdp_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_final |> 
  ggplot(aes(x = cpr_gdppc_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_1968_l1 |> 
  ggplot(aes(x = cpr_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_1968_l1 |> 
  ggplot(aes(x = cpr_gdp_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_1968_l1 |> 
  ggplot(aes(x = cpr_gdppc_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

# esr viz.s ----
ptas_final |> 
  ggplot(aes(x = esr_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_final |> 
  ggplot(aes(x = esr_gdp_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_final |> 
  ggplot(aes(x = esr_gdppc_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_1968_l1 |> 
  ggplot(aes(x = esr_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_1968_l1 |> 
  ggplot(aes(x = esr_gdp_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_1968_l1 |> 
  ggplot(aes(x = esr_gdppc_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

## north-north ----
imp_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(
    .imp == 5,
    glb_s == 0
    ) |> 
  ggplot(aes(x = nn_esr_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

## south-south ----
imp_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(
    .imp == 1,
    glb_s == 1
  ) |>
  select(-.imp, -.id)
  ggplot(aes(x = ss_esr_gdp_mean, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

# bop viz.s ----
ptas_final |> 
  ggplot(aes(x = bop_pct_gdp)) +
  geom_density()

ptas_final |> 
  ggplot(aes(x = bop_pct_gdp, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_1968_l1 |> 
  ggplot(aes(x = bop_pct_gdp, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ptas_final |> 
  filter(year == 2019) |> 
  ggplot(aes(x = bop_pct_gdp, y = hr_score)) +
  geom_point() +
  geom_smooth(method = "lm")

# autocorrelation ----
ptas_final |> 
  group_by(cow) |> 
  mutate(hr_lag = lag(hr_score)) |> 
  ggplot(aes(x = hr_lag, y = hr_score)) +
  geom_point()

# n ptas ----
## see if countries accede to more after fisc. crisis
ptas_count <- ptas_panel |> 
  mutate(
    base_treaty_inforce = if_else(
      base_treaty*inforce == 0, NA, base_treaty*inforce
      )
    ) |> 
  summarize(
    n_ptas = n_distinct(
      base_treaty_inforce,
      na.rm = TRUE
      ),
    .by = c(cow, year)
    ) |> 
  arrange(cow, year)

## asian financial crisis ----
### indonesia ----
ptas_count |> 
  filter(cow == 850) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()

### malaysia ----
ptas_count |> 
  filter(cow == 820) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()

### thailand ----
ptas_count |> 
  filter(cow == 800) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()

### s korea ----
ptas_count |> 
  filter(cow == 732) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()

## peso crisis ----
### mexico ----
ptas_count |> 
  filter(cow == 70) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()

## other in s. america ----
### argentina ----
ptas_count |> 
  filter(cow == 160) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()

### brazil ----
ptas_count |> 
  filter(cow == 140) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()

## other ----
### russia ----
ptas_count |> 
  filter(cow == 365) |> 
  ggplot(aes(x = year, y = n_ptas)) +
  geom_col()
