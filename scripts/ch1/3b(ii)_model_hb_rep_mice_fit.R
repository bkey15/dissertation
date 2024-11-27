# Replicate Hafner-Burton (2005) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals.

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_1.rda"))
load(here("data/ch1/results/imputations/imp_2.rda"))
load(here("data/ch1/results/imputations/imp_3.rda"))

# no start year ----
## cpr ----
### mean ----
fit_cpr1_nost <- with(
  imp_1,
  lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr1_nost <- pool(fit_cpr1_nost)
sum_cpr1_nost <- summary(pool_cpr1_nost)

### gdp mean ----
fit_cpr2_nost <- with(
  imp_1,
  lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
     )
  )

pool_cpr2_nost <- pool(fit_cpr2_nost)
sum_cpr2_nost <- summary(pool_cpr2_nost)

### gdppc mean ----
fit_cpr3_nost <- with(
  imp_1,
  lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
     )
  )

pool_cpr3_nost <- pool(fit_cpr3_nost)
sum_cpr3_nost <- summary(pool_cpr3_nost)

### pop mean ----
fit_cpr4_nost <- with(
  imp_1,
  lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
     )
  )

pool_cpr4_nost <- pool(fit_cpr4_nost)
sum_cpr4_nost <- summary(pool_cpr4_nost)

## esr ----
### mean ----
fit_esr1_nost <- with(
  imp_1,
  lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr1_nost <- pool(fit_esr1_nost)
sum_esr1_nost <- summary(pool_esr1_nost)

### gdp mean ----
fit_esr2_nost <- with(
  imp_1,
  lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr2_nost <- pool(fit_esr2_nost)
sum_esr2_nost <- summary(pool_esr2_nost)

### gdppc mean ----
fit_esr3_nost <- with(
  imp_1,
  lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr3_nost <- pool(fit_esr3_nost)
sum_esr3_nost <- summary(pool_esr3_nost)

### pop mean ----
fit_esr4_nost <- with(
  imp_1,
  lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr4_nost <- pool(fit_esr4_nost)
sum_esr4_nost <- summary(pool_esr4_nost)

## save ----
sum_cpr1_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_cpr1_nost.rda")
    )
sum_cpr2_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_cpr2_nost.rda")
    )
sum_cpr3_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_cpr3_nost.rda")
    )
sum_cpr4_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_cpr4_nost.rda")
    )
sum_esr1_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_esr1_nost.rda")
    )
sum_esr2_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_esr2_nost.rda")
    )
sum_esr3_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_esr3_nost.rda")
    )
sum_esr4_nost |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/no_start_year/sum_esr4_nost.rda")
    )

# start 1968 ----
## cpr ----
### mean ----
fit_cpr1_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr1_1968 <- pool(fit_cpr1_1968)
sum_cpr1_1968 <- summary(pool_cpr1_1968)

### gdp mean ----
fit_cpr2_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr2_1968 <- pool(fit_cpr2_1968)
sum_cpr2_1968 <- summary(pool_cpr2_1968)

### gdppc mean ----
fit_cpr3_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr3_1968 <- pool(fit_cpr3_1968)
sum_cpr3_1968 <- summary(pool_cpr3_1968)

### pop mean ----
fit_cpr4_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr4_1968 <- pool(fit_cpr4_1968)
sum_cpr4_1968 <- summary(pool_cpr4_1968)

## esr ----
### mean ----
fit_esr1_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr1_1968 <- pool(fit_esr1_1968)
sum_esr1_1968 <- summary(pool_esr1_1968)

### gdp mean ----
fit_esr2_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr2_1968 <- pool(fit_esr2_1968)
sum_esr2_1968 <- summary(pool_esr2_1968)

### gdppc mean ----
fit_esr3_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr3_1968 <- pool(fit_esr3_1968)
sum_esr3_1968 <- summary(pool_esr3_1968)

### pop mean ----
fit_esr4_1968 <- with(
  imp_2,
  lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr4_1968 <- pool(fit_esr4_1968)
sum_esr4_1968 <- summary(pool_esr4_1968)

## save ----
sum_cpr1_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_cpr1_1968.rda")
  )
sum_cpr2_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_cpr2_1968.rda")
  )
sum_cpr3_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_cpr3_1968.rda")
  )
sum_cpr4_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_cpr4_1968.rda")
  )
sum_esr1_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_esr1_1968.rda")
  )
sum_esr2_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_esr2_1968.rda")
  )
sum_esr3_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_esr3_1968.rda")
  )
sum_esr4_1968 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1968/sum_esr4_1968.rda")
  )

# start 1977 ----
## cpr ----
### mean ----
fit_cpr1_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(cpr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr1_1977 <- pool(fit_cpr1_1977)
sum_cpr1_1977 <- summary(pool_cpr1_1977)

### gdp mean ----
fit_cpr2_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(cpr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr2_1977 <- pool(fit_cpr2_1977)
sum_cpr2_1977 <- summary(pool_cpr2_1977)

### gdppc mean ----
fit_cpr3_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(cpr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr3_1977 <- pool(fit_cpr3_1977)
sum_cpr3_1977 <- summary(pool_cpr3_1977)

### pop mean ----
fit_cpr4_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(cpr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_cpr4_1977 <- pool(fit_cpr4_1977)
sum_cpr4_1977 <- summary(pool_cpr4_1977)

## esr ----
### mean ----
fit_esr1_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(esr_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr1_1977 <- pool(fit_esr1_1977)
sum_esr1_1977 <- summary(pool_esr1_1977)

### gdp mean ----
fit_esr2_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(esr_gdp_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr2_1977 <- pool(fit_esr2_1977)
sum_esr2_1977 <- summary(pool_esr2_1977)

### gdppc mean ----
fit_esr3_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(esr_gdppc_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr3_1977 <- pool(fit_esr3_1977)
sum_esr3_1977 <- summary(pool_esr3_1977)

### pop mean ----
fit_esr4_1977 <- with(
  imp_3,
  lm(hr_score ~ lag(esr_pop_mean) + lag(e_polity2) + lag(p_durable) + lag(wdi_popden) + lag(wdi_trade) + lag(inv) + lag(gdppc_log10) + lag(cow) + lag(year)
  )
)

pool_esr4_1977 <- pool(fit_esr4_1977)
sum_esr4_1977 <- summary(pool_esr4_1977)

## save ----
sum_cpr1_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_cpr1_1977.rda")
  )
sum_cpr2_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_cpr2_1977.rda")
  )
sum_cpr3_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_cpr3_1977.rda")
  )
sum_cpr4_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_cpr4_1977.rda")
  )
sum_esr1_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_esr1_1977.rda")
  )
sum_esr2_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_esr2_1977.rda")
  )
sum_esr3_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_esr3_1977.rda")
  )
sum_esr4_1977 |> 
  save(
    file = here("data/ch1/results/fits/hb_rep/mice/start_1977/sum_esr4_1977.rda")
  )

