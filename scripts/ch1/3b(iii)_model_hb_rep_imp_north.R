# Replicate Hafner-Burton (2005) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals (& lagged covars). Limited to "Global North" countries.

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_2_l1.rda"))
load(here("data/ch1/results/imputations/imp_3_l1.rda"))

# filter to north ----
imp_2_l1_north <- imp_2_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(glb_s == 0) |> 
  as.mids()

imp_3_l1_north <- imp_3_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
  ) |> 
  filter(glb_s == 0) |> 
  as.mids()

# start 1968 ----
## general ----
## i.e., no consideration for ns, ss, nn ptas
### mean ----
gen_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_gen_mean_1968 <- pool(gen_mean_1968)
sum_gen_mean_1968 <- summary(pool_gen_mean_1968)

### gdp ----
gen_gdp_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_gen_gdp_mean_1968 <- pool(gen_gdp_mean_1968)
sum_gen_gdp_mean_1968 <- summary(pool_gen_gdp_mean_1968)

### gdppc ----
gen_gdppc_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_gen_gdppc_mean_1968 <- pool(gen_gdppc_mean_1968)
sum_gen_gdppc_mean_1968 <- summary(pool_gen_gdppc_mean_1968)

### combine ----
general <- list(
  mean = sum_gen_mean_1968,
  gdp = sum_gen_gdp_mean_1968,
  gdppc = sum_gen_gdppc_mean_1968
  )

## cpr ----
### mean ----
cpr_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_cpr_mean + ns_cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr_mean_1968 <- pool(cpr_mean_1968)
sum_cpr_mean_1968 <- summary(pool_cpr_mean_1968)

### gdp ----
cpr_gdp_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_cpr_gdp_mean + ns_cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr_gdp_mean_1968 <- pool(cpr_gdp_mean_1968)
sum_cpr_gdp_mean_1968 <- summary(pool_cpr_gdp_mean_1968)

### gdppc ----
cpr_gdppc_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_cpr_gdppc_mean + ns_cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr_gdppc_mean_1968 <- pool(cpr_gdppc_mean_1968)
sum_cpr_gdppc_mean_1968 <- summary(pool_cpr_gdppc_mean_1968)

### combine ----
cpr <- list(
  mean = sum_cpr_mean_1968,
  gdp = sum_cpr_gdp_mean_1968,
  gdppc = sum_cpr_gdppc_mean_1968
  )

## esr ----
### mean ----
esr_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_esr_mean + ns_esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr_mean_1968 <- pool(esr_mean_1968)
sum_esr_mean_1968 <- summary(pool_esr_mean_1968)

### gdp ----
esr_gdp_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_esr_gdp_mean + ns_esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr_gdp_mean_1968 <- pool(esr_gdp_mean_1968)
sum_esr_gdp_mean_1968 <- summary(pool_esr_gdp_mean_1968)

### gdppc ----
esr_gdppc_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_esr_gdppc_mean + ns_esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr_gdppc_mean_1968 <- pool(esr_gdppc_mean_1968)
sum_esr_gdppc_mean_1968 <- summary(pool_esr_gdppc_mean_1968)

### combine ----
esr <- list(
  mean = sum_esr_mean_1968,
  gdp = sum_esr_gdp_mean_1968,
  gdppc = sum_esr_gdppc_mean_1968
)

## both ----
### mean ----
both_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_cpr_mean + ns_cpr_mean + nn_esr_mean + ns_esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both_mean_1968 <- pool(both_mean_1968)
sum_both_mean_1968 <- summary(pool_both_mean_1968)

### gdp ----
both_gdp_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_cpr_gdp_mean + ns_cpr_gdp_mean + nn_esr_gdp_mean + ns_esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both_gdp_mean_1968 <- pool(both_gdp_mean_1968)
sum_both_gdp_mean_1968 <- summary(pool_both_gdp_mean_1968)

### gdppc ----
both_gdppc_mean_1968 <- with(
  imp_2_l1,
  lm(hr_score ~ nn_cpr_gdppc_mean + ns_cpr_gdppc_mean + nn_esr_gdppc_mean + ns_esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both_gdppc_mean_1968 <- pool(both_gdppc_mean_1968)
sum_both_gdppc_mean_1968 <- summary(pool_both_gdppc_mean_1968)

### combine ----
both <- list(
  mean = sum_both_mean_1968,
  gdp = sum_both_gdp_mean_1968,
  gdppc = sum_both_gdppc_mean_1968
)

## all combine ----
imp_2_hb_res_north <- list(
  general = general,
  cpr = cpr,
  esr = esr,
  both = both
  )

# start 1977 ----
## general ----
## i.e., no consideration for ns, ss, nn ptas
### mean ----
gen_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_mean + esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_gen_mean_1977 <- pool(gen_mean_1977)
sum_gen_mean_1977 <- summary(pool_gen_mean_1977)

### gdp ----
gen_gdp_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_gdp_mean + esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_gen_gdp_mean_1977 <- pool(gen_gdp_mean_1977)
sum_gen_gdp_mean_1977 <- summary(pool_gen_gdp_mean_1977)

### gdppc ----
gen_gdppc_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ cpr_gdppc_mean + esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_gen_gdppc_mean_1977 <- pool(gen_gdppc_mean_1977)
sum_gen_gdppc_mean_1977 <- summary(pool_gen_gdppc_mean_1977)

### combine ----
general <- list(
  mean = sum_gen_mean_1977,
  gdp = sum_gen_gdp_mean_1977,
  gdppc = sum_gen_gdppc_mean_1977
)

## cpr ----
### mean ----
cpr_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_cpr_mean + ns_cpr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr_mean_1977 <- pool(cpr_mean_1977)
sum_cpr_mean_1977 <- summary(pool_cpr_mean_1977)

### gdp ----
cpr_gdp_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_cpr_gdp_mean + ns_cpr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr_gdp_mean_1977 <- pool(cpr_gdp_mean_1977)
sum_cpr_gdp_mean_1977 <- summary(pool_cpr_gdp_mean_1977)

### gdppc ----
cpr_gdppc_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_cpr_gdppc_mean + ns_cpr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_cpr_gdppc_mean_1977 <- pool(cpr_gdppc_mean_1977)
sum_cpr_gdppc_mean_1977 <- summary(pool_cpr_gdppc_mean_1977)

### combine ----
cpr <- list(
  mean = sum_cpr_mean_1977,
  gdp = sum_cpr_gdp_mean_1977,
  gdppc = sum_cpr_gdppc_mean_1977
)

## esr ----
### mean ----
esr_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_esr_mean + ns_esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr_mean_1977 <- pool(esr_mean_1977)
sum_esr_mean_1977 <- summary(pool_esr_mean_1977)

### gdp ----
esr_gdp_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_esr_gdp_mean + ns_esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr_gdp_mean_1977 <- pool(esr_gdp_mean_1977)
sum_esr_gdp_mean_1977 <- summary(pool_esr_gdp_mean_1977)

### gdppc ----
esr_gdppc_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_esr_gdppc_mean + ns_esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_esr_gdppc_mean_1977 <- pool(esr_gdppc_mean_1977)
sum_esr_gdppc_mean_1977 <- summary(pool_esr_gdppc_mean_1977)

### combine ----
esr <- list(
  mean = sum_esr_mean_1977,
  gdp = sum_esr_gdp_mean_1977,
  gdppc = sum_esr_gdppc_mean_1977
)

## both ----
### mean ----
both_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_cpr_mean + ns_cpr_mean + ss_esr_mean + ns_esr_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both_mean_1977 <- pool(both_mean_1977)
sum_both_mean_1977 <- summary(pool_both_mean_1977)

### gdp ----
both_gdp_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_cpr_gdp_mean + ns_cpr_gdp_mean + ss_esr_gdp_mean + ns_esr_gdp_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both_gdp_mean_1977 <- pool(both_gdp_mean_1977)
sum_both_gdp_mean_1977 <- summary(pool_both_gdp_mean_1977)

### gdppc ----
both_gdppc_mean_1977 <- with(
  imp_3_l1,
  lm(hr_score ~ ss_cpr_gdppc_mean + ns_cpr_gdppc_mean + ss_esr_gdppc_mean + ns_esr_gdppc_mean + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year)
)

pool_both_gdppc_mean_1977 <- pool(both_gdppc_mean_1977)
sum_both_gdppc_mean_1977 <- summary(pool_both_gdppc_mean_1977)

### combine ----
both <- list(
  mean = sum_both_mean_1977,
  gdp = sum_both_gdp_mean_1977,
  gdppc = sum_both_gdppc_mean_1977
)

## all combine ----
imp_3_hb_res_north <- list(
  general = general,
  cpr = cpr,
  esr = esr,
  both = both
)

# save ----
imp_2_hb_res_north |> 
  save(file = here("data/ch1/results/fits/hb_rep/mice/start_1968/imp_2_hb_res_north.rda"))
imp_3_hb_res_north |> 
  save(file = here("data/ch1/results/fits/hb_rep/mice/start_1977/imp_3_hb_res_north.rda"))
