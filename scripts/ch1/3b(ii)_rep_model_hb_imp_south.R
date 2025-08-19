# Replicate Hafner-Burton (2005) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals (& lagged covars). Limited to "Global South" countries.

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch1/results/imputations/imp_1968_t_lags.rda"))
load(here("data/ch1/results/imputations/imp_1977_t_lags.rda"))

## L1 ----
hb_rep_1968_l1 <- imp_1968_t_lags[["l1"]]
hb_rep_1977_l1 <- imp_1977_t_lags[["l1"]]

# filter to south ----
## note: logged event that as.mids() yields is glb_s (no variation; all == 1)
imp_1968_l1_south <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(glb_s == 1) |> 
  as.mids()

imp_1977_l1_south <- hb_rep_1977_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(glb_s == 1) |> 
  as.mids()

## get treat names
### general
treat_names_gen <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    contains("mean"),
    -contains(c("pop", "_x_")),
    -starts_with(
      c("ss_", "ns_", "nn_")
      )
    ) |> 
  names()

### partner type (ss & ns)
#### ss
treat_names_ss <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("ss_"),
    -contains("pop")
    ) |> 
  names()

treat_names_ss_cpr <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("ss_cpr"),
    -contains("pop")
    ) |> 
  names()

treat_names_ss_esr <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("ss_esr"),
    -contains("pop")
    ) |> 
  names()

#### ns
treat_names_ns <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("ns_"),
    -contains("pop")
    ) |> 
  names()

treat_names_ns_cpr <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("ns_cpr"),
    -contains("pop")
    ) |> 
  names()

treat_names_ns_esr <- hb_rep_1968_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  select(
    starts_with("ns_esr"),
    -contains("pop")
    ) |> 
  names()

# start 1968 ----
start_1968 <- list()

## general ----
## i.e., no consideration for ns, ss, nn ptas
for(name in treat_names_gen){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(imp_1968_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1968[[as.character(name)]] <- sum
}

## ptas by partner-type ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(imp_1968_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1968[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## cpr & esr + partner-type ----
for(i in seq_along(treat_names_ss_cpr)){
  j <- treat_names_ss_cpr[[i]]
  k <- treat_names_ns_cpr[[i]]
  l <- treat_names_ss_esr[[i]]
  m <- treat_names_ns_esr[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + ",
    l,
    " + ",
    m,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(imp_1968_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1968[[paste(as.character(j), as.character(k), as.character(l), as.character(m), sep = "_AND_")]] <- sum
}

# start 1977 ----
start_1977 <- list()

## general ----
## i.e., no consideration for ns, ss, nn ptas
for(name in treat_names_gen){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(imp_1977_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1977[[as.character(name)]] <- sum
}

## ptas by partner-type ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(imp_1977_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1977[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## cpr & esr + partner-type ----
for(i in seq_along(treat_names_ss_cpr)){
  j <- treat_names_ss_cpr[[i]]
  k <- treat_names_ns_cpr[[i]]
  l <- treat_names_ss_esr[[i]]
  m <- treat_names_ns_esr[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + ",
    l,
    " + ",
    m,
    " + e_polity2 + p_durable + wdi_popden + wdi_trade + inv + gdppc_log10 + hras + cow + year"
  )
  
  fit <- with(imp_1977_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1977[[paste(as.character(j), as.character(k), as.character(l), as.character(m), sep = "_AND_")]] <- sum
}

# combine ----
imp_hb_rep_res_south <- list(
  start_1968 = start_1968,
  start_1977 = start_1977
  )

# save ----
imp_hb_rep_res_south |> 
  save(file = here("data/ch1/results/fits/hb_rep/mice/imp_hb_rep_res_south.rda"))
