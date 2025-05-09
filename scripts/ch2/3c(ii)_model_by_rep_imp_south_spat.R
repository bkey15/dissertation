# Replicate Bodea-Ye (2018) w/ two-way fixed effects & multiple imputation (`mice`). Fit models w/ imputed vals (& lagged covars). Limited to developing ("Global South") countries, as is the B-Y paper.

# load packages ----
library(tidyverse)
library(here)
library(mice)

# load data ----
load(here("data/ch2/results/imputations/imp_1962_sp_l1.rda"))
load(here("data/ch2/results/imputations/imp_1981_sp_l1.rda"))
load(here("data/ch2/results/imputations/imp_1990_sp_l1.rda"))

# filter to south ----
imp_1962_sp_l1_south <- imp_1962_sp_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(glb_s == 1)

## get treat names
### general
treat_names_gen <- imp_1962_sp_l1_south |> 
  select(
    n_bits,
    starts_with("partner_"),
    -ends_with("sp_lag")
    ) |> 
  names()

### partner type (ss & ns)
treat_names_ss <- imp_1962_sp_l1_south |> 
  select(
    starts_with("ss_"),
    -ends_with("sp_lag")
    ) |> 
  names()

treat_names_ns <- imp_1962_sp_l1_south |> 
  select(
    starts_with("ns_"),
    -ends_with("sp_lag")
    ) |> 
  names()

## get interaction names
### general
interact_names_gen <- imp_1962_sp_l1_south |> 
  select(
    starts_with("e_polity2_x_"),
    -contains(
      c("ss", "ns", "nn")
      ),
    -ends_with("sp_lag")
    ) |> 
  names()

### partner type (ss & ns)
interact_names_ss <- imp_1962_sp_l1_south |> 
  select(
    starts_with("e_polity2_x_ss"),
    -ends_with("sp_lag")
    ) |> 
  names()

interact_names_ns <- imp_1962_sp_l1_south |> 
  select(
    starts_with("e_polity2_x_ns"),
    -ends_with("sp_lag")
    ) |> 
  names()

## as.mids()
imp_1962_sp_l1_south <- imp_1962_sp_l1_south |> 
  as.mids()

## continue filter
imp_1981_sp_l1_south <- imp_1981_sp_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(glb_s == 1) |> 
  as.mids()

imp_1990_sp_l1_south <- imp_1990_sp_l1 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(glb_s == 1) |> 
  as.mids()

# no interaction terms ----
## start 1962 ----
start_1962 <- list()

### general ----
### i.e., no consideration for ns, ss, nn bits
for(name in treat_names_gen){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1962_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1962[[as.character(name)]] <- sum
}

### BITs by partner-type (ss & ns) ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1962_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1962[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## start 1981 ----
start_1981 <- list()

### general ----
### i.e., no consideration for ns, ss, nn bits
for(name in treat_names_gen){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1981_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1981[[as.character(name)]] <- sum
}

### BITs by partner-type (SS & NS) ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1981_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1981[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## start 1990 ----
start_1990 <- list()

### general ----
### i.e., no consideration for ns, ss, nn bits
for(name in treat_names_gen){
  formula <- paste0(
    "hr_score ~ ",
    name,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1990_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1990[[as.character(name)]] <- sum
}

### BITs by partner-type (SS & NS) ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1990_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1990[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## combine ----
no_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# has interaction terms ----
## start 1962 ----
start_1962 <- list()

### general ----
### i.e., no consideration for ns, ss, nn bits
for(i in seq_along(treat_names_gen)){
  j <- treat_names_gen[[i]]
  k <- interact_names_gen[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + e_polity2 + ",
    k,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
    )
  
  fit <- with(imp_1962_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1962[[as.character(j)]] <- sum
}

### BITs by partner-type (ss & ns) ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  l <- interact_names_ss[[i]]
  n <- interact_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + ",
    l,
    " + ",
    n,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1962_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1962[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## start 1981 ----
start_1981 <- list()

### general ----
### i.e., no consideration for ns, ss, nn bits
for(i in seq_along(treat_names_gen)){
  j <- treat_names_gen[[i]]
  k <- interact_names_gen[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + e_polity2 + ",
    k,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1981_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1981[[as.character(j)]] <- sum
}

### BITs by partner-type (ss & ns) ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  l <- interact_names_ss[[i]]
  n <- interact_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + ",
    l,
    " + ",
    n,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1981_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1981[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## start 1990 ----
start_1990 <- list()

### general ----
### i.e., no consideration for ns, ss, nn bits
for(i in seq_along(treat_names_gen)){
  j <- treat_names_gen[[i]]
  k <- interact_names_gen[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + e_polity2 + ",
    k,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1990_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1990[[as.character(j)]] <- sum
}

### BITs by partner-type (ss & ns) ----
for(i in seq_along(treat_names_ss)){
  j <- treat_names_ss[[i]]
  k <- treat_names_ns[[i]]
  l <- interact_names_ss[[i]]
  n <- interact_names_ns[[i]]
  
  formula <- paste0(
    "hr_score ~ ",
    j,
    " + ",
    k,
    " + e_polity2 + ",
    l,
    " + ",
    n,
    " + wdi_fdiin + wdi_trade + p_durable + gdppc_log10 + wdi_popden + v2cademmob + n_bits_sp_lag + factor(cow) + factor(year)"
  )
  
  fit <- with(imp_1990_sp_l1_south, lm(as.formula(formula)))
  
  pool <- pool(fit)
  sum <- summary(pool)
  
  start_1990[[paste(as.character(j), as.character(k), sep = "_AND_")]] <- sum
}

## combine ----
has_interactions <- list(
  start_1962 = start_1962,
  start_1981 = start_1981,
  start_1990 = start_1990
  )

# all combine ----
imp_by_rep_res_south_sp <- list(
  no_interactions = no_interactions,
  has_interactions = has_interactions
  )


# save ----
imp_by_rep_res_south_sp |> 
  save(file = here("data/ch2/results/fits/by_rep/mice/imp_by_rep_res_south_sp.rda"))
