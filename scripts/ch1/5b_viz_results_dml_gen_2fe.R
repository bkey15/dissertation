# load packages ----
library(tidyverse)
library(here)
library(tools)
library(knitr)

# load data ----
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_2_dml_fits_pool_2fe.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_2_dml_south_2fe_fits_pool.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_2_dml_north_2fe_fits_pool.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_3_dml_fits_pool_2fe.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_3_dml_south_2fe_fits_pool.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_3_dml_north_2fe_fits_pool.rda"))

# create tables displaying dataset-treatment result ----
# 1968 ----
## general models ----
### mean ----
list_small <- imp_2_dml_fits_pool_2fe[c("cpr_mean", "esr_mean", "both_mean")]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
        ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
        )
      ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = "esr_mean") |> 
  rename(mod_1 = estimate) |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(term = "cpr_mean", .before = 1) |> 
  rename(mod_2 = estimate) |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

gen_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### gdp mean ----
list_small <- imp_2_dml_fits_pool_2fe[c("cpr_gdp_mean", "esr_gdp_mean", "both_gdp_mean")]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = "esr_gdp_mean") |> 
  rename(mod_1 = estimate) |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(term = "cpr_gdp_mean", .before = 1) |> 
  rename(mod_2 = estimate) |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

gen_gdp_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### gdppc mean ----
list_small <- imp_2_dml_fits_pool_2fe[c("cpr_gdppc_mean", "esr_gdppc_mean", "both_gdppc_mean")]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = "esr_gdppc_mean") |> 
  rename(mod_1 = estimate) |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(term = "cpr_gdppc_mean", .before = 1) |> 
  rename(mod_2 = estimate) |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

gen_gdppc_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### combine ----
gen_res_1968 <- list(
  gen_mean_res_1968 = gen_gdp_mean_res_1968,
  gen_gdp_mean_res_1968 = gen_gdp_mean_res_1968,
  gen_gdppc_mean_res_1968 = gen_gdppc_mean_res_1968
  )

## south models ----
### mean ----
list_small <- imp_2_dml_south_2fe_fits_pool[c(
  "ss_cpr_mean_AND_ns_cpr_mean",
  "ss_esr_mean_AND_ns_esr_mean",
  "ss_cpr_mean_AND_ns_cpr_mean_AND_ss_esr_mean_AND_ns_esr_mean"
  )]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("ss_esr_mean", "ns_esr_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("ss_cpr_mean", "ns_cpr_mean"),
    .before = 1
    ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

south_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### gdp mean ----
list_small <- imp_2_dml_south_2fe_fits_pool[c(
  "ss_cpr_gdp_mean_AND_ns_cpr_gdp_mean",
  "ss_esr_gdp_mean_AND_ns_esr_gdp_mean",
  "ss_cpr_gdp_mean_AND_ns_cpr_gdp_mean_AND_ss_esr_gdp_mean_AND_ns_esr_gdp_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("ss_esr_gdp_mean", "ns_esr_gdp_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("ss_cpr_gdp_mean", "ns_cpr_gdp_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

south_gdp_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### gdppc mean ----
list_small <- imp_2_dml_south_2fe_fits_pool[c(
  "ss_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean",
  "ss_esr_gdppc_mean_AND_ns_esr_gdppc_mean",
  "ss_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean_AND_ss_esr_gdppc_mean_AND_ns_esr_gdppc_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("ss_esr_gdppc_mean", "ns_esr_gdppc_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("ss_cpr_gdppc_mean", "ns_cpr_gdppc_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

south_gdppc_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### combine ----
south_res_1968 <- list(
  south_mean_res_1968 = south_mean_res_1968,
  south_gdp_mean_res_1968 = south_gdp_mean_res_1968,
  south_gdppc_mean_res_1968 = south_gdppc_mean_res_1968
)

## north models ----
### mean ----
list_small <- imp_2_dml_north_2fe_fits_pool[c(
  "nn_cpr_mean_AND_ns_cpr_mean",
  "nn_esr_mean_AND_ns_esr_mean",
  "nn_cpr_mean_AND_ns_cpr_mean_AND_nn_esr_mean_AND_ns_esr_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("nn_esr_mean", "ns_esr_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("nn_cpr_mean", "ns_cpr_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

north_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### gdp mean ----
list_small <- imp_2_dml_north_2fe_fits_pool[c(
  "nn_cpr_gdp_mean_AND_ns_cpr_gdp_mean",
  "nn_esr_gdp_mean_AND_ns_esr_gdp_mean",
  "nn_cpr_gdp_mean_AND_ns_cpr_gdp_mean_AND_nn_esr_gdp_mean_AND_ns_esr_gdp_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("nn_esr_gdp_mean", "ns_esr_gdp_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("nn_cpr_gdp_mean", "ns_cpr_gdp_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

north_gdp_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### gdppc mean ----
list_small <- imp_2_dml_north_2fe_fits_pool[c(
  "nn_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean",
  "nn_esr_gdppc_mean_AND_ns_esr_gdppc_mean",
  "nn_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean_AND_nn_esr_gdppc_mean_AND_ns_esr_gdppc_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("nn_esr_gdppc_mean", "ns_esr_gdppc_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("nn_cpr_gdppc_mean", "ns_cpr_gdppc_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

north_gdppc_mean_res_1968 <- cbind(mod_1, mod_2, mod_3)

### combine ----
north_res_1968 <- list(
  north_mean_res_1968 = north_mean_res_1968,
  north_gdp_mean_res_1968 = north_gdp_mean_res_1968,
  north_gdppc_mean_res_1968 = north_gdppc_mean_res_1968
)

# 1977 ----
## general models ----
### mean ----
list_small <- imp_3_dml_fits_pool_2fe[c("cpr_mean", "esr_mean", "both_mean")]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = "esr_mean") |> 
  rename(mod_1 = estimate) |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(term = "cpr_mean", .before = 1) |> 
  rename(mod_2 = estimate) |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

gen_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### gdp mean ----
list_small <- imp_3_dml_fits_pool_2fe[c("cpr_gdp_mean", "esr_gdp_mean", "both_gdp_mean")]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = "esr_gdp_mean") |> 
  rename(mod_1 = estimate) |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(term = "cpr_gdp_mean", .before = 1) |> 
  rename(mod_2 = estimate) |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

gen_gdp_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### gdppc mean ----
list_small <- imp_3_dml_fits_pool_2fe[c("cpr_gdppc_mean", "esr_gdppc_mean", "both_gdppc_mean")]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = "esr_gdppc_mean") |> 
  rename(mod_1 = estimate) |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(term = "cpr_gdppc_mean", .before = 1) |> 
  rename(mod_2 = estimate) |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

gen_gdppc_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### combine ----
gen_res_1977 <- list(
  gen_mean_res_1977 = gen_gdp_mean_res_1977,
  gen_gdp_mean_res_1977 = gen_gdp_mean_res_1977,
  gen_gdppc_mean_res_1977 = gen_gdppc_mean_res_1977
)

## south models ----
### mean ----
list_small <- imp_3_dml_south_2fe_fits_pool[c(
  "ss_cpr_mean_AND_ns_cpr_mean",
  "ss_esr_mean_AND_ns_esr_mean",
  "ss_cpr_mean_AND_ns_cpr_mean_AND_ss_esr_mean_AND_ns_esr_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("ss_esr_mean", "ns_esr_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("ss_cpr_mean", "ns_cpr_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

south_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### gdp mean ----
list_small <- imp_3_dml_south_2fe_fits_pool[c(
  "ss_cpr_gdp_mean_AND_ns_cpr_gdp_mean",
  "ss_esr_gdp_mean_AND_ns_esr_gdp_mean",
  "ss_cpr_gdp_mean_AND_ns_cpr_gdp_mean_AND_ss_esr_gdp_mean_AND_ns_esr_gdp_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("ss_esr_gdp_mean", "ns_esr_gdp_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("ss_cpr_gdp_mean", "ns_cpr_gdp_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

south_gdp_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### gdppc mean ----
list_small <- imp_3_dml_south_2fe_fits_pool[c(
  "ss_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean",
  "ss_esr_gdppc_mean_AND_ns_esr_gdppc_mean",
  "ss_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean_AND_ss_esr_gdppc_mean_AND_ns_esr_gdppc_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("ss_esr_gdppc_mean", "ns_esr_gdppc_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("ss_cpr_gdppc_mean", "ns_cpr_gdppc_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

south_gdppc_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### combine ----
south_res_1977 <- list(
  south_mean_res_1977 = south_mean_res_1977,
  south_gdp_mean_res_1977 = south_gdp_mean_res_1977,
  south_gdppc_mean_res_1977 = south_gdppc_mean_res_1977
)

## north models ----
### mean ----
list_small <- imp_3_dml_north_2fe_fits_pool[c(
  "nn_cpr_mean_AND_ns_cpr_mean",
  "nn_esr_mean_AND_ns_esr_mean",
  "nn_cpr_mean_AND_ns_cpr_mean_AND_nn_esr_mean_AND_ns_esr_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("nn_esr_mean", "ns_esr_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("nn_cpr_mean", "ns_cpr_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

north_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### gdp mean ----
list_small <- imp_3_dml_north_2fe_fits_pool[c(
  "nn_cpr_gdp_mean_AND_ns_cpr_gdp_mean",
  "nn_esr_gdp_mean_AND_ns_esr_gdp_mean",
  "nn_cpr_gdp_mean_AND_ns_cpr_gdp_mean_AND_nn_esr_gdp_mean_AND_ns_esr_gdp_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("nn_esr_gdp_mean", "ns_esr_gdp_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("nn_cpr_gdp_mean", "ns_cpr_gdp_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

north_gdp_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### gdppc mean ----
list_small <- imp_3_dml_north_2fe_fits_pool[c(
  "nn_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean",
  "nn_esr_gdppc_mean_AND_ns_esr_gdppc_mean",
  "nn_cpr_gdppc_mean_AND_ns_cpr_gdppc_mean_AND_nn_esr_gdppc_mean_AND_ns_esr_gdppc_mean"
)]
mod <- seq_along(list_small)

for(i in mod){
  list_small[[i]] <- list_small[[i]] |> 
    select(term, estimate, p.value) |> 
    mutate(
      estimate = as.character(
        round(estimate, digits = 3)
      ),
      estimate = case_when(
        p.value <= 0.1 & p.value > 0.05 ~ paste0(estimate, "*"),
        p.value <= 0.05 & p.value > 0.01 ~ paste0(estimate, "**"),
        p.value <= 0.01 ~ paste0(estimate, "***"),
        .default = estimate
      )
    ) |> 
    select(-p.value)
}

mod_1 <- list_small[[1]] |> 
  add_row(term = c("nn_esr_gdppc_mean", "ns_esr_gdppc_mean")) |> 
  rename(mod_1 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_2 <- list_small[[2]] |> 
  add_row(
    term = c("nn_cpr_gdppc_mean", "ns_cpr_gdppc_mean"),
    .before = 1
  ) |> 
  rename(mod_2 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")
mod_3 <- list_small[[3]] |> 
  rename(mod_3 = estimate) |> 
  remove_rownames() |> 
  column_to_rownames(var = "term")

north_gdppc_mean_res_1977 <- cbind(mod_1, mod_2, mod_3)

### combine ----
north_res_1977 <- list(
  north_mean_res_1977 = north_mean_res_1977,
  north_gdp_mean_res_1977 = north_gdp_mean_res_1977,
  north_gdppc_mean_res_1977 = north_gdppc_mean_res_1977
)

# kabelize ----
## 1968 ----
### general models ----

## 1977 ----
### general models ----

# combine all ----
imp_pool_tbls <- list(
  gen_res_1968 = gen_res_1968,
  south_res_1968 = south_res_1968,
  north_res_1968 = north_res_1968,
  gen_res_1977 = gen_res_1977,
  south_res_1977 = south_res_1977,
  north_res_1977 = north_res_1977
  )

# save ----
imp_pool_tbls |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_pool_tbls.rda"))
