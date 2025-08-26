# load packages ----
library(tidyverse)
library(glmnet)

# load data ----
by_rep_mice_subfolder <- "data/ch2/results/fits/by_rep/mice"

by_rep_mice_files <- list.files(by_rep_mice_subfolder, pattern = "\\.rda$", full.names = TRUE)

for(file in by_rep_mice_files){
  load(file)
}

# create data tables ----
## all countries ----




sum_cpr1_nost <- sum_cpr1_nost |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = "none",
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr1_nost <- sum_esr1_nost |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = "none"
    ) |> 
  relocate(cpr_esr, start_year)

### 1968 ----
sum_cpr1_1968 <- sum_cpr1_1968 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = 1968
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr1_1968 <- sum_esr1_1968 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = 1968
  ) |> 
  relocate(cpr_esr, start_year)

### 1977 ----
sum_cpr1_1977 <- sum_cpr1_1977 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = 1977
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr1_1977 <- sum_esr1_1977 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = 1977
    ) |> 
  relocate(cpr_esr, start_year)

### merge ----
sum_mice_mean <- sum_cpr1_nost |> 
  rbind(
    sum_esr1_nost,
    sum_cpr1_1968,
    sum_esr1_1968,
    sum_cpr1_1977,
    sum_esr1_1977
    ) |> 
  arrange(cpr_esr, start_year)

## gdp_mean ----
### no start year ----
sum_cpr2_nost <- sum_cpr2_nost |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = "none",
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr2_nost <- sum_esr2_nost |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = "none"
    ) |> 
  relocate(cpr_esr, start_year)

### 1968 ----
sum_cpr2_1968 <- sum_cpr2_1968 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = 1968
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr2_1968 <- sum_esr2_1968 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = 1968
    ) |> 
  relocate(cpr_esr, start_year)

### 1977 ----
sum_cpr2_1977 <- sum_cpr2_1977 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = 1977
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr2_1977 <- sum_esr2_1977 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = 1977
    ) |> 
  relocate(cpr_esr, start_year)

### merge ----
sum_mice_gdp_mean <- sum_cpr2_nost |> 
  rbind(
    sum_esr2_nost,
    sum_cpr2_1968,
    sum_esr2_1968,
    sum_cpr2_1977,
    sum_esr2_1977
    ) |> 
  arrange(cpr_esr, start_year)

## gdppc_mean ----
### no start year ----
sum_cpr3_nost <- sum_cpr3_nost |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = "none",
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr3_nost <- sum_esr3_nost |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = "none"
    ) |> 
  relocate(cpr_esr, start_year)

### 1968 ----
sum_cpr3_1968 <- sum_cpr3_1968 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = 1968
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr3_1968 <- sum_esr3_1968 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = 1968
    ) |> 
  relocate(cpr_esr, start_year)

### 1977 ----
sum_cpr3_1977 <- sum_cpr3_1977 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "cpr",
    start_year = 1977
    ) |> 
  relocate(cpr_esr, start_year)

sum_esr3_1977 <- sum_esr3_1977 |> 
  slice_head(n = 8) |> 
  mutate(
    cpr_esr = "esr",
    start_year = 1977
    ) |> 
  relocate(cpr_esr, start_year)

### merge ----
sum_mice_gdppc_mean <- sum_cpr3_nost |> 
  rbind(
    sum_esr3_nost,
    sum_cpr3_1968,
    sum_esr3_1968,
    sum_cpr3_1977,
    sum_esr3_1977
    ) |> 
  arrange(cpr_esr, start_year)
