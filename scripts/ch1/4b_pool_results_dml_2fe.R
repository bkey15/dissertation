# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mice)
library(janitor)
library(knitr)

# load data ----
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_1_dml_fits_2fe.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_2_dml_fits_2fe.rda"))
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_3_dml_fits_2fe.rda"))

# pool results ----
## imp_1 ----
imp_1_dml_fits_pool_2fe <- list()
treat_names <- imp_1_dml_fits_2fe |> 
  names()
m <- 1:5

for(name in treat_names){
  list <- imp_1_dml_fits_2fe[[name]]
  prepool_tbl <- tibble()
  
  for(i in m){
    res <- list[[i]]$summary() |> 
      as.data.frame() |> 
      rownames_to_column() |> 
      clean_names() |> 
      rename(
        term = rowname,
        std.error = std_error
        ) |> 
      select(term, estimate, std.error)
    prepool_tbl <- prepool_tbl |> 
      rbind(res)
  }
  
  pool_res <- prepool_tbl |> 
    pool.table()
  imp_1_dml_fits_pool_2fe[[as.character(name)]] <- pool_res
}

## imp_2 ----
imp_2_dml_fits_pool_2fe <- list()
treat_names <- imp_2_dml_fits_2fe |> 
  names()
m <- 1:5

for(name in treat_names){
  list <- imp_2_dml_fits_2fe[[name]]
  prepool_tbl <- tibble()
  
  for(i in m){
    res <- list[[i]]$summary() |> 
      as.data.frame() |> 
      rownames_to_column() |> 
      clean_names() |> 
      rename(
        term = rowname,
        std.error = std_error
      ) |> 
      select(term, estimate, std.error)
    prepool_tbl <- prepool_tbl |> 
      rbind(res)
  }
  
  pool_res <- prepool_tbl |> 
    pool.table()
  imp_2_dml_fits_pool_2fe[[as.character(name)]] <- pool_res
}

## imp_3 ----
imp_3_dml_fits_pool_2fe <- list()
treat_names <- imp_3_dml_fits_2fe |> 
  names()
m <- 1:5

for(name in treat_names){
  list <- imp_3_dml_fits_2fe[[name]]
  prepool_tbl <- tibble()
  
  for(i in m){
    res <- list[[i]]$summary() |> 
      as.data.frame() |> 
      rownames_to_column() |> 
      clean_names() |> 
      rename(
        term = rowname,
        std.error = std_error
      ) |> 
      select(term, estimate, std.error)
    prepool_tbl <- prepool_tbl |> 
      rbind(res)
  }
  
  pool_res <- prepool_tbl |> 
    pool.table()
  imp_3_dml_fits_pool_2fe[[as.character(name)]] <- pool_res
}

# save ----
imp_1_dml_fits_pool_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_1_dml_fits_pool_2fe.rda"))
imp_2_dml_fits_pool_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_2_dml_fits_pool_2fe.rda"))
imp_3_dml_fits_pool_2fe |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_3_dml_fits_pool_2fe.rda"))
