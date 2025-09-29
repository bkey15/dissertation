# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mice)
library(janitor)

# load data ----
load(here("data/ch1/results/fits/dml_regularize/full_dat/imp_dml_fits_2fe_south.rda"))

# pool results ----
imp_dml_pool_2fe_south <- list()
interact_stat <- names(imp_dml_fits_2fe_south)

for(stat in interact_stat){
  list_1 <- imp_dml_fits_2fe_south[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
        list_4 <- list_3[[treat]]
        m <- 1:length(list_4)
        prepool_tbl <- tibble()
        for(i in m){
          res <- list_4[[i]]$summary() |> 
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
        imp_dml_pool_2fe_south[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]] <- pool_res
      }
    }
  }
}

# save ----
imp_dml_pool_2fe_south |> 
  save(file = here("data/ch1/results/fits/dml_regularize/pool/imp_dml_pool_2fe_south.rda"))
