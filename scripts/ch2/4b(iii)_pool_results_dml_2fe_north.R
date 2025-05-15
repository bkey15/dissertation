# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mice)
library(janitor)
library(knitr)

# load data ----
load(here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_fits_2fe_north.rda"))

# pool results ----
imp_dml_pool_2fe_north <- list()
interact_stat <- names(imp_dml_fits_2fe_north)
start_yrs <- names(imp_dml_fits_2fe_north[[1]])
m <- 1:length(imp_dml_fits_2fe_north[[1]][[1]][[1]])

for(stat in interact_stat){
  list_1 <- imp_dml_fits_2fe_north[[stat]]
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    treat_names <- names(list_2)
    for(name in treat_names){
      list_3 <- list_2[[name]]
      prepool_tbl <- tibble()
      for(i in m){
        res <- list_3[[i]]$summary() |> 
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
      imp_dml_pool_2fe_north[[as.character(stat)]][[as.character(year)]][[as.character(name)]] <- pool_res
    }
  }
  }

# save ----
imp_dml_pool_2fe_north |> 
  save(file = here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_pool_2fe_north.rda"))
