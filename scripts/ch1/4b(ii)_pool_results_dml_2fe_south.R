# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mice)
library(janitor)
library(knitr)

# load data ----
load(here("data/ch1/results/fits/dml_lasso/dml_final/imp_dml_fits_2fe_south.rda"))

# pool results ----
imp_dml_pool_2fe_south <- list()
start_yrs <- names(imp_dml_fits_2fe_south)
treat_names <- names(imp_dml_fits_2fe_south[[1]])
m <- 1:length(imp_dml_fits_2fe_south[[1]][[1]])

for(year in start_yrs){
  list_1 <-  imp_dml_fits_2fe_south[[year]]
  for(name in treat_names){
    list_2 <- list_1[[name]]
    prepool_tbl <- tibble()
    for(i in m){
      res <- list_2[[i]]$summary() |> 
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
  imp_dml_pool_2fe_south[[as.character(year)]][[as.character(name)]] <- pool_res
  }
  }

# save ----
imp_dml_pool_2fe_south |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_final/imp_dml_pool_2fe_south.rda"))
