# load packages ----
library(tidyverse)
library(here)
library(DoubleML)
library(mice)
library(janitor)
library(knitr)

# load data ----
load(here("data/ch3/results/fits/dml_lasso/dml_final/imp_dml_fits_2fe.rda"))

# pool results ----
imp_dml_pool_2fe <- list()
start_yrs <- names(imp_dml_fits_2fe)
lag_names <- names(imp_dml_fits_2fe[[1]])
treat_names <- names(imp_dml_fits_2fe[[1]][[1]])
m <- 1:length(imp_dml_fits_2fe[[1]][[1]][[1]])

for(year in start_yrs){
  list_1 <- imp_dml_fits_2fe[[year]]
  for(lag in lag_names){
    list_2 <- list_1[[lag]]
    for(treat in treat_names){
      list_3 <- list_2[[treat]]
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
      imp_dml_pool_2fe[[as.character(year)]][[as.character(lag)]][[as.character(treat)]] <- pool_res
    }
  }
}

# save ----
imp_dml_pool_2fe |> 
  save(file = here("data/ch3/results/fits/dml_lasso/dml_final/imp_dml_pool_2fe.rda"))
