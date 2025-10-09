# load packages ----
library(tidyverse)
library(here)
library(mice)
library(janitor)
library(reticulate)

# load python modules ----
py_require(packages = "doubleml")
py_require(packages = "joblib")

dml <- import("doubleml")
jlb <- import("joblib")

# load data ----
files <- dir("data/ch3/results/fits/dml_regularize/full_dat", full.names = TRUE)

for(file in files){
  if(str_detect(file, "2fe_regr")){
  load(here(file))
  }
  else if(str_detect(file, "2fe_class")){
    imp_dml_fits_2fe_class <- jlb$load(filename = file)
  }
}

# pool results ----
## regr ----
### get initial specs ----
imp_dml_pool_2fe <- list()
interact_stat <- names(imp_dml_fits_2fe_regr)

### finalize ----
for(stat in interact_stat){
  list_1 <- imp_dml_fits_2fe_regr[[stat]]
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
        imp_dml_pool_2fe[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]] <- pool_res
      }
    }
  }
}

## class ----
### get initial specs ----
imp_dml_pool_2fe_class <- list()
treat_num_stat <- names(imp_dml_fits_2fe_class)

### finalize ----
for(stat in treat_num_stat){
  list_1 <- imp_dml_fits_2fe_class[[stat]]
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
          res <- list_4[[i]]$summary |> 
            rownames_to_column() |> 
            clean_names() |> 
            rename(
              term = rowname,
              estimate = coef,
              std.error = std_err
              ) |> 
            select(term, estimate, std.error)
          prepool_tbl <- prepool_tbl |> 
            rbind(res)
          }
        
        pool_res <- prepool_tbl |> 
          pool.table()
        imp_dml_pool_2fe_class[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]] <- pool_res
      }
    }
  }
}

### reorganize ----
for(stat in treat_num_stat){
  list_1 <- imp_dml_pool_2fe_class[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      if(str_detect(stat, "single")){
        imp_dml_pool_2fe[["no_interactions"]][[as.character(year)]][[as.character(lag)]][[as.character(treat_names)]] <- list_3[[1]]
        }
      else{
        imp_dml_pool_2fe[["has_interactions"]][[as.character(year)]][[as.character(lag)]][[paste(as.character(treat_names[[1]]), as.character(treat_names[[2]]), sep = "_AND_")]] <- rbind(list_3[[1]], list_3[[2]])
      }
    }
  }
}

# save ----
imp_dml_pool_2fe |> 
  save(file = here("data/ch3/results/fits/dml_regularize/pool/imp_dml_pool_2fe.rda"))
