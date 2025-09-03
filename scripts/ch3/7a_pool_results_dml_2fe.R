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
imp_dml_fits_2fe <- jlb$load(filename = "data/ch3/results/fits/dml_lasso/full_dat/imp_dml_fits_2fe.gz")

# prep pool ----
## concat framework attributes ----
treat_num_stat <- names(imp_dml_fits_2fe)

for(stat in treat_num_stat){
  if(str_detect(stat, "multi")){
    list_1 <- imp_dml_fits_2fe[[stat]]
    start_yrs <- names(list_1)
    for(year in start_yrs){
      list_2 <- list_1[[year]]
      lag_names <- names(list_2)
      for(lag in lag_names){
        list_3 <- list_2[[lag]]
        treat_names <- list_3 |> 
          names() |> 
          str_subset("inforce")
        treat_name_1 <- treat_names[[1]]
        treat_name_2 <- treat_names[[2]]
        list_4_treat_1 <- list_3[[treat_name_1]]
        list_4_treat_2 <- list_3[[treat_name_2]]
        m <- 1:length(list_4_treat_1)
        for(i in m){
          frame_treat_1 <- list_4_treat_1[[i]][["framework"]]
          frame_treat_2 <- list_4_treat_2[[i]][["framework"]]
          concat_res <- dml$concat(list(frame_treat_1, frame_treat_2))
          imp_dml_fits_2fe[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[paste(as.character(treat_name_1), as.character(treat_name_2), sep = "_AND_")]][[as.character(i)]] <- concat_res
        }
      }
    }
  }
}

## filter out unconcat treats ----
for(stat in treat_num_stat){
  if(str_detect(stat, "multi")){
    list_1 <- imp_dml_fits_2fe[[stat]]
    start_yrs <- names(list_1)
    for(year in start_yrs){
      list_2 <- list_1[[year]]
      lag_names <- names(list_2)
      for(lag in lag_names){
        list_3 <- list_2[[lag]]
        filter_res <- list_3[str_detect(names(list_3), "AND")]
        imp_dml_fits_2fe[[as.character(stat)]][[as.character(year)]][[as.character(lag)]] <- filter_res
      }
    }
  }
}

# pool results ----
imp_dml_pool_2fe <- list()

for(stat in treat_num_stat){
  list_1 <- imp_dml_fits_2fe[[stat]]
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
        imp_dml_pool_2fe[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]] <- pool_res
      }
    }
  }
}

# rename main list branches ----
names(imp_dml_pool_2fe) <- c("no_interactions", "has_interactions")

# save ----
imp_dml_pool_2fe |> 
  save(file = here("data/ch3/results/fits/dml_lasso/pool/imp_dml_pool_2fe.rda"))
