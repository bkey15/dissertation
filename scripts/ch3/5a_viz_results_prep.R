# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch3/results/fits/dml_lasso/dml_final/imp_dml_pool_2fe.rda"))
load(here("data/ch3/results/fits/dml_lasso/dml_final/imp_dml_pool_spat_regfe.rda"))

# get main data ----
## 2fe ----
dfs_pre <- list()
imp_dml_pool_2fe_viz_dfs <- list()
interact_stat <- names(imp_dml_pool_2fe)

### initialize
for(stat in interact_stat){
  list_1 <- imp_dml_pool_2fe[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    res_tbl <- tibble()
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
          res <- list_3[[treat]] |> 
            janitor::clean_names() |> 
            select(
              term,
              estimate,
              p_value,
              starts_with("conf")
              ) |> 
            mutate(
              id = treat,
              n_lag = as.numeric(
                str_extract_all(
                  lag,
                  pattern = "\\d+"
                )
              )
            ) |> 
            relocate(id, term, n_lag)
          
          res_tbl <- res_tbl |> 
            rbind(res)
          
          dfs_pre[[as.character(stat)]][[as.character(year)]] <- res_tbl
      }
    }
  }
}

### reorganize
for(stat in interact_stat){
  list_1 <- dfs_pre[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    year_df <- list_1[[year]]
    treat_names <- unique(year_df$id)
    for(treat in treat_names){
      year_df_sm <- year_df |> 
        filter(id == treat) |> 
        select(-id)
      
      imp_dml_pool_2fe_viz_dfs[[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- year_df_sm
    }
  }
}

## spat_regfe ----
dfs_pre <- list()
imp_dml_pool_spat_regfe_viz_dfs <- list()
interact_stat <- names(imp_dml_pool_spat_regfe)

### initialize
for(stat in interact_stat){
  list_1 <- imp_dml_pool_spat_regfe[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    res_tbl <- tibble()
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
        res <- list_3[[treat]] |> 
          janitor::clean_names() |> 
          select(
            term,
            estimate,
            p_value,
            starts_with("conf")
            ) |> 
          mutate(
            id = treat,
            n_lag = as.numeric(
              str_extract_all(
                lag,
                pattern = "\\d+"
              )
            )
          ) |> 
          relocate(id, term, n_lag)
        
        res_tbl <- res_tbl |> 
          rbind(res)
        
        dfs_pre[[as.character(stat)]][[as.character(year)]] <- res_tbl
      }
    }
  }
}

### reorganize
for(stat in interact_stat){
  list_1 <- dfs_pre[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    year_df <- list_1[[year]]
    treat_names <- unique(year_df$id)
    for(treat in treat_names){
      year_df_sm <- year_df |> 
        filter(id == treat) |> 
        select(-id)
      
      imp_dml_pool_spat_regfe_viz_dfs[[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- year_df_sm
    }
  }
}

# save ----
imp_dml_pool_2fe_viz_dfs |> 
  save(file = here("data/ch3/viz_prep/imp_dml_pool_2fe_viz_dfs.rda"))
imp_dml_pool_spat_regfe_viz_dfs |> 
  save(file = here("data/ch3/viz_prep/imp_dml_pool_spat_regfe_viz_dfs.rda"))

