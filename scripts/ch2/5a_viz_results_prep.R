# load packages ----
library(tidyverse)
library(here)

# load data ----
## 2fe ----
load(here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_pool_2fe_gen.rda"))
load(here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_pool_2fe_south.rda"))
load(here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_pool_2fe_north.rda"))

## spat_regfe ----
load(here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_pool_spat_regfe_gen.rda"))
load(here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_pool_spat_regfe_south.rda"))
load(here("data/ch2/results/fits/dml_lasso/dml_final/imp_dml_pool_spat_regfe_north.rda"))

# combine data ----
imp_dml_pool_2fe_all <- list(
  general = imp_dml_pool_2fe_gen,
  south = imp_dml_pool_2fe_south,
  north = imp_dml_pool_2fe_north
  )

imp_dml_pool_spat_regfe_all <- list(
  general = imp_dml_pool_spat_regfe_gen,
  south = imp_dml_pool_spat_regfe_south,
  north = imp_dml_pool_spat_regfe_north
  )

# get main data ----
## 2fe ----
dfs_pre <- list()
imp_dml_pool_2fe_viz_dfs <- list()
mod_type <- names(imp_dml_pool_2fe_all)

### initialize
for(type in mod_type){
  list_1 <- imp_dml_pool_2fe_all[[type]]
  interact_stat <- names(list_1)
  for(stat in interact_stat){
    list_2 <- list_1[[stat]]
    start_yrs <- names(list_2)
    for(year in start_yrs){
      list_3 <- list_2[[year]]
      lag_names <- names(list_3)
      res_tbl <- tibble()
      for(lag in lag_names){
        list_4 <- list_3[[lag]]
        treat_names <- names(list_4)
        for(treat in treat_names){
          res <- list_4[[treat]] |> 
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
          
          dfs_pre[[as.character(type)]][[as.character(stat)]][[as.character(year)]] <- res_tbl
        }
      }
    }
  }
}

### reorganize
for(type in mod_type){
  list_1 <- dfs_pre[[type]]
  interact_stat <- names(list_1)
  for(stat in interact_stat){
    list_2 <- list_1[[stat]]
    start_yrs <- names(list_2)
    for(year in start_yrs){
      year_df <- list_2[[year]]
      treat_names <- unique(year_df$id)
      for(treat in treat_names){
        year_df_sm <- year_df |> 
          filter(id == treat) |> 
          select(-id)
        imp_dml_pool_2fe_viz_dfs[[as.character(type)]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- year_df_sm
      }
    }
  }
}

## spat_regfe ----
dfs_pre <- list()
imp_dml_pool_spat_regfe_viz_dfs <- list()
mod_type <- names(imp_dml_pool_spat_regfe_all)

### initialize
for(type in mod_type){
  list_1 <- imp_dml_pool_spat_regfe_all[[type]]
  interact_stat <- names(list_1)
  for(stat in interact_stat){
    list_2 <- list_1[[stat]]
    start_yrs <- names(list_2)
    for(year in start_yrs){
      list_3 <- list_2[[year]]
      lag_names <- names(list_3)
      res_tbl <- tibble()
      for(lag in lag_names){
        list_4 <- list_3[[lag]]
        treat_names <- names(list_4)
        for(treat in treat_names){
          res <- list_4[[treat]] |> 
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
          
          dfs_pre[[as.character(type)]][[as.character(stat)]][[as.character(year)]] <- res_tbl
        }
      }
    }
  }
}

### reorganize
for(type in mod_type){
  list_1 <- dfs_pre[[type]]
  interact_stat <- names(list_1)
  for(stat in interact_stat){
    list_2 <- list_1[[stat]]
    start_yrs <- names(list_2)
    for(year in start_yrs){
      year_df <- list_2[[year]]
      treat_names <- unique(year_df$id)
      for(treat in treat_names){
        year_df_sm <- year_df |> 
          filter(id == treat) |> 
          select(-id)
        imp_dml_pool_spat_regfe_viz_dfs[[as.character(type)]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- year_df_sm
      }
    }
  }
}

# save ----
imp_dml_pool_2fe_viz_dfs |> 
  save(file = here("data/ch2/viz_prep/imp_dml_pool_2fe_viz_dfs.rda"))
imp_dml_pool_spat_regfe_viz_dfs |> 
  save(file = here("data/ch2/viz_prep/imp_dml_pool_spat_regfe_viz_dfs.rda"))

