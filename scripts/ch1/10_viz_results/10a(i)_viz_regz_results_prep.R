# load packages ----
library(tidyverse)
library(here)

# load data ----
files <- dir("data/ch1/results/fits/dml_regularize/pool/", full.names = TRUE)

for(file in files){
  load(here(file))
}

# combine data ----
imp_dml_pool_all <- list(
  no_slx = list(
    general = imp_dml_pool_no_slx_gen,
    south = imp_dml_pool_no_slx_south,
    north = imp_dml_pool_no_slx_north
    ),
  norm_slx = list(
    general = imp_dml_pool_norm_slx_gen#,
    #south = imp_dml_pool_norm_slx_south,
    #north = imp_dml_pool_norm_slx_north
    ),
  bcr_slx = list(
    general = imp_dml_pool_bcr_slx_gen,
    south = imp_dml_pool_bcr_slx_south#,
    #north = imp_dml_pool_bcr_slx_north
    )
  )

# prep data ----
## get initial specs ----
dfs_pre <- list()
#imp_dml_pool_viz_dfs <- list()
imp_dml_pool_viz_dfs_new <- list()
eff_cat <- names(imp_dml_pool_all)

## initialize ----
for(cat in eff_cat){
  list_1 <- imp_dml_pool_all[[cat]]
  mod_type <- names(list_1)
  for(type in mod_type){
    list_2 <- list_1[[type]]
    interact_stat <- names(list_2)
    for(stat in interact_stat){
      list_3 <- list_2[[stat]]
      start_yrs <- names(list_3)
      for(year in start_yrs){
        list_4 <- list_3[[year]]
        lag_names <- names(list_4)
        res_tbl <- tibble()
        for(lag in lag_names){
          list_5 <- list_4[[lag]]
          treat_names <- names(list_5)
          for(treat in treat_names){
            res <- list_5[[treat]] |> 
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
            
            dfs_pre[[as.character(cat)]][[as.character(type)]][[as.character(stat)]][[as.character(year)]] <- res_tbl
          }
        }
      }
    }
  }
}

## reorganize ----
for(cat in eff_cat){
  list_1 <- dfs_pre[[cat]]
  mod_type <- names(list_1)
  for(type in mod_type){
    list_2 <- list_1[[type]]
    interact_stat <- names(list_2)
    for(stat in interact_stat){
      list_3 <- list_2[[stat]]
      start_yrs <- names(list_3)
      for(year in start_yrs){
        year_df <- list_3[[year]]
        treat_names <- unique(year_df$id)
        for(treat in treat_names){
          year_df_sm <- year_df |>
            filter(id == treat) |> 
            select(-id)
          imp_dml_pool_viz_dfs_new[[as.character(cat)]][[as.character(type)]][[as.character(stat)]][[as.character(year)]][[as.character(treat)]] <- year_df_sm
        }
      }
    }
  }
}

# save ----
imp_dml_pool_viz_dfs_new |> 
  save(file = here("data/ch1/viz_prep/regularize/imp_dml_pool_viz_dfs_new.rda"))
