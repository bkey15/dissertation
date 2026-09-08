# Combine individual fits & runtimes

# load packages ----
library(tidyverse)
library(here)

# load resamp splits ----
## quest
### load(here("splits/splits_imp_dml_dats_bcr_slx_RTYPE.rda"))
#### note: ensure directory is changed before entering R session. run: cd /projects/p33143/data/ch1
## pc
### load(here("data/ch1/results/splits/bcr_slx/splits_imp_dml_dats_bcr_slx_RTYPE.rda"))

# get metadata ----
fit_metadata_list <- list()
interact_stat <- names(splits_imp_dml_dats_bcr_slx_#RTYPE)
model_id <- 1

for(stat in interact_stat){
  list_1 <- splits_imp_dml_dats_bcr_slx#_RTYPE[[stat]]
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
        for(i in m){
          fit_metadata_list[[sprintf("%03d", model_id)]] <- list(
            stat = stat,
            year = year,
            lag = lag,
            treat = treat,
            i = i
            )
          model_id <- model_id + 1
        }
      }
    }
  }
}

# combine fits ----
## MTYPE <- list()
model_names <- names(fit_metadata_list)

for(name in model_name){
  # quest:
  # load(
  #   file.path(
  #     "fits/LTYPE/full_dat/MTYPE",
  #     paste0(name, ".rda")
  #     )
  #   )
  
  # pc:
  # load(
  #   file.path(
  #     "quest/ch1/data/fits/LTYPE/full_dat/MTYPE",
  #     paste0(name, ".rda")
  #     )
  #   )
    
    stat <- fit_metadata_list[[name]]$stat
    year <- fit_metadata_list[[name]]$year
    lag <- fit_metadata_list[[name]]$lag
    treat <- fit_metadata_list[[name]]$treat
    i <- fit_metadata_list[[name]]$i
    
    #MTYPE[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
}

# combine runtimes ----
## tictoc_MTYPE <- list()

for(name in model_names){
  # quest:
  # load(
  #   file.path(
  #     "fits/LTYPE/runtimes/MTYPE",
  #     paste0(name, ".rda")
  #     )
  #   )
  
  # pc:
  # load(
  #   file.path(
  #     "quest/ch1/data/fits/LTYPE/runtimes/MTYPE",
  #     paste0(name, ".rda")
  #     )
  #   )
  
  stat <- fit_metadata_list[[name]]$stat
  year <- fit_metadata_list[[name]]$year
  lag <- fit_metadata_list[[name]]$lag
  treat <- fit_metadata_list[[name]]$treat
  i <- fit_metadata_list[[name]]$i
  
  #tictoc_MTYPE[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- tictoc_fit
}

# save results ----
# MTYPE |> 
#   save(file = here("PATH/fits/dml_tree/full_dat/MTYPE.rda"))
# tictoc_MTYPE |> 
#   save(file = here("PATH/runtimes/tictoc_MTYPE.rda"))