# Combine individual fits & runtimes

# load packages ----
library(tidyverse)
library(here)

# combine fits ----
imp_dml_fits_bcr_slx_class_tree_rf <- list()
fit_files <- dir("quest/ch3/data/fits/dml_tree/full_dat/imp_dml_fits_bcr_slx_class_tree_rf", full.names = TRUE)

for(file in fit_files){
  load(here(file))
  
  stat <- fit_metadata$stat
  year <- fit_metadata$year
  lag <- fit_metadata$lag
  treat <- fit_metadata$treat
  i <- fit_metadata$i
  
  imp_dml_fits_bcr_slx_class_tree_rf[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
  
}

# combine runtimes ----
# tictoc_MTYPE <- list()
# rt_files <- dir("quest/ch3/data/fits/LTYPE/runtimes/MTYPE", full.names = TRUE)

for(file in rt_files){
  load(here(file))
  
  stat <- fit_metadata$stat
  year <- fit_metadata$year
  lag <- fit_metadata$lag
  treat <- fit_metadata$treat
  i <- fit_metadata$i
  
  #tictoc_MTYPE[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- tictoc_fit
}

# save results ----
# MTYPE |> 
#   save(file = here("PATH/fits/LTYPE/full_dat/MTYPE.rda"))
# tictoc_MTYPE |> 
#   save(file = here("PATH/runtimes/tictoc_MTYPE.rda"))