# Combine individual fits & runtimes

# load packages ----
library(tidyverse)
library(here)

# combine fits ----
## MTYPE <- list()
# fit_files <- dir("quest/ch2/data/fits/LTYPE/full_dat/MTYPE", full.names = TRUE)

for(file in fit_files){
  load(here(file))
  
  stat <- fit_metadata$stat
  year <- fit_metadata$year
  lag <- fit_metadata$lag
  treat <- fit_metadata$treat
  i <- fit_metadata$i
  
  #MTYPE[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- fit
  
}

# combine runtimes ----
tictoc_imp_dml_fits_bcr_slx_gen_regz_lasso <- list()
# rt_files <- dir("quest/ch2/data/fits/LTYPE/runtimes/MTYPE", full.names = TRUE)

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