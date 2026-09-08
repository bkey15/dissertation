# Computing combined nuisance loss (cnl). See Bach et al. (2024b).

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)

# load data ----
load(here("data/ch1/results/fits/dml_regularize/full_dat/imp_dml_fits_no_slx_gen.rda"))

# compute cnls ----
imp_dml_cnl_no_slx_gen <- list()
interact_stat <- names(imp_dml_fits_no_slx_gen)

for(stat in interact_stat){
  list_1 <- imp_dml_fits_no_slx_gen[[stat]]
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
          # get outcome
          y_name <- list_4[[i]]$data$y_col
          y <- list_4[[i]]$data$data[[y_name]]
          
          # get n obs
          n <- length(y)
          
          # get n reps
          n_reps <- 1:list_4[[i]]$n_rep
          
          # get n treats
          n_d <- 1:length(list_4[[i]]$data$d_cols)
          
          for(d_num in n_d){
            d_name <- list_4[[i]]$data$d_cols[[d_num]]
            d <- list_4[[i]]$data$data[[d_name]]
            
            # make cnl storage list
            cnl_list <- list()
            
            for(rep in n_reps){
              # get preds
              ## ml_l
              pred_ml_l <- list_4[[i]]$predictions$ml_l[,,d_num][,rep]
              
              ## ml_m
              pred_ml_m <- list_4[[i]]$predictions$ml_m[,,d_num][,rep]
              
              # compute rmse
              ## ml_l
              rmse_ml_l <- sqrt(sum((y - pred_ml_l)^2)/n)
              
              ## ml_m
              rmse_ml_m <- sqrt(sum((d - pred_ml_m)^2)/n)
              
              # compute & store cnl
              cnl_list[[rep]] <- rmse_ml_m*(rmse_ml_m + rmse_ml_l)
              }
            
            imp_dml_cnl_no_slx_gen[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(d_name)]] <- cnl_list
          }
        }
      }
    }
  }
}
