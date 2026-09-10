# Split DML dats for Quest fits

# load packages ----
library(tidyverse)
library(here)

# load all dats
# source(here("scripts/ch1/4_prep_model/a_regz/i_no_slx/....R"))
# source(here("scripts/ch1/4_prep_model/a_regz/ii_slx/....R"))

# interact_stat <- names(imp_dml_dats_...)
task_id <- 0

for(stat in interact_stat){
  # list_1 <- imp_dml_dats_...[[stat]]
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
          task_id <- task_id + 1
          
          model_info <- list(
            data = list_4[[i]],
            stat = stat,
            year = year,
            lag = lag,
            treat = treat,
            i = i,
            task_id = task_id
            )
          
          model_info |> 
            save(
              file = here(
                # "quest/ch1/data_splits/imp_dml_dats_...",
                sprintf("%03d.rda", task_id)
                )
              )
        }
      }
    }
  }
}
