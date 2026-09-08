library(tidyverse)
library(here)

source(here("scripts/ch1/4_prep_model/a_regz/ii_slx/4a(vi)_prep_regz_model_dml_slx_north.R"))

interact_stat <- names(imp_dml_dats_slx_north)
task_id <- 0

for(stat in interact_stat){
  list_1 <- imp_dml_dats_slx_north[[stat]]
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
                "quest/ch1/data_splits/imp_dml_dats_slx_north",
                sprintf("%03d.rda", task_id)
                )
              )
        }
      }
    }
  }
}
