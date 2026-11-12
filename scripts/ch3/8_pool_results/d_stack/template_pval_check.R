# interact_stat <- names(imp_dml_pool_bcr_slx_TYPE_stack)

for(stat in interact_stat){
  # list_1 <- imp_dml_pool_bcr_slx_TYPE_stack[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
        list_4 <- list_3[[treat]]
        if(any(list_4[["p.value"]] < 0.1)){
          print(stat)
          print(year)
          print(lag)
          print(treat)
        }
      }
    }
  }
}
