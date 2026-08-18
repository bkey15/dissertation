# Get spatially buffered two-way cluster-robust splits.
# Note: code contains a good deal of clearing garbage/removed data to optimize available memory and speed-up computation times. Also using data.table functions for this purpose.

# load packages ----
library(tidyverse)
library(data.table)
library(here)
library(DoubleML)
library(tidymodels)
library(spatialsample)
library(sf)

# set seed ----
## important: set seed before running prep script
set.seed(15275)

# run prep script ----
# note: sf::st_union() may be used to create region polygons
#source(here("scripts/ch1/4_prep_model/a_regz/spat_regfe/4a(iv)_prep_regz_model_dml_spat_regfe_north.R"))
source(here("scripts/ch1/4_prep_model/a_regz/spat_cfe/4a(vi)_prep_regz_model_dml_spat_cfe_north.R"))

# load country polygons ----
load(here("data/ch1/preprocessed/world_base.rda"))
world_base <- world_base |> 
  mutate(cow = as.factor(cow))

# get unique cow & year; get row ids ----
## note: doing so before spatial buffering to remove initialized dml data and thus clear up memory
## note: imp_dml_dats_spat_cfe_north has already been filtered such that only countries with polygons appear (see preproc code), so no need to filter further at this stage.
## note: economizing code, noting that row ids only differ by lag
interact_stat <- names(imp_dml_dats_spat_cfe_north)
start_yrs <- names(imp_dml_dats_spat_cfe_north[[1]])
lag_names <- names(imp_dml_dats_spat_cfe_north[[1]][[1]])[1:4]
treat_names_reg <- names(imp_dml_dats_spat_cfe_north[[1]][[1]][[1]])
treat_names_interact <- names(imp_dml_dats_spat_cfe_north[[2]][[1]][[1]])
m <- 1:length(imp_dml_dats_spat_cfe_north[[1]][[1]][[1]][[1]])
cow_yr_uniques <- list()
row_ids <- list()

for(year in start_yrs){
  for(lag in lag_names){
    dt <- imp_dml_dats_spat_cfe_north[[1]][[year]][[lag]][[1]][[1]]$data[, .(cow, year)]
    
    cow_unique <- dt[, cow] |> 
      unique()
    yr_unique <- dt[, year] |> 
      unique()
    
    row_ids_dt <- dt[, row_id := 1:nrow(dt)]
    
    cow_yr_uniques[[as.character(year)]][[as.character(lag)]] <- list(cow = cow_unique, year = yr_unique)
    row_ids[[as.character(year)]][[as.character(lag)]] <- row_ids_dt
  }
}

rm(
  list = setdiff(
    ls(),
    c(
      "world_base",
      "interact_stat",
      "start_yrs",
      "lag_names",
      "treat_names_reg",
      "treat_names_interact",
      "m",
      "cow_yr_uniques",
      "row_ids"
      )
    )
  )
gc()

# set folds & reps ----
nf <- 10
nr <- 3

# make buffered + 2-way cluster-robust splits ----
#purrr::walk(test_splits_cow$splits, function(x) print(autoplot(x)))
## get spatial buffered & year splits ----
### note: buffer = 1000m*200 = 200km
## note: am filtering world_base to those countries appearing as unique values in cow_yr_uniques out of courtesy; but this is only out of an abundance of caution. in the preproc step, we saw that the number of countries in the spatial datasets never decreases over time, so the number of countries in world_base and cow_unique is always the same.
### note: if{} else{} code is used so I can still have imp_dml_dats_spat_cfe_gen (or even a smaller version thereof) removed from environment. only difference between the two chunks is "treat_names_reg" and "treat_names_interact."
splits_cow_all <- list()
splits_yr_all <- list()

for(stat in interact_stat){
  if(str_starts(stat, "no_")){
    for(year in start_yrs){
      for(lag in lag_names){
        for(treat in treat_names_reg){
          for(i in m){
            splits_cow <- world_base |> 
              filter(cow %in% cow_yr_uniques[[year]][[lag]]$cow) |> 
              spatial_buffer_vfold_cv(
                v = nf,
                repeats = nr+1,
                radius = NULL,
                buffer = 1000*200
                )
            
            splits_yr <- cow_yr_uniques[[year]][[lag]]$year |> 
              data.table() |> 
              setNames("year") |> 
              vfold_cv(
                v = nf,
                repeats = nr+1
                )
            
            rep_names <- splits_cow$id |> 
              unique()
            for(rep in rep_names){
              splits_cow_rep <- splits_cow |> 
                filter(id == rep)
              splits_yr_rep <- splits_yr |> 
                filter(id == rep)
              nf_seq <- 1:nrow(splits_cow_rep)
              for(fold in nf_seq){
                test_splits_cow <- splits_cow_rep$splits[[fold]] |> 
                  assessment() |> 
                  st_drop_geometry()
                train_splits_cow <- splits_cow_rep$splits[[fold]] |> 
                  analysis() |> 
                  st_drop_geometry()
                
                test_splits_yr <- splits_yr_rep$splits[[fold]] |> 
                  assessment()
                
                splits_cow_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(fold)]] <- list(test = test_splits_cow, train = train_splits_cow)
                splits_yr_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(fold)]] <- test_splits_yr
              }
            }
          }
        }
      }
    }
  }
  else{
    for(year in start_yrs){
      for(lag in lag_names){
        for(treat in treat_names_interact){
          for(i in m){
            splits_cow <- world_base |> 
              filter(cow %in% cow_yr_uniques[[year]][[lag]]$cow) |> 
              spatial_buffer_vfold_cv(
                v = nf,
                repeats = nr+1,
                radius = NULL,
                buffer = 1000*200
                )
            
            splits_yr <- cow_yr_uniques[[year]][[lag]]$year |> 
              data.table() |> 
              setNames("year") |> 
              vfold_cv(
                v = nf,
                repeats = nr+1
                )
            
            rep_names <- splits_cow$id |> 
              unique()
            for(rep in rep_names){
              splits_cow_rep <- splits_cow |> 
                filter(id == rep)
              splits_yr_rep <- splits_yr |> 
                filter(id == rep)
              nf_seq <- 1:nrow(splits_cow_rep)
              for(fold in nf_seq){
                test_splits_cow <- splits_cow_rep$splits[[fold]] |> 
                  assessment() |> 
                  st_drop_geometry()
                train_splits_cow <- splits_cow_rep$splits[[fold]] |> 
                  analysis() |> 
                  st_drop_geometry()
                
                test_splits_yr <- splits_yr_rep$splits[[fold]] |> 
                  assessment()
                
                splits_cow_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(fold)]] <- list(test = test_splits_cow, train = train_splits_cow)
                splits_yr_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(fold)]] <- test_splits_yr
              }
            }
          }
        }
      }
    }
  }
}

rm(
  list = setdiff(
    ls(),
    c(
      "interact_stat",
      "row_ids",
      "splits_cow_all",
      "splits_yr_all"
      )
    )
  )
gc()

## cross-join spatial & year splits ----
splits_join <- list()

for(stat in interact_stat){
  list_1_cow <- splits_cow_all[[stat]]
  list_1_yr <- splits_yr_all[[stat]]
  start_yrs <- names(list_1_cow)
  for(year in start_yrs){
    list_2_cow <- list_1_cow[[year]]
    list_2_yr <- list_1_yr[[year]]
    lag_names <- names(list_2_cow)
    for(lag in lag_names){
      list_3_cow <- list_2_cow[[lag]]
      list_3_yr <- list_2_yr[[lag]]
      treat_names <- names(list_3_cow)
      for(treat in treat_names){
        list_4_cow <- list_3_cow[[treat]]
        list_4_yr <- list_3_yr[[treat]]
        m <- 1:length(list_4_cow)
        for(i in m){
          list_5_cow <- list_4_cow[[i]]
          list_5_yr <- list_4_yr[[i]]
          rep_names <- names(list_5_cow)
          for(rep in rep_names){
            list_6_cow <- list_5_cow[[rep]]
            list_6_yr <- list_5_yr[[rep]]
            nf_seq <- 1:length(list_6_cow)
            for(fold_cow in nf_seq){
              list_7_cow <- list_6_cow[[fold_cow]]
              split_names <- names(list_7_cow)
              for(split in split_names){
                for(fold_yr in nf_seq){
                  splits_join[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(split)]][[paste(as.character(fold_cow), as.character(fold_yr), sep = "-")]] <- list_7_cow[[split]]$cow |> 
                    CJ(list_6_yr[[fold_yr]]$year) |> 
                    setNames(c("cow", "year"))
                }
              }
            }
          }
        }
      }
    }
  }
}

rm(
  list = setdiff(
    ls(),
    c(
      "interact_stat",
      "row_ids",
      "splits_join"
      )
    )
  )
gc()

## clean splits ----
### note: for each test fold, the training folds are already disjoint by country. we now ensure that the training folds are disjoint by year as well.
splits_join_clean <- list()

for(stat in interact_stat){
  list_1 <- splits_join[[stat]]
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
          list_5 <- list_4[[i]]
          rep_names <- names(list_5)
          for(rep in rep_names){
            list_6 <- list_5[[rep]]
            
            test_list <- list_6[[1]]
            train_list <- list_6[[2]]
            
            test_list_names <- names(test_list)
            train_list_names <- names(train_list)
            for(test_name in test_list_names){
              test_clean <- test_list[[test_name]]
              for(train_name in train_list_names){
                cow_fold_id <- str_extract(train_name, "^[^-]+")
                if(test_name == train_name){
                  train_clean <- train_list[str_starts(train_list_names, paste0(cow_fold_id, "-"))]
                  train_clean[[test_name]] <- NULL
                  train_clean <- train_clean |> 
                    rbindlist()
                  
                  splits_join_clean[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(test_name)]] <- list(test = test_clean, train = train_clean)
                }
              }
            }
          }
        }
      }
    }
  }
}

## verify cow + year disjointness ----
### note: use control+shift+c to do/undo comment block
# overlap_check <- list()
# 
# for(stat in interact_stat){
#   list_1 <- splits_join_clean[[stat]]
#   for(year in names(list_1)){
#     list_2 <- list_1[[year]]
#     for(lag in names(list_2)){
#       list_3 <- list_2[[lag]]
#       for(treat in names(list_3)){
#         list_4 <- list_3[[treat]]
#         for(i in names(list_4)){
#           list_5 <- list_4[[i]]
#           for(rep in names(list_5)){
#             list_6 <- list_5[[rep]]
#             for(split in names(list_6)){
#               
#               test  <- list_6[[split]]$test
#               train <- list_6[[split]]$train
#               
#               cow_overlap <- intersect(
#                 unique(test$cow),
#                 unique(train$cow)
#                 )
#               
#               year_overlap <- intersect(
#                 unique(test$year),
#                 unique(train$year)
#                 )
#               
#               overlap_check[[length(overlap_check) + 1]] <- tibble(
#                 stat = stat,
#                 start_year = year,
#                 lag = lag,
#                 treat = treat,
#                 i = i,
#                 rep = rep,
#                 split = split,
#                 n_test = nrow(test),
#                 n_train = nrow(train),
#                 n_cow_overlap = length(cow_overlap),
#                 n_year_overlap = length(year_overlap),
#                 cow_disjoint = length(cow_overlap) == 0,
#                 year_disjoint = length(year_overlap) == 0
#               )
#             }
#           }
#         }
#       }
#     }
#   }
# }
# 
# overlap_check <- rbindlist(overlap_check)
# overlap_check_verdict <- overlap_check |> 
#   summarize(
#     all_cow_disjoint = all(cow_disjoint),
#     all_year_disjoint = all(year_disjoint)
#   )
# 
rm(
  list = setdiff(
    ls(),
    c(
      "interact_stat",
      "splits_join_clean",
      "row_ids"#,
#      "overlap_check_verdict"
      )
    )
  )
gc()

## finalize splits ----
splits_imp_dml_dats_spat_cfe_north <- list()
train_list <- list()
test_list <- list()

for(stat in interact_stat){
  list_1 <- splits_join_clean[[stat]]
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
          list_5 <- list_4[[i]]
          rep_num <- 1:length(list_5)
          for(rep in rep_num){
            list_6 <- list_5[[rep]]
            fold_names <- names(list_6)
            for(fold in seq_along(fold_names)){
              test_id <- row_ids[[year]][[lag]][list_6[[fold]][[1]], on = .(cow, year), nomatch = 0][, row_id]
              train_id <- row_ids[[year]][[lag]][list_6[[fold]][[2]], on = .(cow, year), nomatch = 0][, row_id]
              
              test_list[[fold]] <- test_id
              train_list[[fold]] <- train_id
              
              splits_imp_dml_dats_spat_cfe_north[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[rep]] <- list(train_ids = train_list, test_ids = test_list)
            }
          }
        }
      }
    }
  }
}

rm(list = setdiff(ls(), "splits_imp_dml_dats_spat_cfe_north"))
gc()

# save splits ----
splits_imp_dml_dats_spat_cfe_north |> 
  save(file = here("data/ch1/results/splits/spat_cfe/splits_imp_dml_dats_spat_cfe_north.rda"))
