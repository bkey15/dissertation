# Get stack metadata

# load packages ----
library(tidyverse)
library(here)

# get file names ----
fit_files_regz <- dir("data/ch3/results/fits/dml_regz/full_dat", full.names = TRUE)
fit_files_tree <- dir("data/ch3/results/fits/dml_tree/full_dat", full.names = TRUE)
fit_files <- c(fit_files_regz, fit_files_tree) |> 
  str_subset("(?=.*bcr)(?=.*regr)")

# stack results ----
## load data & get resids ----
### note: loading fits separately to minimize memory usage; loading all at once can take very long or cause R to crash
resids_all <- list()

for(file in fit_files){
  load(here(file))
  fit <- ls() |> 
    str_subset("^imp_dml_fits") |> 
    get()
  fit_name <- ls() |> 
    str_subset("^imp_dml_fits") |> 
    str_extract("[^_]+$")
  interact_stat <- names(fit)
  for(stat in interact_stat){
    list_1 <- fit[[stat]]
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
          preds <- list_4[[i]]$predictions
          nuis_part <- names(preds)
          d_all <- list_4[[i]]$data$d_cols
          y_name <- list_4[[i]]$data$y_col
          y <- list_4[[i]]$data$data_model[[y_name]]
          n_rep <- 1:list_4[[i]]$n_rep
          for(part in nuis_part){
            for(d_name in d_all){
              d <- list_4[[i]]$data$data_model[[d_name]]
              d_pos <- match(d_name, d_all)
              for(rep in n_rep){
                pred <- preds[[part]][,,d_pos][,rep]
                if(part == "ml_l"){
                  if(length(d_all) == 1){
                    resids_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(part)]][[as.character(rep)]][[as.character(fit_name)]] <- y - pred
                  }
                  else{
                    resids_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(part)]][[as.character(d_name)]][[as.character(rep)]][[as.character(fit_name)]] <- y - pred
                    }
                  }
                else{
                  if(length(d_all) == 1){
                    resids_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(part)]][[as.character(rep)]][[as.character(fit_name)]] <- d - pred
                  }
                  else{
                    resids_all[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(part)]][[as.character(d_name)]][[as.character(rep)]][[as.character(fit_name)]] <- d - pred
                    }
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
      c("fit_files", "resids_all")
      )
    )
  gc()
}

## get weights, stacked resids, and mse ----
interact_stat <- names(resids_all)
imp_dml_fits_bcr_slx_regr_stack_metadata <- list()

for(stat in interact_stat){
  list_1 <- resids_all[[stat]]
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
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          list_5 <- list_4[[i]]
          nuis_part <- names(list_5)
          for(part in nuis_part){
            list_6 <- list_5[[part]]
            if(length(split_names) == 1){
              n_rep <- names(list_6)
              for(rep in n_rep){
                cv_resid <- list_6[[rep]] |> 
                  bind_cols() |> 
                  as.matrix()
                nlearners <- ncol(cv_resid)
                
                sq_resid <- Matrix::crossprod(cv_resid)
                A <- cbind(matrix(1, nlearners, 1), diag(1, nlearners))

                fallback_used <- FALSE
                weights <- tryCatch(
                  {
                    quadprog::solve.QP(
                      Dmat = Matrix::nearPD(sq_resid)$mat,
                      dvec = matrix(0, nlearners, 1),
                      Amat = A,
                      bvec = c(1, rep(0, nlearners)),
                      meq = 1
                      )$solution
                  },
                  error = function(e) {
                    
                    fallback_used <<- TRUE
                    
                    Dmat <- (sq_resid + t(sq_resid)) / 2
                    Dmat <- Dmat / max(abs(Dmat))
                    
                    quadprog::solve.QP(
                      Dmat = Dmat,
                      dvec = matrix(0, nlearners, 1),
                      Amat = A,
                      bvec = c(1, rep(0, nlearners)),
                      meq = 1
                      )$solution
                  }
                )
                
                mse <- list()
                names(weights) <- names(list_6[[rep]])
                stack_resid <- drop(cv_resid %*% weights)
                stack <- mean(stack_resid^2)
                mse[["stack"]] <- stack
                
                cv_resid_tbl <- cv_resid |> 
                  as_tibble()
                lrn_names <- names(cv_resid_tbl)
                
                for(lrn in lrn_names){
                  resid_vec <- cv_resid_tbl[lrn] |> 
                    pull()
                  lrn_mse <- mean(resid_vec^2)
                  mse[[as.character(lrn)]] <- lrn_mse
                }
                
                imp_dml_fits_bcr_slx_regr_stack_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(part)]][[as.character(rep)]] <- list(weights = weights, stack_resid = stack_resid, mse = mse, fallback_used = fallback_used)
              }
            }
            else{
              d_all <- names(list_6)
              for(d_name in d_all){
                list_7 <- list_6[[d_name]]
                n_rep <- names(list_7)
                for(rep in n_rep){
                  cv_resid <- list_7[[rep]] |> 
                    bind_cols() |> 
                    as.matrix()
                  nlearners <- ncol(cv_resid)
                  
                  sq_resid <- Matrix::crossprod(cv_resid)
                  A <- cbind(matrix(1, nlearners, 1), diag(1, nlearners))

                  fallback_used <- FALSE
                  weights <- tryCatch(
                    {
                      quadprog::solve.QP(
                        Dmat = Matrix::nearPD(sq_resid)$mat,
                        dvec = matrix(0, nlearners, 1),
                        Amat = A,
                        bvec = c(1, rep(0, nlearners)),
                        meq = 1
                        )$solution
                    },
                    error = function(e) {
                      
                      fallback_used <<- TRUE
                      
                      Dmat <- (sq_resid + t(sq_resid)) / 2
                      Dmat <- Dmat / max(abs(Dmat))
                      
                      quadprog::solve.QP(
                        Dmat = Dmat,
                        dvec = matrix(0, nlearners, 1),
                        Amat = A,
                        bvec = c(1, rep(0, nlearners)),
                        meq = 1
                        )$solution
                    }
                  )
                  
                  mse <- list()
                  names(weights) <- names(list_7[[rep]])
                  stack_resid <- drop(cv_resid %*% weights)
                  stack <- mean(stack_resid^2)
                  mse[["stack"]] <- stack
                  
                  cv_resid_tbl <- cv_resid |> 
                    as_tibble()
                  lrn_names <- names(cv_resid_tbl)
                  
                  for(lrn in lrn_names){
                    resid_vec <- cv_resid_tbl[lrn] |> 
                      pull()
                    lrn_mse <- mean(resid_vec^2)
                    mse[[as.character(lrn)]] <- lrn_mse
                    }
                  
                  imp_dml_fits_bcr_slx_regr_stack_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(part)]][[as.character(d_name)]][[as.character(rep)]] <- list(weights = weights, stack_resid = stack_resid, mse = mse, fallback_used = fallback_used)
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
      "imp_dml_fits_bcr_slx_regr_stack_metadata"
      )
    )
  )
gc()

# check fallbacks ----
count_fallback_fields <- function(x) {
  if (!is.list(x)) {
    return(0L)
  }
  
  total <- if ("fallback_used" %in% names(x)) 1L else 0L
  
  if (length(x) > 0) {
    total <- total + sum(vapply(x, count_fallback_fields, integer(1)))
  }
  
  total
}

count_fallbacks <- function(x) {
  if (!is.list(x)) return(0)
  
  total <- 0
  
  if ("fallback_used" %in% names(x)) {
    total <- total + as.numeric(isTRUE(x[["fallback_used"]]))
  }
  
  if (length(x) > 0) {
    total <- total + sum(vapply(x, count_fallbacks, numeric(1)))
  }
  
  total
}

n_total <- count_fallback_fields(imp_dml_fits_bcr_slx_regr_stack_metadata)

n_fallback <- count_fallbacks(imp_dml_fits_bcr_slx_regr_stack_metadata)

cat(
  n_fallback, "of", n_total,
  "stacking optimizations used the fallback (",
  round(100 * n_fallback / n_total, 2),
  "%).\n"
  )

# compute psi_a and psi_b ----
for(stat in interact_stat){
  list_1 <- imp_dml_fits_bcr_slx_regr_stack_metadata[[stat]]
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
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          list_5_a <- list_4[[i]][["ml_l"]]
          list_5_b <- list_4[[i]][["ml_m"]]
          if(length(split_names) == 1){
            n_rep <- names(list_5_a)
            for(rep in n_rep){
              ml_l_resid <- list_5_a[[rep]]$stack_resid
              ml_m_resid <- list_5_b[[rep]]$stack_resid
              
              psi_a <- -(ml_m_resid^2)
              psi_b <- ml_l_resid*ml_m_resid
              
              imp_dml_fits_bcr_slx_regr_stack_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][["psi"]][[as.character(rep)]] <- list(psi_a = psi_a, psi_b = psi_b)
            }
          }
          else{
            d_all <- names(list_5_a)
            for(d_name in d_all){
              list_6_a <- list_5_a[[d_name]]
              list_6_b <- list_5_b[[d_name]]
              n_rep <- names(list_6_a)
              for(rep in n_rep){
                ml_l_resid <- list_6_a[[rep]]$stack_resid
                ml_m_resid <- list_6_b[[rep]]$stack_resid
                
                psi_a <- -(ml_m_resid^2)
                psi_b <- ml_l_resid*ml_m_resid
                
                imp_dml_fits_bcr_slx_regr_stack_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][["psi"]][[as.character(d_name)]][[as.character(rep)]] <- list(psi_a = psi_a, psi_b = psi_b)
              }
            }
          }
        }
      }
    }
  }
}

# save metadata ----
imp_dml_fits_bcr_slx_regr_stack_metadata |> 
  save(file = here("data/ch3/results/fits/dml_stack/metadata/imp_dml_fits_bcr_slx_regr_stack_metadata.rda"))
