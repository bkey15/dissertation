# Estimate stacked coefficient(s), SE(s), etc.
## note: for base code, see: https://rdrr.io/cran/DoubleML/src/R/double_ml.R

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)

# run prep script ----
source(here("scripts/ch1/4_prep_model/ii_slx/4b(ii)_prep_model_dml_slx_south.R"))

# load splits ----
load(here("data/ch1/results/splits/bcr_slx/splits_imp_dml_dats_bcr_slx_south.rda"))

# load metadata ----
load(here("data/ch1/results/fits/dml_stack/metadata/imp_dml_fits_bcr_slx_south_stack_metadata.rda"))

# get var scaling factors ----
## note: storing these separately for use in selection of final SE
interact_stat <- names(imp_dml_dats_slx_south)
cluster_cols <- c("cow", "year")
var_scaling_factor <- list()

for(stat in interact_stat){
  list_1 <- imp_dml_dats_slx_south[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
        list_4 <- list_3[[treat]]
        m <- seq_len(length(list_4))
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          dat <- list_4[[i]]$data |> 
            select(all_of(cluster_cols))
          
          n_first_clusters <- length(unique(dat[[1]]))
          n_second_clusters <- length(unique(dat[[2]]))
          var_scaling_factor[[as.character(stat)]][[as.character(year)]][[as.character(lag)]] <- min(n_first_clusters, n_second_clusters)
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
      "cluster_cols",
      "var_scaling_factor",
      "imp_dml_dats_slx_south",
      "splits_imp_dml_dats_bcr_slx_south",
      "imp_dml_fits_bcr_slx_south_stack_metadata"
      )
    )
  )
gc()

# get estimates ----
## coef ----
### note: also collating psi, psi_a, & psi_b
estimates <- list()
imp_dml_fits_bcr_slx_south_stack <- list()

for(stat in interact_stat){
  list_1_a <- imp_dml_fits_bcr_slx_south_stack_metadata[[stat]]
  list_1_b <- splits_imp_dml_dats_bcr_slx_south[["smpls"]][[stat]]
  list_1_c <- splits_imp_dml_dats_bcr_slx_south[["smpls_cluster"]][[stat]]
  start_yrs <- names(list_1_a)
  for(year in start_yrs){
    list_2_a <- list_1_a[[year]]
    list_2_b <- list_1_b[[year]]
    list_2_c <- list_1_c[[year]]
    lag_names <- names(list_2_a)
    for(lag in lag_names){
      list_3_a <- list_2_a[[lag]]
      list_3_b <- list_2_b[[lag]]
      list_3_c <- list_2_c[[lag]]
      treat_names <- names(list_3_a)
      for(treat in treat_names){
        list_4_a <- list_3_a[[treat]]
        list_4_b <- list_3_b[[treat]]
        list_4_c <- list_3_c[[treat]]
        m <- seq_len(length(list_4_a))
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          list_5_a <- list_4_a[[i]]
          list_5_b <- list_4_b[[i]][-1]
          list_5_c <- list_4_c[[i]][-1]
          n_rep <- seq_len(length(list_5_a))
          for(rep in n_rep){
            smpls <- list_5_b[[rep]]
            smpls_cluster <- list_5_c[[rep]]
            n_folds <- seq_len(length(smpls[["test_ids"]]))
            if(length(split_names) == 1){
              list_6_a <- list_5_a[["psi"]][[rep]]
              psi_a <- list_6_a[["psi_a"]]
              psi_b <- list_6_a[["psi_b"]]
              psi_a_subsample_mean <- 0
              psi_b_subsample_mean <- 0
              for(i_fold in n_folds){
                test_index <- smpls[["test_ids"]][[i_fold]]
                test_cluster_inds <- smpls_cluster[["test_ids"]][[i_fold]]
                
                xx <- sapply(
                  test_cluster_inds,
                  function(x) length(x)
                  )
                scaling_factor <- 1 / prod(xx)
                psi_a_subsample_mean <- psi_a_subsample_mean +
                  scaling_factor * sum(psi_a[test_index])
                psi_b_subsample_mean <- psi_b_subsample_mean +
                  scaling_factor * sum(psi_b[test_index])
              }
              theta <- -psi_b_subsample_mean / psi_a_subsample_mean
              psi <- psi_a * theta + psi_b
              
              estimates[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]] <- list(coef = theta, psi = psi, psi_a = psi_a, psi_b = psi_b)
            }
            else{
              list_6_a <- list_5_a[["psi"]]
              d_all <- list_6_a |> 
                names()
              for(d_name in d_all){
                psi_a <- list_6_a[[d_name]][[rep]][["psi_a"]]
                psi_b <- list_6_a[[d_name]][[rep]][["psi_b"]]
                psi_a_subsample_mean <- 0
                psi_b_subsample_mean <- 0
                for(i_fold in n_folds){
                  test_index <- smpls[["test_ids"]][[i_fold]]
                  test_cluster_inds <- smpls_cluster[["test_ids"]][[i_fold]]
                  
                  xx <- sapply(
                    test_cluster_inds,
                    function(x) length(x)
                    )
                  scaling_factor <- 1 / prod(xx)
                  psi_a_subsample_mean <- psi_a_subsample_mean +
                    scaling_factor * sum(psi_a[test_index])
                  psi_b_subsample_mean <- psi_b_subsample_mean +
                    scaling_factor * sum(psi_b[test_index])
                }
                theta <- -psi_b_subsample_mean / psi_a_subsample_mean
                psi <- psi_a * theta + psi_b
                
                estimates[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(d_name)]] <- list(coef = theta, psi = psi, psi_a = psi_a, psi_b = psi_b)
              }
            }
          }
          if(length(split_names) == 1){
            all_coef <- t(unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x$coef)))
            coef <- apply(all_coef, 1, function(x) median(x, na.rm = TRUE))
            names(coef) <- treat
            
            psi_length <- estimates[[stat]][[year]][[lag]][[treat]][[i]][[1]]$psi |> 
              length()
            rep_length <- estimates[[stat]][[year]][[lag]][[treat]][[i]] |> 
              length()
            
            all_psi <- array(
              unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x$psi)), dim = c(psi_length, rep_length, 1)
              )
            all_psi_a <- array(
              unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x$psi_a)), dim = c(psi_length, rep_length, 1)
              )
            all_psi_b <- array(
              unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x$psi_b)), dim = c(psi_length, rep_length, 1)
              )
            
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- list(all_coef  = all_coef, coef = coef, psi = all_psi, psi_a = all_psi_a, psi_b = all_psi_b)
          }
          else{
            coef_psi_list <- list()
            for(d_name in d_all){
              all_coef <- t(unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x[[d_name]]$coef)))
              coef <- apply(all_coef, 1, function(x) median(x, na.rm = TRUE))
              names(coef) <- d_name
              
              psi_length <- estimates[[stat]][[year]][[lag]][[treat]][[i]][[1]][[d_name]]$psi |> 
                length()
              rep_length <- estimates[[stat]][[year]][[lag]][[treat]][[i]] |> 
                length()
              
              all_psi <- array(
                unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x[[d_name]]$psi)), dim = c(psi_length, rep_length, 1)
                )
              all_psi_a <- array(
                unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x[[d_name]]$psi_a)), dim = c(psi_length, rep_length, 1)
                )
              all_psi_b <- array(
                unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x[[d_name]]$psi_b)), dim = c(psi_length, rep_length, 1)
                )
              
              coef_psi_list[[as.character(d_name)]] <- list(all_coef = all_coef, coef = coef, all_psi = all_psi, all_psi_a = all_psi_a, all_psi_b = all_psi_b)
            }
            all_coef <- t(unname(sapply(coef_psi_list, function(x) rbind(x$all_coef))))
            coef <- sapply(coef_psi_list, function(x) rbind(x$coef))
            
            d_length <- length(d_all)
            
            all_psi <- array(
              unname(sapply(coef_psi_list, function(x) x$all_psi)), dim = c(psi_length, rep_length, d_length)
              )
            all_psi_a <- array(
              unname(sapply(coef_psi_list, function(x) x$all_psi_a)), dim = c(psi_length, rep_length, d_length)
              )
            all_psi_b <- array(
              unname(sapply(coef_psi_list, function(x) x$all_psi_b)), dim = c(psi_length, rep_length, d_length)
              )
            
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- list(all_coef  = all_coef, coef = coef, psi = all_psi, psi_a = all_psi_a, psi_b = all_psi_b)
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
      "cluster_cols",
      "var_scaling_factor",
      "imp_dml_dats_slx_south",
      "splits_imp_dml_dats_bcr_slx_south",
      "imp_dml_fits_bcr_slx_south_stack_metadata",
      "estimates",
      "imp_dml_fits_bcr_slx_south_stack"
    )
  )
)
gc()

## se ----
for(stat in interact_stat){
  list_1_a <- imp_dml_fits_bcr_slx_south_stack[[stat]]
  list_1_b <- splits_imp_dml_dats_bcr_slx_south[["smpls"]][[stat]]
  list_1_c <- splits_imp_dml_dats_bcr_slx_south[["smpls_cluster"]][[stat]]
  list_1_d <- imp_dml_dats_slx_south[[stat]]
  list_1_e <- var_scaling_factor[[stat]]
  start_yrs <- names(list_1_a)
  for(year in start_yrs){
    list_2_a <- list_1_a[[year]]
    list_2_b <- list_1_b[[year]]
    list_2_c <- list_1_c[[year]]
    list_2_d <- list_1_d[[year]]
    list_2_e <- list_1_e[[year]]
    lag_names <- names(list_2_a)
    for(lag in lag_names){
      list_3_a <- list_2_a[[lag]]
      list_3_b <- list_2_b[[lag]]
      list_3_c <- list_2_c[[lag]]
      list_3_d <- list_2_d[[lag]]
      lag_var_scaling_factor <- list_2_e[[lag]]
      treat_names <- names(list_3_a)
      for(treat in treat_names){
        list_4_a <- list_3_a[[treat]]
        list_4_b <- list_3_b[[treat]]
        list_4_c <- list_3_c[[treat]]
        list_4_d <- list_3_d[[treat]]
        m <- seq_len(length(list_4_a))
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          list_5_a <- list_4_a[[i]]
          list_5_b <- list_4_b[[i]][-1]
          list_5_c <- list_4_c[[i]][-1]
          dat <- list_4_d[[i]]$data |> 
            select(all_of(cluster_cols))
          n_rep <- seq_len(length(list_5_b))
          for(rep in n_rep){
            smpls <- list_5_b[[rep]]
            smpls_cluster <- list_5_c[[rep]]
            n_folds <- seq_len(length(smpls[["test_ids"]]))
            if(length(split_names) == 1){
              psi <- list_5_a[["psi"]][,,1][,rep]
              psi_a <- list_5_a[["psi_a"]][,,1][,rep]
              
              first_cluster_var <- dat[[cluster_cols[1]]]
              second_cluster_var <- dat[[cluster_cols[2]]]
              gamma_hat <- 0
              j_hat <- 0
              for(i_fold in n_folds){
                test_inds <- smpls[["test_ids"]][[i_fold]]
                test_cluster_inds <-  smpls_cluster[["test_ids"]][[i_fold]]
                I_k <- test_cluster_inds[[1]]
                J_l <- test_cluster_inds[[2]]
                const <- min(length(I_k), length(J_l)) / ((length(I_k) * length(J_l))^2)
                for(cluster_value in I_k){
                  ind_cluster <- (first_cluster_var == cluster_value) &
                    second_cluster_var %in% J_l
                  gamma_hat <- gamma_hat + const * sum(
                    outer(
                      psi[ind_cluster],
                      psi[ind_cluster]
                      )
                    )
                }
                for(cluster_value in J_l){
                  ind_cluster <- (second_cluster_var == cluster_value) & 
                    first_cluster_var %in% I_k
                  gamma_hat <- gamma_hat + const * sum(
                    outer(
                      psi[ind_cluster],
                      psi[ind_cluster]
                      )
                    )
                }
                j_hat <- j_hat + sum(psi_a[test_inds]) / (length(I_k) * length(J_l))
              }
              gamma_hat <- gamma_hat / length(n_folds)
              j_hat <- j_hat / length(n_folds)
              sigma2_hat <- gamma_hat / (j_hat^2) / lag_var_scaling_factor
              estimates[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]]$se <- sqrt(sigma2_hat)
            }
            else{
              d_all <- list_5_a[["coef"]] |> 
                names()
              for(d_name in d_all){
                d_pos <- match(d_name, d_all)
                
                psi <- list_5_a[["psi"]][,,d_pos][,rep]
                psi_a <- list_5_a[["psi_a"]][,,d_pos][,rep]
                
                first_cluster_var <- dat[[cluster_cols[1]]]
                second_cluster_var <- dat[[cluster_cols[2]]]
                gamma_hat <- 0
                j_hat <- 0
                for(i_fold in n_folds){
                  test_inds <- smpls[["test_ids"]][[i_fold]]
                  test_cluster_inds <-  smpls_cluster[["test_ids"]][[i_fold]]
                  I_k <- test_cluster_inds[[1]]
                  J_l <- test_cluster_inds[[2]]
                  const <- min(length(I_k), length(J_l)) / ((length(I_k) * length(J_l))^2)
                  for(cluster_value in I_k){
                    ind_cluster <- (first_cluster_var == cluster_value) &
                      second_cluster_var %in% J_l
                    gamma_hat <- gamma_hat + const * sum(
                      outer(
                        psi[ind_cluster],
                        psi[ind_cluster]
                        )
                      )
                  }
                  for(cluster_value in J_l){
                    ind_cluster <- (second_cluster_var == cluster_value) & 
                      first_cluster_var %in% I_k
                    gamma_hat <- gamma_hat + const * sum(
                      outer(
                        psi[ind_cluster],
                        psi[ind_cluster]
                        )
                      )
                  }
                  j_hat <- j_hat + sum(psi_a[test_inds]) / (length(I_k) * length(J_l))
                }
                gamma_hat <- gamma_hat / length(n_folds)
                j_hat <- j_hat / length(n_folds)
                sigma2_hat <- gamma_hat / (j_hat^2) / lag_var_scaling_factor
                estimates[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(d_name)]]$se <- sqrt(sigma2_hat)
              }
            }
          }
          if(length(split_names) == 1){
            all_se <- t(unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x$se)))
            all_coef <- imp_dml_fits_bcr_slx_south_stack[[stat]][[year]][[lag]][[treat]][[i]][["all_coef"]]
            coef <- imp_dml_fits_bcr_slx_south_stack[[stat]][[year]][[lag]][[treat]][[i]][["coef"]]
            se <- sqrt(apply(
              lag_var_scaling_factor * all_se^2 + (all_coef - coef)^2,
              1, function(x) median(x, na.rm = TRUE)) / lag_var_scaling_factor)
            names(se) <- treat
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$all_se <- all_se
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$se <- se
          }
          else{
            se_list <- list()
            for(d_name in d_all){
              d_pos <- match(d_name, d_all)
              
              all_se <- t(unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x[[d_name]]$se)))
              all_coef <- imp_dml_fits_bcr_slx_south_stack[[stat]][[year]][[lag]][[treat]][[i]][["all_coef"]][d_pos,]
              coef <- imp_dml_fits_bcr_slx_south_stack[[stat]][[year]][[lag]][[treat]][[i]][["coef"]][[d_name]]
              se <- sqrt(apply(
                lag_var_scaling_factor * all_se^2 + (all_coef - coef)^2,
                1, function(x) median(x, na.rm = TRUE)) / lag_var_scaling_factor)
              names(se) <- d_name
              se_list[[as.character(d_name)]] <- list(all_se = all_se, se = se)
            }
            all_se <- t(unname(sapply(se_list, function(x) rbind(x$all_se))))
            se <- sapply(se_list, function(x) rbind(x$se))
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$all_se <- all_se
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$se <- se
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
      "estimates",
      "imp_dml_fits_bcr_slx_south_stack"
    )
  )
)
gc()

# compute t-stat, p-val, conf int, summary ----
format_perc <- function(probs, digits){
  paste(
    format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
    "%"
    )
}
level <- 0.95

make_summary <- function(coef, se, t_stat, pval,
                         digits = max(3L, getOption("digits") - 3L)) {
  
  if (all(is.na(coef))) {
    message("fit() not yet called.")
    return(NULL)
  }
  
  k <- length(coef)
  
  table <- matrix(NA_real_, ncol = 4, nrow = k)
  
  rownames(table) <- names(coef)
  
  colnames(table) <- c(
    "Estimate.", "Std. Error", "t value", "Pr(>|t|)"
    )
  
  table[, 1] <- coef
  table[, 2] <- se
  table[, 3] <- t_stat
  table[, 4] <- pval
  
  invisible(table)
}

for(stat in interact_stat){
  list_1 <- imp_dml_fits_bcr_slx_south_stack[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1[[year]]
    lag_names <- names(list_2)
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
        list_4 <- list_3[[treat]]
        m <- seq_len(length(list_4))
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          list_5 <- list_4[[i]]
          if(length(split_names) == 1){
            coef <- list_5[["coef"]]
            se <- list_5[["se"]]
            t_stat <- coef / se
            pval <- 2 * pnorm(-abs(t_stat))
            a <- (1 - level) / 2
            a <- c(a, 1 - a)
            pct <- format_perc(a, 3)
            fac <- qnorm(a)
            ci <- array(
              NA_real_,
              dim = 2L,
              dimnames = list(pct)
              )
            ci[] <- coef + se %o% fac
            
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$t_stat <- t_stat
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$pval <- pval
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$ci <- ci
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$summary <- make_summary(coef, se, t_stat, pval)
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][order(names(imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]))]
          }
          else{
            stat_list <- list()
            for(d_name in split_names){
              coef_d <- list_5[["coef"]][d_name]
              se_d <- list_5[["se"]][d_name]
              t_stat_d <- coef_d / se_d
              pval_d <- 2 * pnorm(-abs(t_stat_d))
              a <- (1 - level) / 2
              a <- c(a, 1 - a)
              pct <- format_perc(a, 3)
              fac <- qnorm(a)
              ci_d <- array(
                NA_real_,
                dim = c(length(coef_d), 2L),
                dimnames = list(names(coef_d), pct)
                )
              ci_d[] <- coef_d + se_d %o% fac
              
              stat_list[[as.character(d_name)]] <- list(t_stat_d = t_stat_d, pval_d = pval_d, ci_d = ci_d)
            }
            coef <- list_5[["coef"]]
            se <- list_5[["se"]]
            t_stat <- sapply(stat_list, function(x) rbind(x$t_stat_d))
            pval <- sapply(stat_list, function(x) rbind(x$pval_d))
            ci <- do.call(rbind, lapply(stat_list, function(x) x$ci_d))
            
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$t_stat <- t_stat
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$pval <- pval
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$ci <- ci
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$summary <- make_summary(coef, se, t_stat, pval)
            imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][order(names(imp_dml_fits_bcr_slx_south_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]))]
          }
        }
      }
    }
  }
}

# save estimates ----
imp_dml_fits_bcr_slx_south_stack |> 
  save(file = here("data/ch1/results/fits/dml_stack/final/imp_dml_fits_bcr_slx_south_stack.rda"))
