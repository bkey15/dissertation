# Estimate stacked coefficient(s) and SE(s)
## note: for base code, see: https://rdrr.io/cran/DoubleML/src/R/double_ml.R

# load packages ----
library(tidyverse)
library(here)
library(DoubleML)

# run prep script ----
# source(here("scripts/ch1/4_prep_model/a_regz/ii_slx/4a(iv)_prep_regz_model_dml_slx_gen.R"))

# load splits ----
# load(here("data/ch1/results/splits/bcr_slx/splits_imp_dml_dats_bcr_slx_gen.rda"))

# load metadata ----
# load(here("data/ch1/results/fits/dml_stack/metadata/imp_dml_fits_bcr_slx_gen_stack_metadata.rda"))

# get test cluster metadata & se scaling factors ----
# interact_stat <- names(imp_dml_dats_slx_gen)
interact_stat <- names(fit)
cluster_vars <- c("cow", "year")
test_cluster_metadata <- list()
var_scaling_factor <- list()

for(stat in interact_stat){
  list_1_a <- fit[[stat]]
  # list_1_b <- splits_imp_dml_dats_bcr_slx_gen[[stat]]
  list_1_c <- imp_dml_fits_bcr_slx_gen_stack_metadata[[stat]]
  start_yrs <- names(list_1_a)
  for(year in start_yrs){
    list_2_a <- list_1_a[[year]]
    # list_2_b <- list_1_b[[year]]
    list_2_c <- list_1_c[[year]]
    lag_names <- names(list_2_a)
    for(lag in lag_names){
      list_3_a <- list_2_a[[lag]]
      # list_3_b <- list_2_b[[lag]]
      list_3_c <- list_2_c[[lag]]
      treat_names <- names(list_3_a)
      for(treat in treat_names){
        list_4_a <- list_3_a[[treat]]
        # list_4_b <- list_3_b[[treat]]
        list_4_c <- list_3_c[[treat]]
        m <- seq_len(length(list_4_a))
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          # dat <- list_4_a[[i]]$data |> 
          #   select(all_of(cluster_vars))
          dat <- list_4_a[[i]]$data$data |> 
            select(all_of(cluster_vars))
          # smpls <- list_4_b[[i]][-1]
          smpls <- list_4_a[[i]]$smpls
          psi_list <- list_4_c[[i]]$psi
          n_rep <- seq_len(length(smpls))
          
          n_first_clusters <- length(unique(dat[[1]]))
          n_second_clusters <- length(unique(dat[[2]]))
          var_scaling_factor[[as.character(stat)]][[as.character(year)]][[as.character(lag)]] <- min(n_first_clusters, n_second_clusters)
          for(rep in n_rep){
            test_ids <- smpls[[rep]]$test_ids
            for(i_fold in seq_len(length(test_ids))){
              test_index <- test_ids[[i_fold]]
              cluster_cols <- dat[test_index, ]
              if(length(split_names) == 1){
                psi_a <- psi_list[[rep]]$psi_a[test_index]
                psi_b <- psi_list[[rep]]$psi_b[test_index]
                
                test_cluster_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(i_fold)]] <- list(psi_a = psi_a, psi_b = psi_b, cluster_cols = cluster_cols)
                for(cluster in cluster_vars){
                  test_cluster_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(i_fold)]][["cluster_inds"]][[as.character(cluster)]] <- cluster_cols[[cluster]] |>
                    unique()
                }
              }
              else{
                d_all <- names(psi_list)
                for(d_name in d_all){
                  psi_a <- psi_list[[d_name]][[rep]]$psi_a[test_index]
                  psi_b <- psi_list[[d_name]][[rep]]$psi_b[test_index]
                  
                  test_cluster_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(i_fold)]][[as.character(d_name)]] <- list(psi_a = psi_a, psi_b = psi_b, cluster_cols = cluster_cols)
                  for(cluster in cluster_vars){
                    test_cluster_metadata[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(i_fold)]][["cluster_inds"]][[as.character(cluster)]] <- cluster_cols[[cluster]] |>
                      unique()
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
    c(
      "fit",
      "interact_stat",
      "var_scaling_factor",
      "test_cluster_metadata"
      )
    )
  )
gc()

# get estimates ----
## coef ----
### IMPORTANT: in order to demonstrate virtual reproducibility, code below technically computes the scaling factor from DML's intended cluster-fold assignments. As such, countries like South Sudan (cow == 626) may be "assigned" to a certain fold but not actually appear if it didn't exist yet. I.e., the denominator of DML's per-fold scaling factor my be slightly larger than those computed fully manually (i.e., from counting the clusters actually appearing in the fold). (In this example, the scaling factors differ only by South Sudan for 9 folds, meaning the scaling factor denominator also differs by 1 for these folds.)
estimates <- list()
imp_dml_fits_bcr_slx_gen_stack <- list()

for(stat in interact_stat){
  list_1 <- test_cluster_metadata[[stat]]
  list_1_b <- fit[[stat]]
  start_yrs <- names(list_1)
  for(year in start_yrs){
    list_2 <- list_1 [[year]]
    list_2_b <- list_1_b[[year]]
    lag_names <- names(list_2)
    for(lag in lag_names){
      list_3 <- list_2[[lag]]
      list_3_b <- list_2_b[[lag]]
      treat_names <- names(list_3)
      for(treat in treat_names){
        list_4 <- list_3[[treat]]
        list_4_b <- list_3_b[[treat]]
        m <- seq_len(length(list_4))
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          list_5 <- list_4[[i]]
          list_5_b <- list_4_b[[i]]
          n_rep <- seq_len(length(list_5))
          for(rep in n_rep){
            list_6 <- list_5[[rep]]
            list_6_b <- list_5_b$smpls_cluster[[rep]]
            n_folds <- seq_len(length(list_6))
            if(length(split_names) == 1){
              psi <- list()
              scaling_factors <- list()
              psi_a_subsample_mean <- 0
              psi_b_subsample_mean <- 0
              for(i_fold in n_folds){
                psi_a <- list_6[[i_fold]]$psi_a
                psi_b <- list_6[[i_fold]]$psi_b
                
                # xx <- sapply(
                #   list_6[[i_fold]]$cluster_inds,
                #   function(x) length(x)
                #   )
                xx <- sapply(
                  list_6_b$test_ids[[i_fold]],
                  function(x) length(x)
                  )
                scaling_factor <- 1 / prod(xx)
                psi_a_subsample_mean <- psi_a_subsample_mean +
                  scaling_factor * sum(psi_a)
                psi_b_subsample_mean <- psi_b_subsample_mean +
                  scaling_factor * sum(psi_b)
                
                scaling_factors[[i_fold]] <- scaling_factor
                }
              theta <- -psi_b_subsample_mean / psi_a_subsample_mean
              for(i_fold in n_folds){
                psi_a <- list_6[[i_fold]]$psi_a
                psi_b <- list_6[[i_fold]]$psi_b
                
                psi[[i_fold]] <- psi_a * theta + psi_b
              }
              estimates[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]] <- list(coef = theta, psi = psi, scaling_factors = scaling_factors)
            }
            else{
              d_all <- list_6[[1]] |> 
                names() |> 
                str_subset(
                  "cluster",
                  negate = TRUE
                  )
              for(d_name in d_all){
                psi <- list()
                scaling_factors <- list()
                psi_a_subsample_mean <- 0
                psi_b_subsample_mean <- 0
                for(i_fold in n_folds){
                  psi_a <- list_6[[i_fold]][[d_name]]$psi_a
                  psi_b <- list_6[[i_fold]][[d_name]]$psi_b
                  
                  # xx <- sapply(
                  #   list_6[[i_fold]]$cluster_inds,
                  #   function(x) length(x)
                  #   )
                  xx <- sapply(
                    list_6_b$test_ids[[i_fold]],
                    function(x) length(x)
                  )
                  scaling_factor <- 1 / prod(xx)
                  psi_a_subsample_mean <- psi_a_subsample_mean +
                    scaling_factor * sum(psi_a)
                  psi_b_subsample_mean <- psi_b_subsample_mean +
                    scaling_factor * sum(psi_b)
                  
                  scaling_factors[[i_fold]] <- scaling_factor
                  }
                theta <- -psi_b_subsample_mean / psi_a_subsample_mean
                for(i_fold in n_folds){
                  psi_a <- list_6[[i_fold]][[d_name]]$psi_a
                  psi_b <- list_6[[i_fold]][[d_name]]$psi_b
                  
                  psi[[i_fold]] <- psi_a * theta + psi_b
                }
                estimates[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]][[as.character(d_name)]] <- list(coef = theta, psi = psi)
              }
            }
          }
          if(length(split_names) == 1){
            all_coef <- t(unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x$coef)))
            coef <- apply(all_coef, 1, function(x) median(x, na.rm = TRUE))
            names(coef) <- treat
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- list(all_coef  = all_coef, coef = coef)
          }
          else{
            coef_list <- list()
            for(d_name in d_all){
              all_coef <- t(unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x[[d_name]]$coef)))
              coef <- apply(all_coef, 1, function(x) median(x, na.rm = TRUE))
              names(coef) <- d_name
              coef_list[[as.character(d_name)]] <- list(all_coef = all_coef, coef = coef)
            }
            all_coef <- t(unname(sapply(coef_list, function(x) rbind(x$all_coef))))
            coef <- sapply(coef_list, function(x) rbind(x$coef))
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- list(all_coef  = all_coef, coef = coef)
          }
        }
      }
    }
  }
}

### compare w/ direct scaling factors ----
smpls_cl <- fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["smpls_cluster"]]

scaling_factors_test_1 <- sapply(
  smpls_cl[[1]]$test_ids,
  function(test_cluster_inds) {
    1 / prod(sapply(test_cluster_inds, length))
  }
)
scaling_factors_test_2 <- sapply(
  smpls_cl[[2]]$test_ids,
  function(test_cluster_inds) {
    1 / prod(sapply(test_cluster_inds, length))
  }
)
scaling_factors_test_3 <- sapply(
  smpls_cl[[3]]$test_ids,
  function(test_cluster_inds) {
    1 / prod(sapply(test_cluster_inds, length))
  }
)

sum(estimates[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["1"]][["scaling_factors"]] |> as_vector() - scaling_factors_test_1)
sum(estimates[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["2"]][["scaling_factors"]] |> as_vector() - scaling_factors_test_2)
sum(estimates[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["3"]][["scaling_factors"]] |> as_vector() - scaling_factors_test_3)

### compare coefficients ----
sum(imp_dml_fits_bcr_slx_gen_stack[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_coef"]][,1] - fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_coef"]][,1])
sum(imp_dml_fits_bcr_slx_gen_stack[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_coef"]][,2] - fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_coef"]][,2])
sum(imp_dml_fits_bcr_slx_gen_stack[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_coef"]][,3] - fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["3"]][["all_coef"]][,3])

rm(
  list = setdiff(
    ls(),
    c(
      "fit",
      "interact_stat",
      "var_scaling_factor",
      "test_cluster_metadata",
      "estimates",
      "imp_dml_fits_bcr_slx_gen_stack"
      )
    )
  )
gc()

## se ----
### IMPORTANT: similarly, code below takes clusters from what DML intended, not from what was actually assigned
for(stat in interact_stat){
  list_1_a <- estimates[[stat]]
  list_1_b <- test_cluster_metadata[[stat]]
  list_1_c <- var_scaling_factor[[stat]]
  start_yrs <- names(list_1_a)
  for(year in start_yrs){
    list_2_a <- list_1_a[[year]]
    list_2_b <- list_1_b[[year]]
    list_2_c <- list_1_c[[year]]
    lag_names <- names(list_2_a)
    for(lag in lag_names){
      list_3_a <- list_2_a[[lag]]
      list_3_b <- list_2_b[[lag]]
      lag_var_scaling_factor <- list_2_c[[lag]]
      treat_names <- names(list_3_a)
      for(treat in treat_names){
        list_4_a <- list_3_a[[treat]]
        list_4_b <- list_3_b[[treat]]
        m <- seq_len(length(list_4_a))
        split_names <- treat |> 
          str_split_1("_AND_")
        for(i in m){
          list_5_a <- list_4_a[[i]]
          list_5_b <- list_4_b[[i]]
          n_rep <- seq_len(length(list_5_a))
          for(rep in n_rep){
            list_6_a <- list_5_a[[rep]]
            list_6_b <- list_5_b[[rep]]
            n_folds <- seq_len(length(list_6_b))
            if(length(split_names) == 1){
              gamma_hat <- 0
              j_hat <- 0
              for(i_fold in n_folds){
                # I_k <- list_6_b[[i_fold]]$cluster_inds[[1]]
                # J_l <- list_6_b[[i_fold]]$cluster_inds[[2]]
                I_k <- fit[[stat]][[year]][[lag]][[treat]][[i]][["smpls_cluster"]][[rep]][["test_ids"]][[i_fold]][[1]]
                J_l <- fit[[stat]][[year]][[lag]][[treat]][[i]][["smpls_cluster"]][[rep]][["test_ids"]][[i_fold]][[2]]
                const <- min(length(I_k), length(J_l)) / ((length(I_k) * length(J_l))^2)
                # first_cluster_var <- list_6_b[[i_fold]]$cluster_cols[[1]]
                # second_cluster_var <- list_6_b[[i_fold]]$cluster_cols[[2]]
                dat <- fit[[stat]][[year]][[lag]][[treat]][[i]]$data$data |> 
                  select(cow, year)
                test_index <- fit[[stat]][[year]][[lag]][[treat]][[i]][["smpls"]][[rep]][["test_ids"]][[i_fold]]
                first_cluster_var <- dat[test_index][[1]]
                second_cluster_var <-dat[test_index][[2]] 
                psi_fold <- list_6_a$psi[[i_fold]]
                for(cluster_value in I_k){
                  # ind_cluster <- first_cluster_var == cluster_value
                  # note: need to use `& second_cluster_var %in% J_l` (and the opposite) since we're trying to rely on the intended `test_ids`, not the manually-found `cluster_cols`
                  ind_cluster <- (first_cluster_var == cluster_value) &
                    second_cluster_var %in% J_l
                  gamma_hat <- gamma_hat + const * sum(psi_fold[ind_cluster])^2
                }
                for(cluster_value in J_l){
                  # ind_cluster <- second_cluster_var == cluster_value
                  ind_cluster <- (second_cluster_var == cluster_value) &
                    first_cluster_var %in% I_k
                  gamma_hat <- gamma_hat + const * sum(psi_fold[ind_cluster])^2
                }
                j_hat <- j_hat + sum(list_6_b[[i_fold]]$psi_a) / (length(I_k) * length(J_l))
              }
              gamma_hat <- gamma_hat / length(n_folds)
              j_hat <- j_hat / length(n_folds)
              sigma2_hat <- gamma_hat / (j_hat^2) / lag_var_scaling_factor
              estimates[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][[as.character(rep)]]$se <- sqrt(sigma2_hat)
            }
            else{
              d_all <- names(list_6_a)
              for(d_name in d_all){
                gamma_hat <- 0
                j_hat <- 0
                for(i_fold in n_folds){
                  # I_k <- list_6_b[[i_fold]]$cluster_inds[[1]]
                  # J_l <- list_6_b[[i_fold]]$cluster_inds[[2]]
                  I_k <- fit[[stat]][[year]][[lag]][[treat]][[i]][["smpls_cluster"]][[rep]][["test_ids"]][[i_fold]][[1]]
                  J_l <- fit[[stat]][[year]][[lag]][[treat]][[i]][["smpls_cluster"]][[rep]][["test_ids"]][[i_fold]][[2]]
                  const <- min(length(I_k), length(J_l)) / ((length(I_k) * length(J_l))^2)
                  # first_cluster_var <- list_6_b[[i_fold]]$cluster_cols[[1]]
                  # second_cluster_var <- list_6_b[[i_fold]]$cluster_cols[[2]]
                  dat <- fit[[stat]][[year]][[lag]][[treat]][[i]]$data$data |> 
                    select(cow, year)
                  test_index <- fit[[stat]][[year]][[lag]][[treat]][[i]][["smpls"]][[rep]][["test_ids"]][[i_fold]]
                  first_cluster_var <- dat[test_index][[1]]
                  second_cluster_var <- dat[test_index][[2]] 
                  psi_fold <- list_6_a[[d_name]]$psi[[i_fold]]
                  for(cluster_value in I_k){
                    # ind_cluster <- first_cluster_var == cluster_value
                    ind_cluster <- (first_cluster_var == cluster_value) &
                      second_cluster_var %in% J_l
                    gamma_hat <- gamma_hat + const * sum(psi_fold[ind_cluster])^2
                  }
                  for(cluster_value in J_l){
                    # ind_cluster <- second_cluster_var == cluster_value
                    ind_cluster <- (second_cluster_var == cluster_value) &
                      first_cluster_var %in% I_k
                    gamma_hat <- gamma_hat + const * sum(psi_fold[ind_cluster])^2
                  }
                  j_hat <- j_hat + sum(list_6_b[[i_fold]][[d_name]]$psi_a) / (length(I_k) * length(J_l))
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
            all_coef <- imp_dml_fits_bcr_slx_gen_stack[[stat]][[year]][[lag]][[treat]][[i]][["all_coef"]]
            coef <- imp_dml_fits_bcr_slx_gen_stack[[stat]][[year]][[lag]][[treat]][[i]][["coef"]]
            se <- sqrt(apply(
              lag_var_scaling_factor * all_se^2 + (all_coef - coef)^2,
              1, function(x) median(x, na.rm = TRUE)) / lag_var_scaling_factor)
            names(se) <- treat
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$all_se <- all_se
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$se <- se
          }
          else{
            se_list <- list()
            for(d_name in d_all){
              d_pos <- match(d_name, d_all)
              
              all_se <- t(unname(sapply(estimates[[stat]][[year]][[lag]][[treat]][[i]], function(x) x[[d_name]]$se)))
              all_coef <- imp_dml_fits_bcr_slx_gen_stack[[stat]][[year]][[lag]][[treat]][[i]][["all_coef"]][d_pos,]
              coef <- imp_dml_fits_bcr_slx_gen_stack[[stat]][[year]][[lag]][[treat]][[i]][["coef"]][[d_name]]
              se <- sqrt(apply(
                lag_var_scaling_factor * all_se^2 + (all_coef - coef)^2,
                1, function(x) median(x, na.rm = TRUE)) / lag_var_scaling_factor)
              names(se) <- d_name
              se_list[[as.character(d_name)]] <- list(all_se = all_se, se = se)
            }
            all_se <- t(unname(sapply(se_list, function(x) rbind(x$all_se))))
            se <- sapply(se_list, function(x) rbind(x$se))
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$all_se <- all_se
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$se <- se
          }
        }
      }
    }
  }
}

### compare SEs ----
#### discrepancies are likely floating-point rounding errors
sum(imp_dml_fits_bcr_slx_gen_stack[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_se"]][,1] - fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_se"]][,1])
sum(imp_dml_fits_bcr_slx_gen_stack[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_se"]][,2] - fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_se"]][,2])
sum(imp_dml_fits_bcr_slx_gen_stack[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_se"]][,3] - fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["all_se"]][,3])

sum(imp_dml_fits_bcr_slx_gen_stack[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["se"]] - fit[["no_interactions"]][["start_1977"]][["l1"]][["lech_hr_mean"]][["1"]][["se"]])

rm(
  list = setdiff(
    ls(),
    c(
      "fit",
      "interact_stat",
      "var_scaling_factor",
      "estimates",
      "imp_dml_fits_bcr_slx_gen_stack"
      )
    )
  )
gc()

# compute t-stat, p-val, conf int ----
format_perc <- function(probs, digits){
  paste(
    format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
    "%"
  )
}
level <- 0.95

for(stat in interact_stat){
  list_1 <- imp_dml_fits_bcr_slx_gen_stack[[stat]]
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
            coef <- list_5$coef
            se <- list_5$se
            t_stat <- coef / se
            pval <- 2 * pnorm(-abs(t_stat))
            a <- (1 - level) / 2
            a <- c(a, 1 - a)
            pct <- format_perc(a, 3)
            fac <- qnorm(a)
            ci <- array(
              NA_real_,
              dim = c(length(coef), 2L),
              dimnames = list(names(coef), pct)
              )
            ci[] <- coef + se %o% fac
            
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$t_stat <- t_stat
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$pval <- pval
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$ci <- ci
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][order(names(imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]))]
            }
          else{
            stat_list <- list()
            for(d_name in split_names){
              d_pos <- match(d_name, names(list_5[["coef"]]))
              
              coef <- list_5[["coef"]][d_pos]
              se <- list_5[["se"]][d_pos]
              t_stat <- coef / se
              pval <- 2 * pnorm(-abs(t_stat))
              a <- (1 - level) / 2
              a <- c(a, 1 - a)
              pct <- format_perc(a, 3)
              fac <- qnorm(a)
              ci <- array(
                NA_real_,
                dim = c(length(coef), 2L),
                dimnames = list(names(coef), pct)
                )
              ci[] <- coef + se %o% fac
              
              stat_list[[as.character(d_name)]] <- list(t_stat = t_stat, pval = pval, ci = ci)
            }
            t_stat <- sapply(stat_list, function(x) rbind(x$t_stat))
            pval <- sapply(stat_list, function(x) rbind(x$pval))
            ci <- do.call(rbind, lapply(stat_list, function(x) x$ci))
            
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$t_stat <- t_stat
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$pval <- pval
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]$ci <- ci
            imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]] <- imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]][order(names(imp_dml_fits_bcr_slx_gen_stack[[as.character(stat)]][[as.character(year)]][[as.character(lag)]][[as.character(treat)]][[as.character(i)]]))]
          }
        }
      }
    }
  }
}

# save estimates ----
# imp_dml_fits_bcr_slx_gen_stack |> 
#   save(file = here("data/ch1/results/fits/dml_stack/final/imp_dml_fits_bcr_slx_gen_stack.rda"))
