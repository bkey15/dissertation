# Prep lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(DoubleML)
library(data.table)

# load data ----
load(here("data/ch1/results/imputations/imp_2_sp_l1.rda"))
load(here("data/ch1/results/imputations/imp_3_sp_l1.rda"))

# get imputed datasets ----
## imp_2 (1968) ----
imp_2_dfs <- list()
m <- 1:imp_2_sp_l1$m

for(i in m){
  imp_df <- imp_2_sp_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(
      glb_s == 1,
      .imp == i
      ) |> 
    select(
      -starts_with("nn_"),
      -glb_s,
      -last_col(),
      -last_col(offset = 1),
      -cow,
      -inforce
    )
  
  imp_2_dfs[[as.character(i)]] <- imp_df
}

## imp_3 (1977) ----
imp_3_dfs <- list()
m <- 1:imp_3_sp_l1$m

for(i in m){
  imp_df <- imp_3_sp_l1 |> 
    mice::complete(
      action = "long",
      include = TRUE
    ) |> 
    filter(
      glb_s == 1,
      .imp == i
    ) |> 
    select(
      -starts_with("nn_"),
      -glb_s,
      -last_col(),
      -last_col(offset = 1),
      -cow,
      -inforce
    )
  
  imp_3_dfs[[as.character(i)]] <- imp_df
}

# initialize data backend ----
## imp_2 ----
### get initial specs ----
m <- 1:imp_2_sp_l1$m
imp_2_dml_south_spat_dats <- list()

### general ----
covars <- model.matrix(
  ~ . - 1, data = imp_2_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -starts_with(
      c("hr_score", "lech_", "ns_", "ss_")
      ),
    -ends_with("_mean"),
    -contains("pop_mean")
    )

treat_names_cpr <- imp_2_dfs[[1]] |> 
  select(
    starts_with("cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

treat_names_esr <- imp_2_dfs[[1]] |> 
  select(
    starts_with("esr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_2_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_cpr)){
    k <- treat_names_cpr[[j]]
    l <- treat_names_esr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
          )
        )
      ) |> 
      names()
    imp_2_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### lech general ----
covars <- model.matrix(
  ~ . - 1, data = imp_2_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -starts_with(
      c("hr_score", "cpr_", "esr_", "ns_", "ss_")
      ),
    -ends_with("_mean"),
    -contains("pop_mean")
    )

treat_names_lech <- imp_2_dfs[[1]] |> 
  select(
    starts_with("lech_hr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_2_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_lech)){
    k <- treat_names_lech[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
            )
          )
        ) |> 
      names()
    imp_2_dml_south_spat_dats[[as.character(k)]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = k,
        y_col = "hr_score"
      )
  }
}

### cpr ----
covars <- model.matrix(
  ~ . - 1, data = imp_2_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "lech_", "esr_", "pop_mean")
      ),
    -ends_with("_mean"),
    -starts_with("cpr_")
    )

treat_names_ss_cpr <- imp_2_dfs[[1]] |> 
  select(
    starts_with("ss_cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

treat_names_ns_cpr <- imp_2_dfs[[1]] |> 
  select(
    starts_with("ns_cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_2_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
            )
          )
        ) |> 
      names()
    imp_2_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### esr ----
covars <- model.matrix(
  ~ . - 1, data = imp_2_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "lech_", "cpr_", "pop_mean")
      ),
    -ends_with("_mean"),
    -starts_with("esr_")
    )

treat_names_ss_esr <- imp_2_dfs[[1]] |> 
  select(
    starts_with("ss_esr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

treat_names_ns_esr <- imp_2_dfs[[1]] |> 
  select(
    starts_with("ns_esr"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_2_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_esr)){
    k <- treat_names_ss_esr[[j]]
    l <- treat_names_ns_esr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
            )
          )
        ) |> 
      names()
    imp_2_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
        )
  }
}

### both ----
covars <- model.matrix(
  ~ . - 1, data = imp_2_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "lech_", "pop_mean")
      ),
    -starts_with(c("cpr_", "esr_")),
    -ends_with("_mean")
  )

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_2_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    n <- treat_names_ss_esr[[j]]
    o <- treat_names_ns_esr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            n,
            o,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
            )
          )
        ) |> 
      names()
    imp_2_dml_south_spat_dats[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l, n, o),
        y_col = "hr_score"
      )
  }
}

### lechner south ----
covars <- model.matrix(
  ~ . - 1, data = imp_2_dfs[[1]]
  ) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "cpr_", "esr_", "pop_mean")
      ),
    -ends_with("_mean"),
    -starts_with(
      c("cpr_", "esr_", "lech_")
      )
    )

treat_names_ss_lech <- imp_2_dfs[[1]] |> 
  select(
    starts_with("ss_lech"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

treat_names_ns_lech <- imp_2_dfs[[1]] |> 
  select(
    starts_with("ns_lech"),
    -ends_with(c("pop_mean", "sp_lag"))
    ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_2_dfs[[i]]
    ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_lech)){
    k <- treat_names_ss_lech[[j]]
    l <- treat_names_ns_lech[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
            )
          )
        ) |> 
      names()
    imp_2_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

## imp_3 ----
### get initial specs ----
m <- 1:imp_3_sp_l1$m
imp_3_dml_south_spat_dats <- list()

### general ----
covars <- model.matrix(
  ~ . - 1, data = imp_3_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    -starts_with(
      c("hr_score", "lech_", "ns_", "ss_")
    ),
    -ends_with("_mean"),
    -contains("pop_mean")
  )

treat_names_cpr <- imp_3_dfs[[1]] |> 
  select(
    starts_with("cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

treat_names_esr <- imp_3_dfs[[1]] |> 
  select(
    starts_with("esr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_cpr)){
    k <- treat_names_cpr[[j]]
    l <- treat_names_esr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
          )
        )
      ) |> 
      names()
    imp_3_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### lech general ----
covars <- model.matrix(
  ~ . - 1, data = imp_3_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    -starts_with(
      c("hr_score", "cpr_", "esr_", "ns_", "ss_")
    ),
    -ends_with("_mean"),
    -contains("pop_mean")
  )

treat_names_lech <- imp_3_dfs[[1]] |> 
  select(
    starts_with("lech_hr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_lech)){
    k <- treat_names_lech[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
          )
        )
      ) |> 
      names()
    imp_3_dml_south_spat_dats[[as.character(k)]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = k,
        y_col = "hr_score"
      )
  }
}

### cpr ----
covars <- model.matrix(
  ~ . - 1, data = imp_3_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "lech_", "esr_", "pop_mean")
    ),
    -ends_with("_mean"),
    -starts_with("cpr_")
  )

treat_names_ss_cpr <- imp_3_dfs[[1]] |> 
  select(
    starts_with("ss_cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

treat_names_ns_cpr <- imp_3_dfs[[1]] |> 
  select(
    starts_with("ns_cpr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
          )
        )
      ) |> 
      names()
    imp_3_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### esr ----
covars <- model.matrix(
  ~ . - 1, data = imp_3_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "lech_", "cpr_", "pop_mean")
    ),
    -ends_with("_mean"),
    -starts_with("esr_")
  )

treat_names_ss_esr <- imp_3_dfs[[1]] |> 
  select(
    starts_with("ss_esr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

treat_names_ns_esr <- imp_3_dfs[[1]] |> 
  select(
    starts_with("ns_esr"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_esr)){
    k <- treat_names_ss_esr[[j]]
    l <- treat_names_ns_esr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
          )
        )
      ) |> 
      names()
    imp_3_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

### both ----
covars <- model.matrix(
  ~ . - 1, data = imp_3_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "lech_", "pop_mean")
    ),
    -starts_with(c("cpr_", "esr_")),
    -ends_with("_mean")
  )

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_cpr)){
    k <- treat_names_ss_cpr[[j]]
    l <- treat_names_ns_cpr[[j]]
    n <- treat_names_ss_esr[[j]]
    o <- treat_names_ns_esr[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            n,
            o,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
          )
        )
      ) |> 
      names()
    imp_3_dml_south_spat_dats[[paste(as.character(k), as.character(l), as.character(n), as.character(o), sep = "_AND_")]][[as.character(i)]] <- df |> 
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l, n, o),
        y_col = "hr_score"
      )
  }
}

### lechner south ----
covars <- model.matrix(
  ~ . - 1, data = imp_3_dfs[[1]]
) |> 
  as_tibble() |> 
  select(
    -contains(
      c("hr_score", "cpr_", "esr_", "pop_mean")
    ),
    -ends_with("_mean"),
    -starts_with(
      c("cpr_", "esr_", "lech_")
    )
  )

treat_names_ss_lech <- imp_3_dfs[[1]] |> 
  select(
    starts_with("ss_lech"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

treat_names_ns_lech <- imp_3_dfs[[1]] |> 
  select(
    starts_with("ns_lech"),
    -ends_with(c("pop_mean", "sp_lag"))
  ) |> 
  names()

for(i in m){
  df <- model.matrix(
    ~ . - 1, data = imp_3_dfs[[i]]
  ) |> 
    as.data.table()
  for(j in seq_along(treat_names_ss_lech)){
    k <- treat_names_ss_lech[[j]]
    l <- treat_names_ns_lech[[j]]
    covar_names <- covars |> 
      select(
        starts_with(
          c(
            "year",
            k,
            l,
            "v2",
            "e_polity2",
            "wdi",
            "p_durable",
            "gdp",
            "pop",
            "inv",
            "hras",
            "bop"
          )
        )
      ) |> 
      names()
    imp_3_dml_south_spat_dats[[paste(as.character(k), as.character(l), sep = "_AND_")]][[as.character(i)]] <- df |>
      double_ml_data_from_data_frame(
        x_cols = covar_names,
        d_cols = c(k, l),
        y_col = "hr_score"
      )
  }
}

# save initialized data ----
imp_2_dml_south_spat_dats |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_2_dml_south_spat_dats.rda"))
imp_3_dml_south_spat_dats |> 
  save(file = here("data/ch1/results/fits/dml_lasso/dml_initial/imp_3_dml_south_spat_dats.rda"))
