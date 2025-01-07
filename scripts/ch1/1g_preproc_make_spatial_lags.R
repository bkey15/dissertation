# Note: is essential to make spatial lags post-imputation, because weights don't work w/ NA values (i.e., 0.5*1 + 0.5*NA = NA)

# load packages ----
library(tidyverse)
library(here)
library(rnaturalearth)
library(countrycode)
library(sf)
library(spdep)

# load data ----
load(here("data/ch1/results/imputations/imp_2.rda"))
load(here("data/ch1/results/imputations/imp_3.rda"))

# prep imp data ----
## complete data ----
### imp_2 ----
imp_2 <- imp_2 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(.imp != 0) |> 
  mutate(
    cow = as.numeric(levels(cow))[cow],
    year = as.numeric(levels(year))[year]
    ) |> 
  relocate(.imp, .id)

### imp_3 ----
imp_3 <- imp_3 |> 
  mice::complete(
    action = "long",
    include = TRUE
    ) |> 
  filter(.imp != 0) |> 
  mutate(
    cow = as.numeric(levels(cow))[cow],
    year = as.numeric(levels(year))[year]
    ) |> 
  relocate(.imp, .id)

## impose val == 0 to all NAs in ss & nn ----
## IMPORTANT: this step is necessary to ensure that the lags are executed. See note in first code line. This is probably justifiable theoretically, too.
imp_2 <- imp_2 |> 
  mutate(
    across(
      starts_with("ss_") | starts_with("nn_"),
      ~ if_else(is.na(.x), 0, .x)
      )
    )

imp_3 <- imp_3 |> 
  mutate(
    across(
      starts_with("ss_") | starts_with("nn_"),
      ~ if_else(is.na(.x), 0, .x)
      )
    )

# prep spatial data ----
## get world country polygons ----
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
  ) |> 
  arrange(subunit)

## get cow codes ----
## Note: France and Norway being manually coded b/c ISO code is -99.
world <- world |> 
  mutate(
    cow = countrycode(
      sourcevar = iso_a3,
      origin = "iso3c",
      destination = "cown"
      ),
    cow = case_when(
      iso_a3 == "SRB" ~ 345,
      subunit == "France" ~ 220,
      subunit == "Norway" ~ 385,
      .default = cow
      )
    ) |> 
  select(cow, region_wb) |> 
  arrange(cow)

## replicate polygons for all years ----
years <- tibble(year = 1968:2019)
world_years <- world |> 
  cross_join(years)

## merge ----
### make merge-by var (cy_id) ----
## Note: need to join w/ world_years as the "base" to preserve sf object, hence the circuitous solution below
imp_2 <- imp_2 |> 
  mutate(
    cy_id = paste(
      cow,
      year,
      sep = "-"
      )
    )
imp_3 <- imp_3 |> 
  mutate(
    cy_id = paste(
      cow,
      year,
      sep = "-"
      )
    )
world_years <- world_years |> 
  mutate(
    cy_id = paste(
      cow,
      year,
      sep = "-"
      )
    )

### 1968 start ----
cys_1968 <- unique(imp_2$cy_id)

imp_2_spt <- world_years |> 
  left_join(imp_2) |> 
  filter(cy_id %in% cys_1968) |> 
  select(-cy_id)

### 1977 start ----
cys_1977 <- unique(imp_3$cy_id)

imp_3_spt <- world_years  |> 
  left_join(imp_3) |> 
  filter(cy_id %in% cys_1977) |> 
  select(-cy_id)

# make neighborhood weight matrices ----
## get raw polygon distance matrices ----
## Note: the for loop below will take a while to run
years <- 1968:2019
dist_mats <- list()

for(i in years){
  yr_i <- imp_2_spt |> 
    filter(
      .imp == 1,
      year == i
      )
  dist_mat <- st_distance(yr_i, yr_i) |> 
    as_tibble()
  dist_mats[[as.character(i)]] <- dist_mat
  }

## get neighbors ----
## Note: neighbor index == position in dist_mat (so US is 1, as first in cow list, by which I arranged imp_..._spt)
nb_lists <- list()
years <- dist_mats |> 
  names()

for(i in years){
  nb_names <- dist_mats[[i]] |> 
    names()
  for(name in nb_names){
    neighbors <- dist_mats[[i]] |> 
      pull(name) |> 
      units::set_units("km") |> 
      as_tibble() |> 
      mutate(
        index = seq_along(dist_mats[[i]]),
        distance = as.numeric(value),
        neighbor = if_else(
          distance <= 200,
          TRUE,
          FALSE
          )
        ) |> 
      filter(
        neighbor == TRUE,
        index != as.numeric(str_remove(name, "V"))
        ) |> 
      select(index) |> 
      pull(index)
      
    nb_lists[[i]][[str_remove(name, "V")]] <- neighbors
    }
  }

## impose index == 0 for states w/o neighbors ----
## Note: step needed to execute nb2listw()
years <- nb_lists |> 
  names()

for(i in years) {
  yr_i <- nb_lists[[i]]
  for(j in seq_along(yr_i)){
    if(length(yr_i[[j]]) == 0){
      nb_lists[[i]][[j]] <- as.integer(0)
      } 
    else{
      nb_lists[[i]][[j]] <- nb_lists[[i]][[j]]
      }
    }
  }


## get weights ----
years <- nb_lists |> 
  names()

for(i in years){
  class(nb_lists[[i]]) <- "nb"
  nb_lists[[i]] <- nb_lists[[i]] |> 
    nb2listw(zero.policy = TRUE)
  }


# make spatial lags ----
## 1968 start ----
### get specs ----
lags_dat_1968 <- list()
m <- unique(imp_2_spt$.imp)
years <- nb_lists |> 
  names()
var_names <- imp_2_spt |> 
  select(
    -c(.imp, .id, cow, region_wb, year, inforce, glb_s)
    ) |> 
  names()
var_names <- var_names[!var_names == "geometry"]

### get data ----
for(imp in m){
  for(i in years){
    yr_i <- imp_2_spt |> 
      filter(
        .imp == imp,
        year == as.numeric(i)
        )
    for(var in var_names){
      var_i <- yr_i |> 
        pull(var)
      nb_listw_i <- nb_lists[[i]]
      lags <- lag.listw(
        x = nb_listw_i,
        var = var_i,
        zero.policy = TRUE,
        NAOK = TRUE
        )
      lags_dat_1968[[as.character(imp)]][[i]][[paste(var, "sp_lag", sep = "_")]] <- lags
    }
    yr_i_cow <- yr_i |> 
      pull(cow)
    yr_i_region <- yr_i |> 
      pull(region_wb)
    yr_i_ids <- tibble(
      .imp = imp,
      cow = yr_i_cow,
      region = yr_i_region,
      year = as.numeric(i)
    )
    lags_dat_1968[[as.character(imp)]][[i]] <- bind_cols(yr_i_ids, lags_dat_1968[[as.character(imp)]][[i]])
  }
  lags_dat_1968[[as.character(imp)]] <- bind_rows(lags_dat_1968[[imp]])
}

lags_dat_1968 <- bind_rows(lags_dat_1968)

## 1977 start ----
### get specs ----
lags_dat_1977 <- list()
nb_lists_small <- nb_lists[-c(1:9)]
m <- unique(imp_3_spt$.imp)
years <- nb_lists_small |> 
  names()
var_names <- imp_3_spt |> 
  select(
    -c(.imp, .id, cow, region_wb, year, inforce, glb_s)
  ) |> 
  names()
var_names <- var_names[!var_names == "geometry"]

### get data ----
for(imp in m){
  for(i in years){
    yr_i <- imp_3_spt |> 
      filter(
        .imp == imp,
        year == as.numeric(i)
      )
    for(var in var_names){
      var_i <- yr_i |> 
        pull(var)
      nb_listw_i <- nb_lists_small[[i]]
      lags <- lag.listw(
        x = nb_listw_i,
        var = var_i,
        zero.policy = TRUE,
        NAOK = TRUE
      )
      lags_dat_1977[[as.character(imp)]][[i]][[paste(var, "sp_lag", sep = "_")]] <- lags
    }
    yr_i_cow <- yr_i |> 
      pull(cow)
    yr_i_region <- yr_i |> 
      pull(region_wb)
    yr_i_ids <- tibble(
      .imp = imp,
      cow = yr_i_cow,
      region = yr_i_region,
      year = as.numeric(i)
      )
    lags_dat_1977[[as.character(imp)]][[i]] <- bind_cols(yr_i_ids, lags_dat_1977[[as.character(imp)]][[i]])
  }
  lags_dat_1977[[as.character(imp)]] <- bind_rows(lags_dat_1977[[imp]])
}

lags_dat_1977 <- bind_rows(lags_dat_1977)

# save ----
lags_dat_1968 |> 
  save(file = here("data/ch1/results/imputations/lags_dat_1968.rda"))
lags_dat_1977 |> 
  save(file = here("data/ch1/results/imputations/lags_dat_1977.rda"))
