# load packages ----
library(tidyverse)
library(here)
library(tools)
library(knitr)

# load data ----
## no start year ----
sums_nost <- dir("data/ch1/results/fits/hb_rep/standard/no_start_year", full.names = TRUE)

for(file in sums_nost){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}

## 1968 ----
sums_1968 <- dir("data/ch1/results/fits/hb_rep/standard/start_1968", full.names = TRUE)

for(file in sums_1968){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}

## 1977 ----
sums_1977 <- dir("data/ch1/results/fits/hb_rep/standard/start_1977", full.names = TRUE)

for(file in sums_1977){
  data <- load(here(file))
  filename <- basename(file) |> 
    file_path_sans_ext()
  assign(filename, get(data, envir = .GlobalEnv))
}

# create tables displaying dataset-treatment result ----

