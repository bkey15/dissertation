# Fit lasso models by tuning for lambda across all imputed datasets.

# load packages ----
library(tidyverse)
library(here)
library(mice)
library(glmnet)
library(naniar)

# load data ----
load(here("data/ch1/results/imputations/imp_1_l1.rda"))
load(here("data/ch1/results/imputations/imp_2_l1.rda"))
load(here("data/ch1/results/imputations/imp_3_l1.rda"))