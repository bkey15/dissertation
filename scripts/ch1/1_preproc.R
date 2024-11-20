# load packages ----
library(tidyverse)
library(here)

# load data ----
## lechner ----
lechner <- read_delim(
  file = "data/ch1/lechner_data/nti_201711.txt"
  )

desta <- read_delim(
  file = "data/ch1/desta/desta_version_02_02.txt"
  )

withdraw <- read_delim(
  file = "data/ch1/desta/desta_dyadic_withdrawal_02_02.txt"
  )

## hr scores ----
load(here("data/common/fariss/LHRS-v4.02-2021.Rdata"))
hrs <- data
rm(data)

# split dyads ----


