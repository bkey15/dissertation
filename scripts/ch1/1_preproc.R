# load packages ----
library(tidyverse)
library(here)

# load data ----
lechner <- read_delim(
  file = "data/ch1/lechner_data/nti_201711.txt"
  )

desta <- read_delim(
  file = "data/ch1/desta/desta_version_02_02.txt"
  )
