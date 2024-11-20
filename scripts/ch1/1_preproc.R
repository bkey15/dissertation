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

# prep lechner data ----
## non-dyads ----
### initial split
non_dyads <- lechner |> 
  filter(str_count(name, "\\S+") != 2)

### isolate false non-dyads
remrows_1 <- c(66, 130, 173, 175, 184, 188, 249, 280, 307, 308, 347, 416, 817)

false_non_dyads <- non_dyads |> 
  filter(
    str_detect(name, "Bosnia and Herzegovina") |
      str_detect(name, "New Zealand") |
      str_detect(name, "Papua New Guinea") |
      str_detect(name, "Belarus Russia") |
      str_detect(name, "Costa Rica") |
      str_detect(name, "Canada US") |
      str_detect(name, "Hong Kong") |
      str_detect(name, "El Salvador") |
      str_detect(name, "Croatia Macedonia") |
      str_detect(name, "Czech Republic") |
      str_detect(name, "Dominican Republic") |
      str_detect(name, "Faroe Islands") |
      str_detect(name, "German Democratic Republic") |
      str_detect(name, "France Tunisia") |
      str_detect(name, "Sri Lanka") |
      str_detect(name, "Ireland UK") |
      str_detect(name, "Saudi Arabia") |
      str_detect(name, "South Africa") |
      str_detect(name, "Trinidad and Tobago") |
      str_detect(name, "United States") |
      str_detect(name, "Jordan Sudan") |
      str_detect(name, "Latvia Ukraine")
    ) |> 
  filter(!number %in% remrows_1)

## dyads ----
### initial split
dyads <- lechner |> 
  filter(str_count(name, "\\S+") == 2)

### isolate false dyads
remrows_2 <- c(4, 100, 253, 291, 682)

false_dyads <- dyads |> 
  filter(
    str_detect(name, "EFTA") |
      str_detect(name, "EC") |
      str_detect(name, "MERCOSUR") |
      str_detect(name, "Lome") |
      str_detect(name, "Yaound")
    ) |> 
  filter(!number %in% remrows_2)

