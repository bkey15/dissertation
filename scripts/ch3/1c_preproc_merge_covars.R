# load packages ----
library(tidyverse)
library(here)
library(vdemdata)
library(naniar)
library(states)
library(WDI)

# load data ----
## ist panel
load(here("data/ch3/preprocessed/ist_panel.rda"))

## vdem
vdem <- vdem

## hr scores
load(here("data/common/raw/fariss/LHRS-v4.02-2021.Rdata"))
hrs <- data
rm(data)

## gdp, gdppc, pop, glb_s (risks/vulnerabilities?)
load(here("data/ch1/preprocessed/standards.rda"))

## bop, wdi +, triggers
wdi1 <- WDI_data[[1]]

## proximity to us-led alliance
us_ally <- read_csv(
  "data/ch3/raw/unga_dat/IdealpointestimatesAll_Jun2024.csv",
  col_select = -1
  ) |> 
  janitor::clean_names()

test <- us_ally |> 
  mutate(year = session + 1945)

test2 <- test |> 
  left_join(hrs, by = join_by(ccode == COW, year == YEAR))

