# load packages ----
library(tidyverse)
library(here)
library(readxl)
library(vdemdata)
library(WDI)
library(countrycode)
library(labelled)

# load data ----
## ems
load(here("data/ch3/preprocessed/ems_standard.rda"))

## colpus
colpus <- read_xlsx(
  path = here("data/ch3/raw/colpus/Master_Coup List_all countries_basic.xlsx")
  )

## vdem
vdem <- vdem

## ch. 2 covars + hr_score
load(here("data/ch2/preprocessed/bits_final.rda"))

## wdi vars
gdp_growth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG")
inflation <- WDI(indicator = "NY.GDP.DEFL.KD.ZG")

## unemployment
unemp <- read_csv(
  file = here("data/common/raw/qog/qog_bas_ts_jan24.csv")
  )

## one-sided violence
one_sided_violence <- read_rds(here("data/ch3/raw/ucdp_prio/OneSided_v25_1.rds"))

## political proximity (to us-led alliance)
us_ally <- read_csv(
  "data/ch3/raw/unga_dat/IdealpointestimatesAll_Jun2024.csv",
  col_select = -1
  ) |> 
  janitor::clean_names()

# prep merge ----
## vdem ----
### controversial election indicators + neo-patrimonialism
## missing cases filtered out below are Zanzibar, Somaliland, Palestine/West Bank, etc.
vdem <- vdem |> 
  select(
    COWcode,
    year,
    v2elwestmon,
    v2elmonden,
    v2elmonref,
    v2x_neopat
    ) |> 
  filter(
    year > 1989,
    year < 2019,
    !is.na(COWcode)
    ) |> 
  rename(cow = COWcode) |> 
  arrange(cow, year)

## ch. 2 covars + hr_score ----
bits_final <- bits_final |> 
  select(cow, year, hr_score, 22:59) |> 
  filter(year > 1989 & year < 2019)

## wdi vars ----
gdp_growth <- gdp_growth |> 
  filter(year > 1989 & year < 2019) |> 
  mutate(
    cow = countrycode(
      sourcevar = country,
      origin = "country.name",
      destination = "cown"
      ),
    cow = if_else(country == "Serbia", 345, cow)
    ) |> 
  rename(wdi_gdpg = NY.GDP.MKTP.KD.ZG) |> 
  select(cow, year, wdi_gdpg) |> 
  filter(!is.na(cow)) |> 
  arrange(cow, year)

inflation <- inflation |> 
  filter(year > 1989 & year < 2019) |> 
  mutate(
    cow = countrycode(
      sourcevar = country,
      origin = "country.name",
      destination = "cown"
      ),
    cow = if_else(country == "Serbia", 345, cow)
    ) |> 
  rename(wdi_inflat = NY.GDP.DEFL.KD.ZG) |> 
  select(cow, year, wdi_inflat) |> 
  filter(!is.na(cow)) |> 
  arrange(cow, year)

## unemployment ----
unemp <- unemp |> 
  select(ccodecow, year, wdi_unempilo) |> 
  filter(year > 1989 & year < 2019) |> 
  rename(cow = ccodecow) |> 
  arrange(cow, year)

## political proximity ----
us_ally <- us_ally |> 
  mutate(year = session + 1945) |> 
  filter(year > 1989 & year < 2019) |> 
  select(ccode, year, ideal_point_all) |> 
  rename(
    cow = ccode,
    pol_prox = ideal_point_all
    )

## colpus ----
years_ist <- tibble(year = 1990:2018)

colpus_base <- colpus |> 
  filter(
    success == 1,
    year > 1989,
    year < 2019
    ) |> 
  select(ccode) |> 
  distinct() |> 
  cross_join(years_ist)

colpus <- colpus |> 
  filter(
    success == 1,
    year > 1989,
    year < 2019
    ) |> 
  select(ccode, year, success) |> 
  right_join(colpus_base) |> 
  mutate(
    success = if_else(
      is.na(success), 0, success
      )
    ) |> 
  rename(
    coup_success = success,
    cow = ccode
    ) |> 
  arrange(cow, year)

## one_sided_violence ----
## note: filter out missing cow row (Rwandi & Burundi, 1996, accounted for in a different row)
one_sided_violence_base <- one_sided_violence |> 
  filter(
    is_government_actor == 1,
    year > 1989,
    year < 2019
    ) |> 
  mutate(
    gwnoa = as.numeric(gwnoa),
    cow = countrycode(
      sourcevar = gwnoa,
      origin = "gwn",
      destination = "cown"
      )
    ) |> 
  select(cow) |> 
  filter(!is.na(cow)) |> 
  distinct() |> 
  cross_join(years_ist)

one_sided_violence <- one_sided_violence |> 
  filter(
    is_government_actor == 1,
    year > 1989,
    year < 2019
    ) |> 
  mutate(
    gwnoa = as.numeric(gwnoa),
    cow = countrycode(
      sourcevar = gwnoa,
      origin = "gwn",
      destination = "cown"
      )
    ) |> 
  mutate(gov_kill = 1) |> 
  select(cow, year, gov_kill) |> 
  right_join(one_sided_violence_base) |> 
  mutate(
    gov_kill = if_else(
      is.na(gov_kill), 0, gov_kill
      )
    ) |> 
  arrange(cow, year)

# merge ----
## vdem, hr_score, & ch.2 covars
merge_base <- vdem |> 
  left_join(bits_final) |> 
  relocate(hr_score, .after = year)

## wdi vars
merge_base <- merge_base |> 
  left_join(gdp_growth) |> 
  left_join(inflation) |> 
  relocate(
    c(wdi_gdpg, wdi_inflat),
    .after = wdi_popden
    ) |> 
  remove_labels()

## unemployment
merge_base <- merge_base |> 
  left_join(unemp) |> 
  relocate(wdi_unempilo, .after = wdi_inflat)

## political proximity
merge_base <- merge_base |> 
  left_join(us_ally)

## colpus
merge_base <- merge_base |> 
  left_join(colpus) |> 
  relocate(coup_success, .after = hr_score)

## one_sided_violence
merge_base <- merge_base |> 
  left_join(one_sided_violence) |> 
  relocate(gov_kill, .after = hr_score)

## ems
merge_base <- merge_base |> 
  left_join(
    ems_standard,
    by = join_by(cow == target, year)
    ) |> 
  relocate(
    c(n_ems, any_inforce),
    .after = hr_score
    )

# filter ----
## remove cases where hr_score is NA
merge_base <- merge_base |> 
  filter(!is.na(hr_score))

## remove non-glb_s cases (save for Slovenia & Saudi Arabia, only cases w/ sanctions where glb_s == 0)
merge_base <- merge_base |> 
  filter(glb_s == 1 | !is.na(any_inforce)) |> 
  select(-glb_s)

# save ----
merge_base |> 
  save(file = here("data/ch3/preprocessed/merge_base.rda"))
