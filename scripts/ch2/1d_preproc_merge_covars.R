# load packages ----
library(tidyverse)
library(here)
library(vdemdata)
library(naniar)
library(states)

# load data ----
## standardized bits data
load(here("data/ch2/preprocessed/bits_standard.rda"))

## standards (gdp, gdppc, pop)
load(here("data/ch1/preprocessed/standards.rda"))

## vdem
vdem <- vdem

## hr scores
load(here("data/common/raw/fariss/LHRS-v4.02-2021.Rdata"))
hrs <- data
rm(data)

## wdi
wdi <- read_csv(
  file = here("data/common/raw/qog/qog_bas_ts_jan24.csv")
  )

## bop
load(here("data/ch1/preprocessed/bop_panel.rda"))

# prep merge ----
## vdem ----
### note 1: use only high- and mid-level
### note 2: avoid v2x_clphy, v2cltort, v2clkill for now; too much missingness in the small countries
vdem_small <- vdem |> 
  select(1:117, v2cademmob, e_polity2) |> 
  filter(year > 1945) |> 
  rename(cow = COWcode) |> 
  relocate(cow, .before = year) |> 
  mutate(
    cow = case_when(
      cow == 260 ~ 255,
      cow == 315 ~ 316,
      cow == 678 ~ 679,
      .default = cow
      )
    ) |> 
  filter(!is.na(cow)) |> 
  select(
    country_name,
    cow,
    year,
    contains("v2x"),
    -ends_with("_codelow"),
    -ends_with("_codehigh"),
    -ends_with("_sd"),
    v2cademmob,
    e_polity2
    ) |> 
  arrange(cow, year)

## hr scores ----
hrs <- hrs |> 
  rename(
    year = YEAR,
    cow = COW,
    hr_score = theta_mean
    ) |> 
  filter(
    !(cow == 260 & year == 1990),
    !(cow == 679 & year == 1990)
    ) |> 
  mutate(
    cow = case_when(
      cow == 260 ~ 255,
      cow == 678 ~ 679,
      .default = cow
      )
    ) |> 
  select(
    cow,
    year,
    hr_score
    ) |> 
  arrange(cow, year)

## wdi ----
wdi_small <- wdi |> 
  rename(cow = ccodecow) |> 
  select(
    cname,
    cname_qog,
    cow,
    year,
    wdi_fdiin,
    wdi_fdiout,
    wdi_trade,
    wdi_popden,
    p_durable
    ) |> 
  arrange(cow, year)

wdi_small <- wdi_small |> 
  mutate(
    cow = case_when(
      cname_qog == "Chile" ~ 155,
      cname_qog == "France (-1962)" ~ 220,
      cname_qog == "Germany, West" ~ 255,
      cname_qog == "Serbia" & year > 1991 ~ 345,
      cname_qog == "Cyprus (-1974)" ~ 352,
      cname_qog == "Congo, Democratic Republic" ~ 490,
      cname_qog == "Ethiopia (-1992)" ~ 530,
      cname_qog == "Sudan (-2011)" ~ 625,
      cname_qog == "Syria" ~ 652,
      cname_qog == "Yemen, North" ~ 679,
      cname_qog == "Yemen, South" ~ 680,
      cname_qog == "Pakistan (-1970)" ~ 770,
      cname_qog == "Vietnam, North" ~ 816,
      cname_qog == "Vietnam, South" ~ 817,
      cname_qog == "Malaysia (-1965)" ~ 820,
      cname_qog == "Indonesia" ~ 850,
      .default = cow
      )
    ) |> 
  filter(cname_qog != "Serbia and Montenegro") |> 
  select(-cname, -cname_qog)

# merge ----
## vdem & hr scores
merge_base <- vdem_small |> 
  right_join(hrs) |> 
  relocate(hr_score, .after = year) |> 
  arrange(cow, year)

### check missings
missings <- merge_base |> 
  filter(is.na(country_name)) |> 
  distinct(cow) |> 
  inner_join(
    cowstates,
    by = c("cow" = "cowcode")
    )

### note: the following 21 countries are uncoded in vdem: Bahamas (31), Dominica (54), Grenada (55), St. Lucia (56), St. Vincent and the Grenadines (57), Antigua & Barbuda (58), St. Kitts and Nevis (60), Belize (80), Monaco (221), Liechtenstein (223), Andorra (232), San Marino (331), Brunei (835), Kiribati (946), Tuvalu (947), Tonga (955), Nauru (970), Marshall Islands (983), Palau (986), Federated States of Micronesia (987), Samoa (990). 18 of these are microstates. For now, we'll remove them from our analysis.

### fix overlap years & years w/o vdem data
merge_base <- merge_base |> 
  filter(
    !(cow == 255 & year < 1949),
    !(cow == 265 & year < 1949),
    !(cow == 317 & year < 1993),
    !(cow == 366 & year < 1990),
    !(cow == 367 & year < 1990),
    !(cow == 368 & year < 1990),
    !(cow == 370 & year < 1990),
    !(cow == 371 & year < 1990),
    !(cow == 373 & year < 1990),
    !(cow == 471 & year < 1961),
    !(cow == 31 | cow == 54 | cow == 55 | cow == 56 | cow == 57 | cow == 58 | cow == 60 | cow == 80 | cow == 221 | cow == 223 | cow == 232 | cow == 331 | cow == 835 | cow == 946 | cow == 947 | cow == 955 | cow == 970 | cow == 983 | cow == 986 | cow == 987 | cow == 990)
    )

## merge_base & wdi_small
merge_base <- merge_base |> 
  left_join(wdi_small)

## merge_base & standards
merge_base <- merge_base |> 
  left_join(standards)

## merge_base & bop_panel
merge_base <- merge_base |> 
  left_join(bop_panel)

## merge_base & ptas_standard
merge_base <- merge_base |> 
  left_join(bits_standard) |> 
  relocate(46:62, .after = hr_score)

# fix any_inforce ----
merge_base <- merge_base |> 
  mutate(
    any_inforce = if_else(
      is.na(any_inforce), 0, any_inforce
      )
    )

# fix glb_s ----
# Armenia (371) & Bhutan (760) have some missing vals b/c of mismatch between datasets as to when they start appearing. Also manually coding to South former countries GDR (265), S. Vietnam (817), Zanzibar (511), S. Yemen (680). These won't matter anyways b/c they'll be filtered out due to inability to impute all vars at next step.
merge_base <- merge_base |> 
  mutate(
    glb_s = case_when(
      cow == 265 ~ 1,
      cow == 371 ~ 1,
      cow == 511 ~ 1,
      cow == 680 ~ 1,
      cow == 760 ~ 1,
      cow == 817 ~ 1,
      .default = glb_s
      )
    )

# filter out 2020+ years ----
# IMPORTANT: doing this for now b/c treatments are NA (Fariss standards end in 2019); don't want to mistakenly fill them with a val of 0; also will aid in imputation computation time
merge_base <- merge_base |> 
  filter(year < 2020)

# save ----
merge_base |> 
  save(file = here("data/ch2/preprocessed/merge_base.rda"))
