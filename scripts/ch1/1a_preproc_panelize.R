# load packages ----
library(tidyverse)
library(here)
library(readxl)
library(countrycode)
library(states)

# load data ----
## desta ----
desta_dyads <- read_xlsx(
  path = "data/ch1/raw/desta/desta_list_of_treaties_02_02_dyads.xlsx",
  sheet = 2
  )

## lechner ----
lechner <- read_delim(
  file = "data/ch1/raw/lechner_data/nti_201711.txt"
  )

## withdraw ----
### assuming withdrawals take place, w/ immediate effect, ignoring NA cases in entryforceyear
withdraw <- read_xlsx(
  path = "data/ch1/raw/desta/desta_dyadic_withdrawal_02_02.xlsx",
  sheet = 2
  ) |> 
  select(1:17) |> 
  mutate(exitforceyear = year) |> 
  select(
    -c(entryforceyear, year)
    )

## hras ----
cat <- read_xls(
  path = "data/ch1/raw/un/UnderlyingData_CAT_OHCHR_26_11_2024.xls",
  skip = 1
  ) |> 
  janitor::clean_names()

iccpr <- read_xls(
  path = "data/ch1/raw/un/UnderlyingData_ICCPR_OHCHR_26_11_2024.xls",
  skip = 1
  ) |> 
  janitor::clean_names()

## bop ----
bop <- read_csv("data/common/raw/imf/imf_bop_small.csv")

# panelize ptas data ----
## merge ptas datasets ----
### desta_dyads & lechner
ptas <- desta_dyads |> 
  inner_join(
    lechner,
    by = c("base_treaty" = "number")
    ) |> 
  select(
    !ends_with(".y")
    ) |> 
  rename(
    name = name.x,
    year = year.x
    ) |> 
  relocate(base_treaty)

#### test success; should only retain the ptas in Lechner's dataset
desta_dyads |> 
  filter(base_treaty %in% lechner$number) |> 
  summarize(n = n())

### ptas & withdraw
#### merge to use exityear vals as guide for next step
ptas <- ptas |> 
  full_join(withdraw) |> 
  relocate(
    exitforceyear,
    .after = entryforceyear
    ) |> 
  select(-coded)

#### manually create exityear vals for original rows in ptas
unique(withdraw$base_treaty)

ptas <- ptas |> 
  mutate(
    exitforceyear = case_when(
      base_treaty == 8 & iso2 == 826 ~ 2020,
      base_treaty == 17 & iso2 == 826 ~ 2020,
      base_treaty == 22 & iso2 == 862 ~ 2006,
      base_treaty == 26 & iso2 == 862 ~ 2006,
      base_treaty == 28 & iso2 == 826 ~ 2020,
      base_treaty == 130 & iso2 == 826 ~ 2020,
      base_treaty == 149 & iso1 == 40 ~ 1994,
      base_treaty == 149 & iso2 == 246 ~ 1994,
      base_treaty == 149 & iso2 == 752 ~ 1994,
      base_treaty == 181 & iso2 == 826 ~ 2020,
      base_treaty == 192 & iso1 == 203 ~ 2004,
      base_treaty == 192 & iso2 == 203 ~ 2004,
      base_treaty == 192 & iso1 == 348 ~ 2004,
      base_treaty == 192 & iso2 == 348 ~ 2004,
      base_treaty == 192 & iso1 == 616 ~ 2004,
      base_treaty == 192 & iso2 == 616 ~ 2004,
      base_treaty == 192 & iso1 == 703 ~ 2004,
      base_treaty == 192 & iso2 == 703 ~ 2004,
      base_treaty == 192 & iso1 == 705 ~ 2004,
      base_treaty == 192 & iso2 == 705 ~ 2004,
      base_treaty == 192 & iso1 == 100 ~ 2007,
      base_treaty == 192 & iso2 == 100 ~ 2007,
      base_treaty == 192 & iso1 == 642 ~ 2007,
      base_treaty == 192 & iso2 == 642 ~ 2007,
      base_treaty == 192 & iso1 == 191 ~ 2013,
      base_treaty == 192 & iso2 == 191 ~ 2013,
      base_treaty == 202 & iso2 == 826 ~ 2020,
      base_treaty == 243 & iso1 == 426 ~ 1997,
      base_treaty == 243 & iso2 == 426 ~ 1997,
      base_treaty == 243 & iso1 == 508 ~ 1997,
      base_treaty == 243 & iso2 == 508 ~ 1997,
      base_treaty == 243 & iso1 == 516 ~ 2000,
      base_treaty == 243 & iso2 == 516 ~ 2000,
      base_treaty == 243 & iso1 == 834 ~ 2004,
      base_treaty == 243 & iso2 == 834 ~ 2004,
      base_treaty == 243 & iso1 == 24 ~ 2007,
      base_treaty == 243 & iso2 == 24 ~ 2007,
      base_treaty == 244 & iso1 == 268 ~ 2009,
      base_treaty == 244 & iso2 == 268 ~ 2009,
      base_treaty == 252 & iso2 == 826 ~ 2020,
      base_treaty == 304 & iso2 == 826 ~ 2020,
      base_treaty == 307 & iso2 == 826 ~ 2020,
      base_treaty == 316 & iso2 == 826 ~ 2020,
      base_treaty == 318 & iso2 == 826 ~ 2020,
      base_treaty == 323 & iso2 == 826 ~ 2020,
      base_treaty == 323 & iso2 == 826 ~ 2020,
      base_treaty == 324 & iso2 == 826 ~ 2020,
      base_treaty == 328 & iso2 == 826 ~ 2020,
      base_treaty == 330 & iso2 == 826 ~ 2020,
      base_treaty == 331 & iso2 == 826 ~ 2020,
      base_treaty == 334 & iso2 == 826 ~ 2020,
      base_treaty == 341 & iso2 == 826 ~ 2020,
      base_treaty == 342 & iso2 == 826 ~ 2020,
      base_treaty == 347 & iso2 == 826 ~ 2020,
      base_treaty == 350 & iso2 == 826 ~ 2020,
      base_treaty == 351 & iso2 == 826 ~ 2020,
      base_treaty == 354 & iso2 == 826 ~ 2020,
      base_treaty == 355 & iso2 == 826 ~ 2020,
      base_treaty == 356 & iso2 == 826 ~ 2020,
      base_treaty == 357 & iso2 == 826 ~ 2020,
      base_treaty == 363 & iso2 == 646 ~ 2007,
      base_treaty == 367 & iso1 == 478 ~ 2000,
      base_treaty == 367 & iso2 == 478 ~ 2000,
      base_treaty == 375 & iso1 == 826 ~ 1973,
      base_treaty == 375 & iso2 == 826 ~ 1973,
      base_treaty == 375 & iso1 == 208 ~ 1973,
      base_treaty == 375 & iso2 == 208 ~ 1973,
      base_treaty == 375 & iso1 == 620 ~ 1986,
      base_treaty == 375 & iso2 == 620 ~ 1986,
      base_treaty == 375 & iso1 == 40 ~ 1995,
      base_treaty == 375 & iso2 == 40 ~ 1995,
      base_treaty == 375 & iso1 == 246 ~ 1995,
      base_treaty == 375 & iso2 == 246 ~ 1995,
      base_treaty == 375 & iso1 == 752 ~ 1995,
      base_treaty == 375 & iso2 == 752 ~ 1995,
      base_treaty == 380 & iso1 == 40 ~ 1994,
      base_treaty == 380 & iso2 == 40 ~ 1994,
      base_treaty == 380 & iso1 == 246 ~ 1994,
      base_treaty == 380 & iso2 == 246 ~ 1994,
      base_treaty == 380 & iso1 == 752 ~ 1994,
      base_treaty == 380 & iso2 == 752 ~ 1994,
      base_treaty == 382 & iso1 == 40 ~ 1994,
      base_treaty == 382 & iso2 == 40 ~ 1994,
      base_treaty == 382 & iso1 == 246 ~ 1994,
      base_treaty == 382 & iso2 == 246 ~ 1994,
      base_treaty == 382 & iso1 == 752 ~ 1994,
      base_treaty == 382 & iso2 == 752 ~ 1994,
      base_treaty == 392 & iso1 == 40 ~ 1994,
      base_treaty == 392 & iso2 == 40 ~ 1994,
      base_treaty == 392 & iso1 == 246 ~ 1994,
      base_treaty == 392 & iso2 == 246 ~ 1994,
      base_treaty == 392 & iso1 == 752 ~ 1994,
      base_treaty == 392 & iso2 == 752 ~ 1994,
      base_treaty == 393 & iso1 == 40 ~ 1994,
      base_treaty == 393 & iso2 == 40 ~ 1994,
      base_treaty == 393 & iso1 == 246 ~ 1994,
      base_treaty == 393 & iso2 == 246 ~ 1994,
      base_treaty == 393 & iso1 == 752 ~ 1994,
      base_treaty == 393 & iso2 == 752 ~ 1994,
      base_treaty == 402 & iso1 == 40 ~ 1994,
      base_treaty == 402 & iso2 == 40 ~ 1994,
      base_treaty == 402 & iso1 == 246 ~ 1994,
      base_treaty == 402 & iso2 == 246 ~ 1994,
      base_treaty == 402 & iso1 == 752 ~ 1994,
      base_treaty == 402 & iso2 == 752 ~ 1994,
      base_treaty == 434 & iso2 == 860 ~ 2008,
      base_treaty == 461 & iso1 == 642 ~ 2007,
      base_treaty == 461 & iso2 == 642 ~ 2007,
      base_treaty == 464 & iso2 == 862 ~ 2006,
      base_treaty == 465 & iso2 == 862 ~ 2006,
      base_treaty == 604 & iso2 == 862 ~ 2017,
      base_treaty == 647 & iso1 == 300 ~ 1981,
      base_treaty == 647 & iso2 == 300 ~ 1981,
      base_treaty == 647 & iso1 == 724 ~ 1987,
      base_treaty == 647 & iso2 == 724 ~ 1987,
      base_treaty == 672 & iso1 == 690 ~ 2004,
      base_treaty == 672 & iso2 == 690 ~ 2004,
      base_treaty == 693 & iso1 == 862 ~ 2006,
      base_treaty == 793 & iso2 == 826 ~ 2020,
      base_treaty == 799 & iso2 == 826 ~ 2020,
      base_treaty == 810 & iso2 == 826 ~ 2020,
      base_treaty == 815 & iso2 == 826 ~ 2020,
      base_treaty == 848 & iso2 == 826 ~ 2020,
      base_treaty == 866 & iso2 == 826 ~ 2020,
      base_treaty == 867 & iso1 == 826 ~ 2020,
      base_treaty == 869 & iso1 == 826 ~ 2020,
      base_treaty == 872 & iso2 == 826 ~ 2020,
      base_treaty == 874 & iso2 == 826 ~ 2020,
      base_treaty == 876 & iso2 == 826 ~ 2020,
      base_treaty == 877 & iso1 == 826 ~ 2020,
      base_treaty == 908 & iso2 == 862 ~ 2006,
      base_treaty == 975 & iso1 == 826 ~ 2020,
      base_treaty == 978 & iso1 == 826 ~ 2020,
      base_treaty == 979 & iso1 == 826 ~ 2020,
      base_treaty == 997 & iso1 == 826 ~ 2020,
      .default = exitforceyear
      )
    ) |> 
  filter(
    entry_type != "consolidated",
    entry_type != "withdrawal",
    !is.na(entryforceyear)
    )

## lengthen ptas data ----
### create row for each pta per year in HR scores
years_hrs <- tibble(year = 1946:2021)

ptas_long <- ptas |> 
  select(-year) |> 
  cross_join(years_hrs) |> 
  relocate(
    year,
    entryforceyear,
    exitforceyear
    )

### create indicator for whether pta is "in force" for the year at issue; standardize Lechner vars by this indicator, such that scores > 0 only appear during in-force years
ptas_long <- ptas_long |> 
  mutate(
    inforce = case_when(
      year < entryforceyear ~ 0,
      year >= entryforceyear & is.na(exitforceyear) ~ 1,
      year >= entryforceyear & year < exitforceyear ~ 1,
      year >= exitforceyear ~ 0
      ),
    across(18:23, ~ .x*inforce)
    )

### create country-code var for each partner
ptas_long <- ptas_long |> 
  mutate(
    country_a = paste(country1, iso1, sep = "_"),
    country_b = paste(country2, iso2, sep = "_")
    )

### create initial panel (will contain many country-year duplicates); separate country-code vars and create IDs for every "main" country and partner per treaty-year
ptas_panel <- ptas_long |> 
  pivot_longer(
    cols = c(country_a, country_b),
    values_to = "country",
    names_repair = "minimal"
    ) |> 
  select(-name) |> 
  separate_wider_delim(
    country,
    delim = "_",
    names = c("country", "iso")
    ) |> 
  mutate(
    iso = as.numeric(iso)
    ) |> 
  relocate(country, iso)

ptas_panel <- ptas_panel |> 
  mutate(
    country1 = if_else(
      country == country1,
      NA,
      country1
      ),
    country2 = if_else(
      country == country2,
      NA,
      country2
      ),
    partner = if_else(
      is.na(country1),
      country2,
      country1
      ),
    iso1 = if_else(
      iso == iso1,
      NA,
      iso1
      ),
    iso2 = if_else(
      iso == iso2,
      NA,
      iso2
      ),
    iso_partner = if_else(
      is.na(iso1),
      iso2,
      iso1
      )
    ) |> 
  select(-country1, -country2, -iso1, -iso2) |> 
  relocate(partner, iso_partner, .after = iso) |> 
  arrange(iso)

### select only essential vars
ptas_panel <- ptas_panel |> 
  select(1:5, 8, 17:19, 23)

### convert ISO codes to COW codes
ptas_panel <- ptas_panel |> 
  mutate(
    cow = countrycode(
      sourcevar = iso,
      origin = "iso3n",
      destination = "cown"
      ),
    cow = case_when(
      iso == 688 ~ 345,
      iso == 900 ~ 347,
      .default = cow
      ),
    cow_partner = countrycode(
      sourcevar = iso_partner,
      origin = "iso3n",
      destination = "cown"
      ),
    cow_partner = case_when(
      iso_partner == 688 ~ 345,
      iso_partner == 900 ~ 347,
      .default = cow_partner
      )
    ) |> 
  relocate(cow, .after = country) |> 
  relocate(cow_partner, .after = partner)

### remove obs where cow or cow_partner is NA (HR scores only has COW countries; most of these aren't in V-Dem either; trying to prevent unnecessary duplicates at next step)
ptas_panel <- ptas_panel |> 
  filter(
    !is.na(cow) & !is.na(cow_partner)
    )

## save ----
ptas_panel |> 
  save(
    file = here("data/ch1/preprocessed/ptas_panel.rda")
  )

# panelize hras data ----
## load cowstates (from `states`) & years_hrs
cowstates <- cowstates |> 
  rename(
    cow = cowcode,
    country = country_name
    ) |> 
  select(cow, country) |> 
  distinct()

years_hrs <- tibble(year = 1946:2021)

## cat ----
### check for failed merger points
cat_fail <- cat |> 
  rename(rat_year = date_of_ratification_accession) |> 
  select(country, rat_year) |> 
  mutate(rat_year = year(rat_year)) |> 
  full_join(cowstates) |> 
  arrange(country)

### fix
cat_fix <- cat_fail |> 
  mutate(
    rat_year = case_when(
      cow == 58 ~ 1993,
      cow == 145 ~ 1999,
      cow == 402 ~ 1992,
      cow == 437 ~ 1995,
      cow == 316 ~ 1988,
      cow == 860 ~ 2003,
      cow == 265 ~ 1987,
      cow == 572 ~ 2004,
      cow == 812 ~ 2012,
      cow == 343 ~ 1994,
      cow == 359 ~ 1995,
      cow == 365 ~ 1987,
      cow == 60 ~ 2020,
      cow == 57 ~ 2001,
      cow == 345 ~ 1991,
      cow == 652 ~ 2004,
      cow == 640 ~ 1988,
      cow == 200 ~ 1988,
      cow == 101 ~ 1991,
      cow == 816 ~ 2015,
      .default = rat_year
      )
    ) |> 
  filter(
    cow != 300,
    cow != 267,
    cow != 245,
    cow != 315,
    cow != 260,
    cow != 240,
    cow != 273,
    cow != 275,
    cow != 730,
    cow != 347,
    cow != 280,
    cow != 332,
    cow != 327,
    cow != 335,
    cow != 817,
    cow != 269,
    cow != 713,
    cow != 337,
    cow != 947,
    cow != 329,
    cow != 271,
    cow != 678,
    cow != 680,
    cow != 511
    ) |> 
  relocate(cow, .after = country)

### check
cat_og <- cat |> 
  rename(rat_year = date_of_ratification_accession) |> 
  select(country, rat_year) |> 
  mutate(rat_year = year(rat_year))

diff_check1 <- cat_fix |> 
  select(-cow) |> 
  setdiff(cat_og)

### create ratification indicator (first states ratified in 1986)
cat_rat <- cat_fix |> 
  cross_join(years_hrs) |> 
  mutate(
    cat_rat = case_when(
      year < 1986 ~ NA,
      year < rat_year ~ 0,
      year >= rat_year ~ 1,
      is.na(rat_year) ~ 0
      )
    ) |> 
  select(cow, year, cat_rat) |> 
  arrange(cow, year)

## iccpr ----
### check for failed merger points
iccpr_fail <- iccpr |> 
  rename(rat_year = date_of_ratification_accession) |> 
  select(country, rat_year) |> 
  mutate(rat_year = year(rat_year)) |> 
  full_join(cowstates) |> 
  arrange(country)

### fix
iccpr_fix <- iccpr_fail |> 
  mutate(
    rat_year = case_when(
      cow == 58 ~ 2019,
      cow == 145 ~ 1982,
      cow == 402 ~ 1993,
      cow == 437 ~ 1992,
      cow == 316 ~ 1975,
      cow == 860 ~ 2003,
      cow == 265 ~ 1973,
      cow == 572 ~ 2004,
      cow == 812 ~ 2009,
      cow == 343 ~ 1994,
      cow == 359 ~ 1993,
      cow == 365 ~ 1973,
      cow == 57 ~ 1981,
      cow == 345 ~ 1971,
      cow == 652 ~ 1969,
      cow == 640 ~ 2003,
      cow == 200 ~ 1976,
      cow == 101 ~ 1978,
      cow == 816 ~ 1982,
      .default = rat_year
      )
    ) |> 
  filter(
    cow != 300,
    cow != 267,
    cow != 245,
    cow != 315,
    cow != 260,
    cow != 240,
    cow != 273,
    cow != 275,
    cow != 730,
    cow != 347,
    cow != 280,
    cow != 332,
    cow != 327,
    cow != 335,
    cow != 817,
    cow != 269,
    cow != 713,
    cow != 337,
    cow != 947,
    cow != 329,
    cow != 271,
    cow != 678,
    cow != 680,
    cow != 511
    )

### check
iccpr_og <- iccpr |> 
  rename(rat_year = date_of_ratification_accession) |> 
  select(country, rat_year) |> 
  mutate(rat_year = year(rat_year))

diff_check2 <- iccpr_fix |> 
  select(-cow) |> 
  setdiff(iccpr_og)

### note: countries in the 2 diff-checks should be the same

### create ratification indicator (first states ratified in 1968)
iccpr_rat <- iccpr_fix |> 
  cross_join(years_hrs) |> 
  mutate(
    iccpr_rat = case_when(
      year < 1968 ~ NA,
      year < rat_year ~ 0,
      year >= rat_year ~ 1,
      is.na(rat_year) ~ 0
      )
    )  |> 
  select(cow, year, iccpr_rat) |> 
  arrange(cow, year)

## merge & save ----
hras <- cat_rat |> 
  left_join(iccpr_rat)

hras |> 
  save(
    file = here("data/ch1/preprocessed/hras.rda")
    )

# panelize bop data ----
## shrink ----
### note: not cleaning names with janitor::clean_names b/c it's producing "x" before year column names
bop_small <- bop |> 
  select(
    !c(ends_with("Q1") | ends_with("Q2") | ends_with("Q3") | ends_with("Q4"))
    ) |> 
  select(1:2, 9:84) |> 
  rename(
    economy_iso3 = "Economy ISO3",
    economy_iso4 = "Economy ISO4",
    )

## convert ISO codes to COW codes ----
bop_small <- bop_small |> 
  mutate(
    cow = countrycode(
      sourcevar = economy_iso3,
      origin = "iso3c",
      destination = "cown"
      ),
    cow = case_when(
      economy_iso3 == "SRB" ~ 345,
      economy_iso3 == "XKX" ~ 347,
      .default = cow
      )
    ) |> 
  filter(!is.na(cow)) |> 
  relocate(cow, .after = economy_iso4)

## panelize ----
bop_panel <- bop_small |> 
  pivot_longer(
    cols = 4:79,
    names_to = "year",
    values_to = "bop_raw"
    ) |> 
  mutate(year = as.numeric(year)) |> 
  select(cow, year, bop_raw) |> 
  arrange(cow, year)

## save ----
bop_panel |> 
  save(
    file = here("data/ch1/preprocessed/bop_panel.rda")
    )
