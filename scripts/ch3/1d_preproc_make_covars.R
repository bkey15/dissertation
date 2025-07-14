# load packages ----
library(tidyverse)
library(here)
library(readxl)
library(countrycode)

# load data ----
load(here("data/ch1/preprocessed/ptas_panel.rda"))
load(here("data/ch3/preprocessed/merge_base.rda"))

desta_dyads <- read_xlsx(
  path = "data/ch1/raw/desta/desta_list_of_treaties_02_02_dyads.xlsx",
  sheet = 2
  )
load(here("data/ch2/raw/bits_raw.rda"))

# make vars ----
## controversial election indicator ----
ems_final <- merge_base |> 
  mutate(
    cont_elect = if_else(
      v2elwestmon == 1 | v2elmonden == 1 | v2elmonref == 1,
      1,
      0
      ),
    cont_elect = if_else(is.na(cont_elect), 0, cont_elect)
    ) |> 
  select(-c(v2elwestmon, v2elmonden, v2elmonref)) |> 
  relocate(cont_elect, .after = coup_success)


## pta_west ----
### get cow codes & filter to US/EU ptas
desta_dyads <- desta_dyads |> 
  mutate(
    cow1 = countrycode(
      sourcevar = iso1,
      origin = "iso3n",
      destination = "cown"
      ),
    cow2 = countrycode(
      sourcevar = iso2,
      origin = "iso3n",
      destination = "cown"
      )
    ) |> 
  filter(
    str_detect(name, "\\bEC\\b") | cow1 == 2 | cow2 == 2
    )

### fix cow codes
desta_dyads <- desta_dyads |> 
  mutate(
    cow1 = case_when(
      iso1 == 688 ~ 345,
      iso1 == 900 ~ 347,
      .default = cow1
      ),
    cow2 = case_when(
      iso2 == 688 ~ 345,
      iso2 == 900 ~ 347,
      .default = cow2
      )
    ) |> 
  select(-c(iso1, iso2)) |> 
  relocate(c(cow1, cow2), .after = country2)

### filter to ptas only involving merge_base countries
ch3_cows <- unique(merge_base$cow)

desta_dyads <- desta_dyads |> 
  filter(cow1 %in% ch3_cows | cow2 %in% ch3_cows)

### filter ptas_panel to base treaties only in desta_dyads
ch3_base_trts <- unique(desta_dyads$base_treaty)

ptas_panel <- ptas_panel |> 
  filter(base_treaty %in% ch3_base_trts)

### filter cow to ch3_cows; remove (most) duplicates
#### note: some duplicates will remain, where treaty was signed prior to some EU countries acceding to union (hence why some country-years will have both inforce == 0 & inforce == 1 for same base_treaty). We'll fix this in summarize() step.
ptas_panel <- ptas_panel |> 
  filter(
    cow %in% ch3_cows,
    year > 1989 & year < 2019
    ) |> 
  select(cow, year, base_treaty, inforce) |> 
  distinct() |> 
  arrange(cow, year)

### count inforce ptas
ptas_panel <- ptas_panel |> 
  summarize(
    n_ptas_west = sum(inforce),
    .by = c(cow, year)
    )

### create pta_west indicator
ptas_panel <- ptas_panel |> 
  mutate(
    pta_west = if_else(
      n_ptas_west > 0, 1, 0
      )
    ) |> 
  select(-n_ptas_west)

### merge & fix missingness
ems_final <- ems_final |> 
  left_join(ptas_panel) |> 
  mutate(
    pta_west = if_else(
      is.na(pta_west), 0, pta_west
      )
    )

## bit_tip_west ----
### filter to US/EU bits/tips (& inforce pre-2019)
bits_raw <- bits_raw |> 
  select(
    treaty_id,
    short_title,
    type,
    status,
    parties,
    date_of_entry_into_force,
    termination_date
    ) |> 
  mutate(
    across(
      contains("date"),
      ~ year(.x)
      )
    ) |> 
  filter(
    str_detect(parties, "\\bEuropean Union\\b") | str_detect(parties, "\\bUnited States of America\\b"),
    date_of_entry_into_force < 2019
    )

### separate "country" names
#### separate
bits_raw <- bits_raw |> 
  separate_wider_delim(
    short_title,
    delim = " - ",
    names = c("country1", "country2"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  separate_wider_delim(
    country2,
    delim = " BIT (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " TIFA (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
    select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " ATEC (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |>
  separate_wider_delim(
    country2,
    delim = " TPA (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " EPA (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |>
  separate_wider_delim(
    country2,
    delim = " FTA (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |>
  separate_wider_delim(
    country2,
    delim = " PCA (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |>
  separate_wider_delim(
    country2,
    delim = " Association Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " Trade Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " Cooperation Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " Framework Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |>
  separate_wider_delim(
    country2,
    delim = " Stabilization Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |>
  separate_wider_delim(
    country2,
    delim = " Partnership Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |>
  separate_wider_delim(
    country2,
    delim = " Trade Relations Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " Association (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " Investment Development Agreement (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " Interim",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " Trade",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " (",
    names = c("country2", "junk"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  select(-junk) |> 
  separate_wider_delim(
    country2,
    delim = " - ",
    names = c("country2", "country3"),
    too_few = "align_start",
    too_many = "merge"
    ) |> 
  separate_wider_delim(
    country3,
    delim = " - ",
    names = c("country3", "country4"),
    too_few = "align_start",
    too_many = "merge"
    )

### lengthen, filter, get cow codes
bits_raw <- bits_raw |> 
  pivot_longer(
    cols = c(
      country1,
      country2,
      country3,
      country4
      ),
    values_to = "signatory"
    ) |> 
  filter(
    !is.na(signatory),
    str_detect(signatory, "^EU", negate = TRUE),
    signatory != "EC",
    signatory != "EEC",
    str_detect(signatory, "^US", negate = TRUE),
    str_detect(signatory, "^United", negate = TRUE)
    ) |> 
  mutate(
    cow = countrycode(
      sourcevar = signatory,
      origin = "country.name",
      destination = "cown"
      )
    ) |> 
  select(-c(type, name))

### fix cow codes; filter out NAs
#### note: fixing only where signatory is unrepresented through other international organization
bits_raw <- bits_raw |> 
  mutate(
    cow = case_when(
      signatory == "Serbia" ~ 345,
      signatory == "DR" ~ 42,
      signatory == "Ankara Agreement (1995)" ~ 640,
      signatory == "NAFTA (1992)" ~ 70,
      .default = cow
      )
    ) |> 
  filter(!is.na(cow)) |> 
  select(-c(parties, signatory))

### panelize
years_ist <- tibble(year = 1990:2018)

bits_panel <- bits_raw |> 
  cross_join(years_ist) |> 
  relocate(cow, year) |> 
  arrange(cow, year)

### create inforce indicator
#### note: in the ch. 2 code, BITs that have a "terminated" status but not an in-force date are assigned a value of 0 for all observations via case_when(.default = 0) (this applies to 40-some BITs in the full dataset). The code is included below for the sake of possible future reproducibility.
bits_panel <- bits_panel |> 
  mutate(
    inforce = case_when(
      status == "Signed (not in force)" ~ 0,
      year < date_of_entry_into_force ~ 0,
      year >= date_of_entry_into_force & status == "In force" ~ 1,
      year >= date_of_entry_into_force & year < termination_date ~ 1,
      year >= termination_date ~ 0,
      .default = 0
    )
  ) |> 
  select(-status, -contains("date"))

### count inforce bits
bits_panel <- bits_panel |> 
  summarize(
    n_bits_tips_west = sum(inforce),
    .by = c(cow, year)
    )

### create bit_tip_west indicator
bits_panel <- bits_panel |> 
  mutate(
    bit_tip_west = if_else(
      n_bits_tips_west > 0, 1, 0
      )
    ) |> 
  select(-n_bits_tips_west)

### merge & fix missingness
ems_final <- ems_final |> 
  left_join(bits_panel) |> 
  mutate(
    bit_tip_west = if_else(
      is.na(bit_tip_west), 0, bit_tip_west
      )
    )

# fix vars ----
## n_ems, any_inforce, gov_kill, coup_success ----
ems_final <- ems_final |> 
  mutate(
    n_ems = if_else(
      is.na(n_ems), 0, n_ems
      ),
    any_inforce = if_else(
      is.na(any_inforce), 0, any_inforce
      ),
    gov_kill = if_else(
      is.na(gov_kill), 0, gov_kill
      ),
    coup_success = if_else(
      is.na(coup_success), 0, coup_success
      )
    )

# save ----
ems_final |> 
  save(file = here("data/ch3/preprocessed/ems_final.rda"))
