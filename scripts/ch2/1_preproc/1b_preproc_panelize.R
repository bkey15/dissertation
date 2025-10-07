# load packages ----
library(tidyverse)
library(here)
library(countrycode)
library(states)

# load data ----
load(here("data/ch2/raw/bits_raw.rda"))

# create dyads ----
## filter to BITs ----
bits <- bits_raw |> 
  filter(type == "BITs") |> 
  select(
    treaty_id,
    short_title,
    status,
    date_of_entry_into_force,
    termination_date
    )

## separate country names ----
### prep ----
bits <- bits |> 
  mutate(
    short_title = case_when(
      short_title == "China-Tajikistan BIT (2024)" ~ "China - Tajikistan BIT (2024)",
      short_title == "Kazakhstan-Kyrgyzstan BIT (2024)" ~ "Kazakhstan - Kyrgyzstan BIT (2024)",
      short_title == "Cambodia-Russian Federation BIT (2015)" ~ "Cambodia - Russian Federation BIT (2015)",
      short_title == "Cabo Verde - Equatorial Guinea  (2019)" ~ "Cabo Verde - Equatorial Guinea BIT (2019)",
      short_title == "Slovakia - United Arab Emirates (2016)" ~ "Slovakia - United Arab Emirates BIT (2016)",
      short_title == "India - North Macedonia (2008)" ~ "India - North Macedonia BIT (2008)",
      short_title == "Gabon - Korea, Republic of (2007)" ~ "Gabon - Korea, Republic of BIT (2007)",
      short_title == "Mali - South Africa BIT" ~ "Mali - South Africa BIT (1995)",
      .default = short_title
      )
    )

### separate ----
bits <- bits |> 
  separate_wider_delim(
    short_title,
    delim = " - ",
    names = c("country1", "country2")
    ) |> 
  separate_wider_delim(
    country2,
    delim = " BIT (",
    names = c("country2", "junk")
    ) |> 
  select(-junk)

## get COW codes ----
bits <- bits |> 
  mutate(
    cow1 = countrycode(
      sourcevar = country1,
      origin = "country.name",
      destination = "cown"
      ),
    cow2 = countrycode(
      sourcevar = country2,
      origin = "country.name",
      destination = "cown"
      ),
    cow1 = case_when(
      country1 == "Serbia" ~ 345,
      country1 == "BLEU (Belgium-Luxembourg Economic Union)" | country1 == "Belgium/Luxembourg" ~ 211,
      .default = cow1
      ),
    cow2 = case_when(
      country2 == "Serbia" ~ 345,
      country2 == "BLEU (Belgium-Luxembourg Economic Union)" | country2 == "Belgium/Luxembourg" ~ 211,
      .default = cow2
      ),
    ) |> 
  relocate(starts_with("cow"), .after = country2)

### create Luxembourg rows
bleu <- bits |> 
  filter(
    cow1 == 211 | cow2 == 211
    ) |> 
  mutate(
    cow1 = if_else(
      country1 == "BLEU (Belgium-Luxembourg Economic Union)" | country1 == "Belgium/Luxembourg", 212, cow1
      ),
    cow2 = if_else(
      country2 == "BLEU (Belgium-Luxembourg Economic Union)" | country2 == "Belgium/Luxembourg", 212, cow2
      )
    ) |> 
  filter(cow1 != 211)

### add Luxembourg rows, delete missing COW rows
bits <- bits |> 
  rbind(bleu) |> 
  filter(
    !is.na(cow1),
    !is.na(cow2)
    ) |> 
  mutate(
    country1 = case_when(
      cow1 == 211 ~ "Belgium",
      cow1 == 212 ~ "Luxembourg",
      .default = country1
      ),
    country2 = case_when(
      cow2 == 211 ~ "Belgium",
      cow2 == 212 ~ "Luxembourg",
      .default = country2
      )
    )

## lengthen BITs data ----
### create row for each BIT per year in HR scores
years_hrs <- tibble(year = 1946:2021)

bits_long <- bits |> 
  cross_join(years_hrs) |> 
  relocate(
    year,
    date_of_entry_into_force,
    termination_date,
    status
    )

### convert dates to years
bits_long <- bits_long |> 
  mutate(
    across(
      contains("date"),
      ~ year(.x)
      )
    )

### create BIT "in force" indicator
#### note: in case_when(.default = 0), BITs that have a "terminated" status but not an in-force date are assigned a value of 0 for all observations. This applies to 40-some BITs.
bits_long <- bits_long |> 
  mutate(
    inforce = case_when(
      status == "Signed (not in force)" ~ 0,
      year < date_of_entry_into_force ~ 0,
      year >= date_of_entry_into_force & status == "In force" ~ 1,
      year >= date_of_entry_into_force & year < termination_date ~ 1,
      year >= termination_date ~ 0,
      .default = 0
    )
  )

### create country-code var for each partner
bits_long <- bits_long |> 
  mutate(
    country_a = paste(country1, cow1, sep = "_"),
    country_b = paste(country2, cow2, sep = "_")
  )

### create initial panel (will contain many country-year duplicates); separate country-code vars and create IDs for every "main" country and partner per treaty-year
bits_panel <- bits_long |> 
  pivot_longer(
    cols = c(country_a, country_b),
    values_to = "country",
    names_repair = "minimal"
    ) |> 
  select(-name) |> 
  separate_wider_delim(
    country,
    delim = "_",
    names = c("country", "cow")
    ) |> 
  mutate(
    cow = as.numeric(cow)
    ) |> 
  relocate(country, cow)

bits_panel <- bits_panel |> 
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
    cow1 = if_else(
      cow == cow1,
      NA,
      cow1
      ),
    cow2 = if_else(
      cow == cow2,
      NA,
      cow2
      ),
    cow_partner = if_else(
      is.na(cow1),
      cow2,
      cow1
      )
    ) |> 
  select(-country1, -country2, -cow1, -cow2) |> 
  relocate(partner, cow_partner, .after = cow) |> 
  arrange(cow)

### select only essential vars
bits_panel <- bits_panel |> 
  select(1:5, treaty_id, inforce)

## save ----
bits_panel |> 
  save(
    file = here("data/ch2/preprocessed/bits_panel.rda")
    )
