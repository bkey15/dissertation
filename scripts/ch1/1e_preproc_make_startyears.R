# Create modified datasets reflecting different "start years"

# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch1/preprocessed/ptas_final.rda"))

# make modified datasets ----
## 1968 (1st ratified hra) ----
### note: selecting 1949 (1st inforce pta) as start year yields same results, b/c all countries have NAs on hras pre-1968 (am not imputing b/c no country could have logically received "treatment" prior to that point), hence why I don't provide the code for it
ptas_final |> 
  summarize(
    min = min(year),
    .by = hras
    )

ptas_1968 <- ptas_final |> 
  filter(year > 1967) |> 
  mutate(
    across(
      c(7:18, starts_with("ns_")),
      ~ if_else(
        is.na(.x), 0, .x
        )
      ),
    across(
      starts_with("ss_"),
      ~ if_else(
        glb_s == 1 & is.na(.x), 0, .x)
      ),
    across(
      starts_with("nn_"),
      ~ if_else(
        glb_s == 0 & is.na(.x), 0, .x)
      )
    )

 ## 1977 (spilker & böhmelt) ----
ptas_1977 <- ptas_final |> 
  filter(year > 1976) |> 
  mutate(
    across(
      c(7:18, starts_with("ns_")),
      ~ if_else(
        is.na(.x), 0, .x
      )
    ),
    across(
      starts_with("ss_"),
      ~ if_else(
        glb_s == 1 & is.na(.x), 0, .x)
    ),
    across(
      starts_with("nn_"),
      ~ if_else(
        glb_s == 0 & is.na(.x), 0, .x)
    )
  )

# drop high miss states ----
# note: these countries are Kosovo, Taiwan, S. Vietnam, S. Yemen. Their missing values can't be imputed b/c there's too much missingness across key variables, particularly ones from the UN/World Bank (hras, wdi_trade, etc., b/c these were generally partially recognized states w/o organizational membership). See notes for more.
ptas_1968 <- ptas_1968 |> 
  filter(
    cow != 347,
    cow != 680,
    cow != 713,
    cow != 817
  )

ptas_1977 <- ptas_1977 |> 
  filter(
    cow != 347,
    cow != 680,
    cow != 713,
    cow != 817
  )

## save ----
ptas_1968 |> 
  save(file = here("data/ch1/preprocessed/ptas_1968.rda"))

ptas_1977 |> 
  save(file = here("data/ch1/preprocessed/ptas_1977.rda"))
