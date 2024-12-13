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
      c(6:13, starts_with("ns_")),
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
      c(6:13, starts_with("ns_")),
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

## save ----
ptas_1968 |> 
  save(file = here("data/ch1/preprocessed/ptas_1968.rda"))

ptas_1977 |> 
  save(file = here("data/ch1/preprocessed/ptas_1977.rda"))
