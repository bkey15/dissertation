# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch3/preprocessed/ist_panel.rda"))

# interact measures w/ inforce ----
ist_panel <- ist_panel |> 
  mutate(
    across(
      starts_with("em_"),
      ~ inforce*.x
      )
    )

# standardize treatments ----
ems_standard <- ist_panel |> 
  summarize(
    n_ems = sum(c_across(starts_with("em_"))),
    .by = c(target, year)
    ) |> 
  mutate(
    any_inforce = if_else(
      n_ems > 0, 1, 0
      )
    )

# save ----
ems_standard |> 
  save(file = here("data/ch3/preprocessed/ems_standard.rda"))
