# load packages ----
library(tidyverse)
library(here)

# load data ----
load(here("data/ch1/preprocessed/standards.rda"))
load(here("data/ch2/preprocessed/bits_panel.rda"))

# merge standards of partners ----
## standards == gdp_log10, gdppc_log10, pop_log10
bits_panel <- bits_panel |> 
  left_join(
    standards,
    by = c(
      "cow_partner" = "cow",
      "year" = "year"
      )
    ) |> 
  rename(glb_s_partner = glb_s) |> 
  relocate(glb_s_partner, .after = cow_partner)

bits_panel <- standards |> 
  select(cow, year, glb_s) |> 
  right_join(bits_panel) |> 
  relocate(glb_s, .after = cow) |> 
  arrange(cow, cow_partner)

# interact standards w/ inforce ----
bits_panel <- bits_panel |> 
  mutate(
    inforce_gdp = inforce*gdp_log10,
    inforce_gdppc = inforce*gdppc_log10,
    inforce_pop = inforce*pop_log10
    )


