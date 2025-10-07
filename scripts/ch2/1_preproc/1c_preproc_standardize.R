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

## force 0 vals to NA (these shouldn't be used in computing the inforce_ means)
bits_panel <- bits_panel |> 
  mutate(
    across(
      contains("inforce_"),
      ~ if_else(.x == 0, NA, .x)
      )
    )

## prep n_bits var (to be used for spatial regressions)
bits_panel <- bits_panel |> 
  mutate(
    treaty_id_inforce = if_else(
      treaty_id*inforce == 0, NA, treaty_id*inforce
    )
  )

# standardize treatments ----
## general ----
bits_gen <- bits_panel |> 
  summarize(
    n_bits = n_distinct(treaty_id_inforce, na.rm = TRUE),
    partner_mean_gdp = mean(inforce_gdp, na.rm = TRUE),
    partner_mean_gdppc = mean(inforce_gdppc, na.rm = TRUE),
    partner_mean_pop = mean(inforce_pop, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      ),
    any_inforce = if_else(
      n_bits > 0, 1, 0
      )
    ) |> 
  arrange(cow, year) |> 
  relocate(any_inforce, n_bits, .after = year)

## south-south ----
bits_ss <- bits_panel |> 
  filter(glb_s == 1 & glb_s_partner == 1) |> 
  summarize(
    ss_n_bits = n_distinct(treaty_id_inforce, na.rm = TRUE),
    ss_partner_mean_gdp = mean(inforce_gdp, na.rm = TRUE),
    ss_partner_mean_gdppc = mean(inforce_gdppc, na.rm = TRUE),
    ss_partner_mean_pop = mean(inforce_pop, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      )
    ) |> 
  arrange(cow, year)

## north-south ----
bits_ns <- bits_panel |> 
  filter((glb_s == 1 & glb_s_partner == 0) | (glb_s == 0 & glb_s_partner == 1)) |> 
  summarize(
    ns_n_bits = n_distinct(treaty_id_inforce, na.rm = TRUE),
    ns_partner_mean_gdp = mean(inforce_gdp, na.rm = TRUE),
    ns_partner_mean_gdppc = mean(inforce_gdppc, na.rm = TRUE),
    ns_partner_mean_pop = mean(inforce_pop, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      )
    ) |> 
  arrange(cow, year)

## north-north ----
bits_nn <- bits_panel |> 
  filter(glb_s == 0 & glb_s_partner == 0) |> 
  summarize(
    nn_n_bits = n_distinct(treaty_id_inforce, na.rm = TRUE),
    nn_partner_mean_gdp = mean(inforce_gdp, na.rm = TRUE),
    nn_partner_mean_gdppc = mean(inforce_gdppc, na.rm = TRUE),
    nn_partner_mean_pop = mean(inforce_pop, na.rm = TRUE),
    .by = c(cow, year)
    ) |> 
  mutate(
    across(
      !c(cow, year),
      ~ if_else(
        is.nan(.x), NA, .x
        )
      )
    ) |> 
  arrange(cow, year)

## merge ----
bits_standard <- bits_gen |> 
  left_join(bits_ss) |> 
  left_join(bits_ns) |> 
  left_join(bits_nn)

# note: first nn BIT doesn't appear until 1950, so...

# save ----
bits_standard |> 
  save(file = here("data/ch2/preprocessed/bits_standard.rda"))
