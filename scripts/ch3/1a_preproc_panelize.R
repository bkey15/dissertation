# load packages ----
library(tidyverse)
library(here)
library(readxl)
library(countrycode)
library(states)

# load data ----
## ist
ist_dyads <- read_xlsx(path = "data/ch3/raw/ist_dat/IST_JPR_Attia&Grauvogel.xlsx")

# truncate data ----
## filter to western senders ----
## USA, Commonwealth Secretariat, European Economic Community (EU predecessor), EU, Organization of American States, United Nations
ist_dyads <- ist_dyads |> 
  filter(
    sender %in% c("2", "1240", "1653", "1653, 1830", "1830", "3900", "4400")
  )

### recode EEC to EU
ist_dyads <- ist_dyads |> 
  mutate(
    sender = case_when(
      sender == "1653" ~ "1830",
      sender == "1653, 1830" ~ "1830",
      .default = sender
      ),
    sender = as.numeric(sender)
    )

## filter to economic sanctions ----
### remove rows w/o any econ. sanctions
ist_dyads <- ist_dyads |> 
  filter(!measures %in% c("DS", "IM"))

## create economic measures indicators ----
ist_dyads <- ist_dyads |> 
  separate_wider_delim(
    measures,
    names = paste0("m_", 1:11),
    delim = ",",
    too_few = "align_start"
    ) |> 
  mutate(
    across(
      starts_with("m_"),
      ~ str_trim(.x)
      ),
    across(
      starts_with("m_"),
      ~ case_when(
        .x == "DS" ~ NA,
        .x == "IM" ~ NA,
        .default = .x
        )
      ),
    em_ae = if_any(
        starts_with("m_"),
        ~ .x == "AE"
      ),
    em_af = if_any(
      starts_with("m_"),
      ~ .x == "AF"
      ),
    em_as = if_any(
      starts_with("m_"),
      ~ .x == "AS"
      ),
    em_ce = if_any(
      starts_with("m_"),
      ~ .x == "CE"
      ),
    em_fb = if_any(
      starts_with("m_"),
      ~ .x == "FB"
      ),
    em_fs = if_any(
      starts_with("m_"),
      ~ .x == "FS"
      ),
    em_oe = if_any(
      starts_with("m_"),
      ~ .x == "OE"
      ),
    em_tb = if_any(
      starts_with("m_"),
      ~ .x == "TB"
      ),
    em_te = if_any(
      starts_with("m_"),
      ~ .x == "TE"
      ),
    across(
      starts_with("em_"),
      ~ if_else(is.na(.x), 0, 1)
      )
    ) |> 
  select(-starts_with("m_")) |> 
  relocate(starts_with("em_"), .after = review)

## filter to HR/DM goals ----
ist_dyads <- ist_dyads |> 
  mutate(
    goals = if_else(
      goals == "AC; WM, NP, CT",
      "AC, WM, NP, CT",
      goals
      )
    ) |> 
  separate_wider_delim(
    goals,
    names = paste0("g_", 1:6),
    delim = ",",
    too_few = "align_start"
    ) |> 
  mutate(
    across(
      starts_with("g_"),
      ~ str_trim(.x)
      ),
    hdg_indic = 
      if_any(
        starts_with("g_"),
        ~ .x == "HR" | .x == "DM"
        )
    ) |> 
  filter(hdg_indic == TRUE)

## create goals indicators ----
ist_dyads <- ist_dyads |> 
  mutate(
    hdg_ac = if_any(
      starts_with("g_"),
      ~ .x == "AC"
      ),
    hdg_cb = if_any(
      starts_with("g_"),
      ~ .x == "CB"
      ),
    hdg_ct = if_any(
      starts_with("g_"),
      ~ .x == "CT"
      ),
    hdg_dm = if_any(
      starts_with("g_"),
      ~ .x == "DM"
      ),
    hdg_dt = if_any(
      starts_with("g_"),
      ~ .x == "DT"
      ),
    hdg_hr = if_any(
      starts_with("g_"),
      ~ .x == "HR"
      ),
    hdg_np = if_any(
      starts_with("g_"),
      ~ .x == "NP"
      ),
    hdg_wm = if_any(
      starts_with("g_"),
      ~ .x == "WM"
      ),
    across(
      starts_with("hdg_"),
      ~ if_else(is.na(.x), 0, 1)
      )
    ) |> 
  select(
    -c(hdg_indic, starts_with("g_"))
    ) |> 
  relocate(starts_with("hdg_"), .before = adaptgoal)
  
# panelize ist data ----
## create `start_year` and `end_year` ----
ist_dyads <- ist_dyads |> 
  mutate(
    startdate = dmy(startdate),
    terdate = dmy(terdate),
    defactoter = dmy(defactoter),
    start_year = year(startdate),
    end_year = if_else(
      !is.na(defactoter), year(defactoter), year(terdate)
      )
    ) |> 
  select(-c(ends_with("date"), defactoter)) |> 
  relocate(ends_with("year"), .after = target)

## lengthen data ----
### create row for each sanction per year during ist_dyads coverage (1990-2018)
years_ist <- tibble(year = 1990:2018)

ist_panel <- ist_dyads |> 
  cross_join(years_ist) |> 
  relocate(year, .before = start_year)

## create `inforce` indicator ----
### gives whether sanctions are in-force for the year at issue; standardize treatment vars by this indicator, such that scores > 0 only appear during in-force years (later)
ist_panel <- ist_panel |> 
  mutate(
    inforce = case_when(
      year < start_year ~ 0,
      year >= start_year & is.na(end_year) ~ 1,
      year >= start_year & year < end_year ~ 1,
      year >= end_year ~ 0
      )
    ) |> 
  relocate(inforce, .after = end_year)

## select useful vars ---- 
ist_panel <- ist_panel |> 
  select(
    -c(
      ends_with("_year"),
      ongoing,
      gradual,
      expiry,
      review,
      datasets,
      comment,
      starts_with("source")
       )
    )

## save ---- 
ist_panel |> 
  save(
    file = here("data/ch3/preprocessed/ist_panel.rda")
    )

test <- ist_dyads |> 
  mutate(n_measures = str_count(measures, ",") + 1) |> 
  relocate(n_measures, .after = measures) |> 
  arrange(n_measures)
