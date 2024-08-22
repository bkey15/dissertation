library(tidyverse)
library(here)
library(feather)
library(vdemdata)
library(desta)

vdem <- vdem
words <- load(here("data/every_single_word/every_single_word.RData"))
mdb_speeches <- read_feather(here("data/mdb_speeches.feather"))

vdem_sml <- vdem |> 
  filter(year > 1991) |> 
  select(
    country_name,
    year,
    v2x_civlib,
    v2x_clphy,
    v2x_gender,
    v2xpe_exlgender,
    v2xcl_slave,
    v2pepwrort) |> 
  mutate(
    lgbt = pnorm(v2pepwrort),
    v2xpe_exlgender_rescale = 1 - v2xpe_exlgender,
    women = (v2x_gender + v2xpe_exlgender_rescale)/2,
    sex = (lgbt + women)/2
    ) |> 
  select(
    -c(v2x_gender,
       v2xpe_exlgender,
       v2xpe_exlgender_rescale,
       v2pepwrort
       )
    )

vdem_sml <- vdem_sml |> 
  mutate(
    decision_met1 = (v2x_clphy + v2xcl_slave + lgbt + women)/4,
    decision_met2 = (v2x_clphy + v2xcl_slave + sex)/3
    )

test1 <- vdem_sml |> 
  arrange(decision_met1) |> 
  slice_head(prop = 1/3)

test2 <- vdem_sml |> 
  arrange(decision_met2) |> 
  slice_head(prop = 1/3)
  
test3 <- vdem_sml |> 
  arrange(v2x_civlib) |> 
  slice_head(prop = 1/3)
