# Scrape BIT data
# Data source: https://investmentpolicy.unctad.org/international-investment-agreements
# For additional information on the scraping process, see notes.txt.

# load packages ----
library(tidyverse)
library(here)
library(rvest)
library(httr2)

# scrape ----
page_nums <- 1:157
bits_raw <- tibble()

for(page in page_nums){
  scrape <- request("https://investmentpolicy.unctad.org/international-investment-agreements/ajax/recent-treaties") |>
    req_method("PUT") |>
    req_headers(
      accept = "application/json, text/javascript, */*; q=0.01",
      `accept-language` = "en-US,en;q=0.9",
      origin = "https://investmentpolicy.unctad.org",
      priority = "u=1, i",
      referer = "https://investmentpolicy.unctad.org/international-investment-agreements",
      `sec-ch-ua` = '"Not(A:Brand";v="99", "Google Chrome";v="133", "Chromium";v="133"',
      `sec-ch-ua-mobile` = "?0",
      `sec-ch-ua-platform` = '"macOS"',
      `sec-fetch-dest` = "empty",
      `sec-fetch-mode` = "cors",
      `sec-fetch-site` = "same-origin",
      `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36",
      `x-requested-with` = "XMLHttpRequest",
      cookie = "fpestid=dvU53MSK48LComz_JFd5SXcFQjJhnU2CQJwBgkE07_l3251wrHWKE3-wd2pGRCcON9IjlQ; _cc_id=b0d25be02d874df86dbabb2e97723313; panoramaId_expiry=1741126084193; panoramaId=4a6637ad5da7d08cfac27be80123bd9563ce43d7fc4e552825f008cfefd0908b; panoramaIdType=panoIndiv; _gid=GA1.2.387146601.1740524178; _ga=GA1.1.192062402.1739394004; _ga_331595331=GS1.1.1740524178.2.0.1740524356.0.0.0; _ga_4057246821=GS1.1.1740524178.2.0.1740524356.0.0.0; _ga_SN6PPP7BP5=GS1.1.1740524178.2.0.1740524356.60.0.683684056; _ga_MNH7S5223X=GS1.1.1740523563.2.1.1740529154.0.0.0"
    ) |>
    req_body_raw(
      paste0('{"sortColumn":"SignDate","sortOrder":0,"pageIndex":', as.character(page), '}'),
      type = "application/json"
    ) |>
    req_perform() |> 
    resp_body_json()
  
  res <- read_html(scrape$Html) |> 
    html_element("table") |> 
    html_table(fill = TRUE)
  
  bits_raw <- bits_raw |> 
    rbind(res)
}

# clean ----
bits_raw <- bits_raw |> 
  janitor::clean_names()

cols <- colnames(bits_raw)

bits_raw <- bits_raw |> 
  select(-no) |> 
  mutate(
    across(
      any_of(cols),
      ~ na_if(.x, "")
      ),
    across(
      contains("date"),
      ~ dmy(.x)
      )
    )

# save ----
bits_raw |> 
  save(
    file = here("data/ch2/raw/bits_raw.rda")
  )
