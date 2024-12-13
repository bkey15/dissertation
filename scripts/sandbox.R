library(tidyverse)
library(here)
library(feather)
library(vdemdata)
library(desta)
library(PanelMatch)
library(tscsdep)
library(hdm)
library(DoubleML)
library(naniar)
library(readxl)
library(foreign)

vignette(package = "PanelMatch")
vignette("using_panelmatch", package = "PanelMatch")

vdem <- vdem
words <- load(here("data/every_single_word/every_single_word.RData"))
mdb_speeches <- read_feather(here("data/mdb_speeches.feather"))

vdem_dem_prot <- vdem |> 
  select(
    country_name,
    year,
    v2cademmob
    ) |> 
  filter(year > 1957)

miss_var_summary(vdem_dem_prot)

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
    v2pepwrort,
    ) |> 
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

ist_dat <- read_xlsx(here("data/ch3/ist_dat/IST_JPR_Attia&Grauvogel.xlsx"))
unique(ist_dat$target)

ist_dat <- ist_dat |> 
  mutate(
    startdate_new = dmy(ist_dat$startdate),
    terdate_new = dmy(ist_dat$terdate)
    )

desta <- read_delim(
  file = "data/ch1/desta/desta_version_02_02.txt"
) |> 
  filter(entry_type != "consolidated") |> 
  mutate(
    number = as.numeric(number)
  )

sb <- read.dta(file = here("replication_files/ch1/spilker_bohmelt/11558_2012_9155_MOESM2_ESM/01_RIO shapefile.dta"))

by <- read.dta(file = here("replication_files/ch2/bodea_ye/BJPS_replication_bodea_ye.dta"))

# old tuning code (pre-for loops)
imp_1.1_covars_cprmean <- imp_1.1 |> 
  select(!3:4 & !6:12)
imp_1.1_covars_cprgdp <- model.matrix(~ . - 1, data = imp_1.1_covars_cprgdp)

imp_1.1_covars_cprgdp <- imp_1.1 |> 
  select(!3:5 & !7:12)
imp_1.1_covars_cprgdp <- model.matrix(~ . - 1, data = imp_1.1_covars_cprgdp)

imp_1.1_covars_cprgdppc <- imp_1.1 |> 
  select(!3:6 & !8:12)
imp_1.1_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_1.1_covars_cprgdppc)

#### esr
imp_1.1_covars_esrmean <- imp_1.1 |> 
  select(!3:8 & !10:12)
imp_1.1_covars_esrmean <- model.matrix(~ . - 1, data = imp_1.1_covars_esrmean)

imp_1.1_covars_esrgdp <- imp_1.1 |> 
  select(!3:9 & !11:12)
imp_1.1_covars_esrgdp <- model.matrix(~ . - 1, data = imp_1.1_covars_esrgdp)

imp_1.1_covars_esrgdppc <- imp_1.1 |> 
  select(!3:10 & !12)
imp_1.1_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_1.1_covars_esrgdppc)

#### both
imp_1.1_covars_bothmean <- imp_1.1 |> 
  select(!3:4 & !6:8 & !10:12)
imp_1.1_covars_bothmean <- model.matrix(~ . - 1, data = imp_1.1_covars_bothmean)

imp_1.1_covars_bothgdp <- imp_1.1 |> 
  select(!3:5 & !7:9 & !11:12)
imp_1.1_covars_bothgdp <- model.matrix(~ . - 1, data = imp_1.1_covars_bothgdp)

imp_1.1_covars_bothgdppc <- imp_1.1 |> 
  select(!3:6 & !8:10 & !12)
imp_1.1_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_1.1_covars_bothgdppc)

#### 1.2
#### cpr
imp_1.2_covars_cprmean <- imp_1.2 |> 
  select(!3:4 & !6:12)
imp_1.2_covars_cprmean <- model.matrix(~ . - 1, data = imp_1.2_covars_cprmean)

imp_1.2_covars_cprgdp <- imp_1.2 |> 
  select(!3:5 & !7:12)
imp_1.2_covars_cprgdp <- model.matrix(~ . - 1, data = imp_1.2_covars_cprgdp)

imp_1.2_covars_cprgdppc <- imp_1.2 |> 
  select(!3:6 & !8:12)
imp_1.2_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_1.2_covars_cprgdppc)

#### esr
imp_1.2_covars_esrmean <- imp_1.2 |> 
  select(!3:8 & !10:12)
imp_1.2_covars_esrmean <- model.matrix(~ . - 1, data = imp_1.2_covars_esrmean)

imp_1.2_covars_esrgdp <- imp_1.2 |> 
  select(!3:9 & !11:12)
imp_1.2_covars_esrgdp <- model.matrix(~ . - 1, data = imp_1.2_covars_esrgdp)

imp_1.2_covars_esrgdppc <- imp_1.2 |> 
  select(!3:10 & !12)
imp_1.2_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_1.2_covars_esrgdppc)

#### both
imp_1.2_covars_bothmean <- imp_1.2 |> 
  select(!3:4 & !6:8 & !10:12)
imp_1.2_covars_bothmean <- model.matrix(~ . - 1, data = imp_1.2_covars_bothmean)

imp_1.2_covars_bothgdp <- imp_1.2 |> 
  select(!3:5 & !7:9 & !11:12)
imp_1.2_covars_bothgdp <- model.matrix(~ . - 1, data = imp_1.2_covars_bothgdp)

imp_1.2_covars_bothgdppc <- imp_1.2 |> 
  select(!3:6 & !8:10 & !12)
imp_1.2_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_1.2_covars_bothgdppc)

#### 1.3
#### cpr
imp_1.3_covars_cprmean <- imp_1.3 |> 
  select(!3:4 & !6:12)
imp_1.3_covars_cprmean <- model.matrix(~ . - 1, data = imp_1.3_covars_cprmean)

imp_1.3_covars_cprgdp <- imp_1.3 |> 
  select(!3:5 & !7:12)
imp_1.3_covars_cprgdp <- model.matrix(~ . - 1, data = imp_1.3_covars_cprgdp)

imp_1.3_covars_cprgdppc <- imp_1.3 |> 
  select(!3:6 & !8:12)
imp_1.3_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_1.3_covars_cprgdppc)

#### esr
imp_1.3_covars_esrmean <- imp_1.3 |> 
  select(!3:8 & !10:12)
imp_1.3_covars_esrmean <- model.matrix(~ . - 1, data = imp_1.3_covars_esrmean)

imp_1.3_covars_esrgdp <- imp_1.3 |> 
  select(!3:9 & !11:12)
imp_1.3_covars_esrgdp <- model.matrix(~ . - 1, data = imp_1.3_covars_esrgdp)

imp_1.3_covars_esrgdppc <- imp_1.3 |> 
  select(!3:10 & !12)
imp_1.3_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_1.3_covars_esrgdppc)

#### both
imp_1.3_covars_bothmean <- imp_1.3 |> 
  select(!3:4 & !6:8 & !10:12)
imp_1.3_covars_bothmean <- model.matrix(~ . - 1, data = imp_1.3_covars_bothmean)

imp_1.3_covars_bothgdp <- imp_1.3 |> 
  select(!3:5 & !7:9 & !11:12)
imp_1.3_covars_bothgdp <- model.matrix(~ . - 1, data = imp_1.3_covars_bothgdp)

imp_1.3_covars_bothgdppc <- imp_1.3 |> 
  select(!3:6 & !8:10 & !12)
imp_1.3_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_1.3_covars_bothgdppc)


#### 1.4
#### cpr
imp_1.4_covars_cprmean <- imp_1.4 |> 
  select(!3:4 & !6:12)
imp_1.4_covars_cprmean <- model.matrix(~ . - 1, data = imp_1.4_covars_cprmean)

imp_1.4_covars_cprgdp <- imp_1.4 |> 
  select(!3:5 & !7:12)
imp_1.4_covars_cprgdp <- model.matrix(~ . - 1, data = imp_1.4_covars_cprgdp)

imp_1.4_covars_cprgdppc <- imp_1.4 |> 
  select(!3:6 & !8:12)
imp_1.4_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_1.4_covars_cprgdppc)

#### esr
imp_1.4_covars_esrmean <- imp_1.4 |> 
  select(!3:8 & !10:12)
imp_1.4_covars_esrmean <- model.matrix(~ . - 1, data = imp_1.4_covars_esrmean)

imp_1.4_covars_esrgdp <- imp_1.4 |> 
  select(!3:9 & !11:12)
imp_1.4_covars_esrgdp <- model.matrix(~ . - 1, data = imp_1.4_covars_esrgdp)

imp_1.4_covars_esrgdppc <- imp_1.4 |> 
  select(!3:10 & !12)
imp_1.4_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_1.4_covars_esrgdppc)

#### both
imp_1.4_covars_bothmean <- imp_1.4 |> 
  select(!3:4 & !6:8 & !10:12)
imp_1.4_covars_bothmean <- model.matrix(~ . - 1, data = imp_1.4_covars_bothmean)

imp_1.4_covars_bothgdp <- imp_1.4 |> 
  select(!3:5 & !7:9 & !11:12)
imp_1.4_covars_bothgdp <- model.matrix(~ . - 1, data = imp_1.4_covars_bothgdp)

imp_1.4_covars_bothgdppc <- imp_1.4 |> 
  select(!3:6 & !8:10 & !12)
imp_1.4_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_1.4_covars_bothgdppc)


#### 1.5
#### cpr
imp_1.5_covars_cprmean <- imp_1.5 |> 
  select(!3:4 & !6:12)
imp_1.5_covars_cprmean <- model.matrix(~ . - 1, data = imp_1.5_covars_cprmean)

imp_1.5_covars_cprgdp <- imp_1.5 |> 
  select(!3:5 & !7:12)
imp_1.5_covars_cprgdp <- model.matrix(~ . - 1, data = imp_1.5_covars_cprgdp)

imp_1.5_covars_cprgdppc <- imp_1.5 |> 
  select(!3:6 & !8:12)
imp_1.5_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_1.5_covars_cprgdppc)

#### esr
imp_1.5_covars_esrmean <- imp_1.5 |> 
  select(!3:8 & !10:12)
imp_1.5_covars_esrmean <- model.matrix(~ . - 1, data = imp_1.5_covars_esrmean)

imp_1.5_covars_esrgdp <- imp_1.5 |> 
  select(!3:9 & !11:12)
imp_1.5_covars_esrgdp <- model.matrix(~ . - 1, data = imp_1.5_covars_esrgdp)

imp_1.5_covars_esrgdppc <- imp_1.5 |> 
  select(!3:10 & !12)
imp_1.5_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_1.5_covars_esrgdppc)

#### both
imp_1.5_covars_bothmean <- imp_1.5 |> 
  select(!3:4 & !6:8 & !10:12)
imp_1.5_covars_bothmean <- model.matrix(~ . - 1, data = imp_1.5_covars_bothmean)

imp_1.5_covars_bothgdp <- imp_1.5 |> 
  select(!3:5 & !7:9 & !11:12)
imp_1.5_covars_bothgdp <- model.matrix(~ . - 1, data = imp_1.5_covars_bothgdp)

imp_1.5_covars_bothgdppc <- imp_1.5 |> 
  select(!3:6 & !8:10 & !12)
imp_1.5_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_1.5_covars_bothgdppc)



#### 2.1
#### cpr
imp_2.1_covars_cprmean <- imp_2.1 |> 
  select(!3:4 & !6:12)
imp_2.1_covars_cprmean <- model.matrix(~ . - 1, data = imp_2.1_covars_cprmean)

imp_2.1_covars_cprgdp <- imp_2.1 |> 
  select(!3:5 & !7:12)
imp_2.1_covars_cprgdp <- model.matrix(~ . - 1, data = imp_2.1_covars_cprgdp)

imp_2.1_covars_cprgdppc <- imp_2.1 |> 
  select(!3:6 & !8:12)
imp_2.1_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_2.1_covars_cprgdppc)

#### esr
imp_2.1_covars_esrmean <- imp_2.1 |> 
  select(!3:8 & !10:12)
imp_2.1_covars_esrmean <- model.matrix(~ . - 1, data = imp_2.1_covars_esrmean)

imp_2.1_covars_esrgdp <- imp_2.1 |> 
  select(!3:9 & !11:12)
imp_2.1_covars_esrgdp <- model.matrix(~ . - 1, data = imp_2.1_covars_esrgdp)

imp_2.1_covars_esrgdppc <- imp_2.1 |> 
  select(!3:10 & !12)
imp_2.1_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_2.1_covars_esrgdppc)

#### both
imp_2.1_covars_bothmean <- imp_2.1 |> 
  select(!3:4 & !6:8 & !10:12)
imp_2.1_covars_bothmean <- model.matrix(~ . - 1, data = imp_2.1_covars_bothmean)

imp_2.1_covars_bothgdp <- imp_2.1 |> 
  select(!3:5 & !7:9 & !11:12)
imp_2.1_covars_bothgdp <- model.matrix(~ . - 1, data = imp_2.1_covars_bothgdp)

imp_2.1_covars_bothgdppc <- imp_2.1 |> 
  select(!3:6 & !8:10 & !12)
imp_2.1_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_2.1_covars_bothgdppc)

#### 2.2
#### cpr
imp_2.2_covars_cprmean <- imp_2.2 |> 
  select(!3:4 & !6:12)
imp_2.2_covars_cprmean <- model.matrix(~ . - 1, data = imp_2.2_covars_cprmean)

imp_2.2_covars_cprgdp <- imp_2.2 |> 
  select(!3:5 & !7:12)
imp_2.2_covars_cprgdp <- model.matrix(~ . - 1, data = imp_2.2_covars_cprgdp)

imp_2.2_covars_cprgdppc <- imp_2.2 |> 
  select(!3:6 & !8:12)
imp_2.2_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_2.2_covars_cprgdppc)

#### esr
imp_2.2_covars_esrmean <- imp_2.2 |> 
  select(!3:8 & !10:12)
imp_2.2_covars_esrmean <- model.matrix(~ . - 1, data = imp_2.2_covars_esrmean)

imp_2.2_covars_esrgdp <- imp_2.2 |> 
  select(!3:9 & !11:12)
imp_2.2_covars_esrgdp <- model.matrix(~ . - 1, data = imp_2.2_covars_esrgdp)

imp_2.2_covars_esrgdppc <- imp_2.2 |> 
  select(!3:10 & !12)
imp_2.2_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_2.2_covars_esrgdppc)

#### both
imp_2.2_covars_bothmean <- imp_2.2 |> 
  select(!3:4 & !6:8 & !10:12)
imp_2.2_covars_bothmean <- model.matrix(~ . - 1, data = imp_2.2_covars_bothmean)

imp_2.2_covars_bothgdp <- imp_2.2 |> 
  select(!3:5 & !7:9 & !11:12)
imp_2.2_covars_bothgdp <- model.matrix(~ . - 1, data = imp_2.2_covars_bothgdp)

imp_2.2_covars_bothgdppc <- imp_2.2 |> 
  select(!3:6 & !8:10 & !12)
imp_2.2_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_2.2_covars_bothgdppc)

#### 2.3
#### cpr
imp_2.3_covars_cprmean <- imp_2.3 |> 
  select(!3:4 & !6:12)
imp_2.3_covars_cprmean <- model.matrix(~ . - 1, data = imp_2.3_covars_cprmean)

imp_2.3_covars_cprgdp <- imp_2.3 |> 
  select(!3:5 & !7:12)
imp_2.3_covars_cprgdp <- model.matrix(~ . - 1, data = imp_2.3_covars_cprgdp)

imp_2.3_covars_cprgdppc <- imp_2.3 |> 
  select(!3:6 & !8:12)
imp_2.3_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_2.3_covars_cprgdppc)

#### esr
imp_2.3_covars_esrmean <- imp_2.3 |> 
  select(!3:8 & !10:12)
imp_2.3_covars_esrmean <- model.matrix(~ . - 1, data = imp_2.3_covars_esrmean)

imp_2.3_covars_esrgdp <- imp_2.3 |> 
  select(!3:9 & !11:12)
imp_2.3_covars_esrgdp <- model.matrix(~ . - 1, data = imp_2.3_covars_esrgdp)

imp_2.3_covars_esrgdppc <- imp_2.3 |> 
  select(!3:10 & !12)
imp_2.3_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_2.3_covars_esrgdppc)

#### both
imp_2.3_covars_bothmean <- imp_2.3 |> 
  select(!3:4 & !6:8 & !10:12)
imp_2.3_covars_bothmean <- model.matrix(~ . - 1, data = imp_2.3_covars_bothmean)

imp_2.3_covars_bothgdp <- imp_2.3 |> 
  select(!3:5 & !7:9 & !11:12)
imp_2.3_covars_bothgdp <- model.matrix(~ . - 1, data = imp_2.3_covars_bothgdp)

imp_2.3_covars_bothgdppc <- imp_2.3 |> 
  select(!3:6 & !8:10 & !12)
imp_2.3_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_2.3_covars_bothgdppc)


#### 2.4
#### cpr
imp_2.4_covars_cprmean <- imp_2.4 |> 
  select(!3:4 & !6:12)
imp_2.4_covars_cprmean <- model.matrix(~ . - 1, data = imp_2.4_covars_cprmean)

imp_2.4_covars_cprgdp <- imp_2.4 |> 
  select(!3:5 & !7:12)
imp_2.4_covars_cprgdp <- model.matrix(~ . - 1, data = imp_2.4_covars_cprgdp)

imp_2.4_covars_cprgdppc <- imp_2.4 |> 
  select(!3:6 & !8:12)
imp_2.4_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_2.4_covars_cprgdppc)

#### esr
imp_2.4_covars_esrmean <- imp_2.4 |> 
  select(!3:8 & !10:12)
imp_2.4_covars_esrmean <- model.matrix(~ . - 1, data = imp_2.4_covars_esrmean)

imp_2.4_covars_esrgdp <- imp_2.4 |> 
  select(!3:9 & !11:12)
imp_2.4_covars_esrgdp <- model.matrix(~ . - 1, data = imp_2.4_covars_esrgdp)

imp_2.4_covars_esrgdppc <- imp_2.4 |> 
  select(!3:10 & !12)
imp_2.4_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_2.4_covars_esrgdppc)

#### both
imp_2.4_covars_bothmean <- imp_2.4 |> 
  select(!3:4 & !6:8 & !10:12)
imp_2.4_covars_bothmean <- model.matrix(~ . - 1, data = imp_2.4_covars_bothmean)

imp_2.4_covars_bothgdp <- imp_2.4 |> 
  select(!3:5 & !7:9 & !11:12)
imp_2.4_covars_bothgdp <- model.matrix(~ . - 1, data = imp_2.4_covars_bothgdp)

imp_2.4_covars_bothgdppc <- imp_2.4 |> 
  select(!3:6 & !8:10 & !12)
imp_2.4_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_2.4_covars_bothgdppc)

#### 2.5
#### cpr
imp_2.5_covars_cprmean <- imp_2.5 |> 
  select(!3:4 & !6:12)
imp_2.5_covars_cprmean <- model.matrix(~ . - 1, data = imp_2.5_covars_cprmean)

imp_2.5_covars_cprgdp <- imp_2.5 |> 
  select(!3:5 & !7:12)
imp_2.5_covars_cprgdp <- model.matrix(~ . - 1, data = imp_2.5_covars_cprgdp)

imp_2.5_covars_cprgdppc <- imp_2.5 |> 
  select(!3:6 & !8:12)
imp_2.5_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_2.5_covars_cprgdppc)

#### esr
imp_2.5_covars_esrmean <- imp_2.5 |> 
  select(!3:8 & !10:12)
imp_2.5_covars_esrmean <- model.matrix(~ . - 1, data = imp_2.5_covars_esrmean)

imp_2.5_covars_esrgdp <- imp_2.5 |> 
  select(!3:9 & !11:12)
imp_2.5_covars_esrgdp <- model.matrix(~ . - 1, data = imp_2.5_covars_esrgdp)

imp_2.5_covars_esrgdppc <- imp_2.5 |> 
  select(!3:10 & !12)
imp_2.5_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_2.5_covars_esrgdppc)

#### both
imp_2.5_covars_bothmean <- imp_2.5 |> 
  select(!3:4 & !6:8 & !10:12)
imp_2.5_covars_bothmean <- model.matrix(~ . - 1, data = imp_2.5_covars_bothmean)

imp_2.5_covars_bothgdp <- imp_2.5 |> 
  select(!3:5 & !7:9 & !11:12)
imp_2.5_covars_bothgdp <- model.matrix(~ . - 1, data = imp_2.5_covars_bothgdp)

imp_2.5_covars_bothgdppc <- imp_2.5 |> 
  select(!3:6 & !8:10 & !12)
imp_2.5_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_2.5_covars_bothgdppc)


#### 3.1
#### cpr
imp_3.1_covars_cprmean <- imp_3.1 |> 
  select(!3:4 & !6:12)
imp_3.1_covars_cprmean <- model.matrix(~ . - 1, data = imp_3.1_covars_cprmean)

imp_3.1_covars_cprgdp <- imp_3.1 |> 
  select(!3:5 & !7:12)
imp_3.1_covars_cprgdp <- model.matrix(~ . - 1, data = imp_3.1_covars_cprgdp)

imp_3.1_covars_cprgdppc <- imp_3.1 |> 
  select(!3:6 & !8:12)
imp_3.1_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_3.1_covars_cprgdppc)

#### esr
imp_3.1_covars_esrmean <- imp_3.1 |> 
  select(!3:8 & !10:12)
imp_3.1_covars_esrmean <- model.matrix(~ . - 1, data = imp_3.1_covars_esrmean)

imp_3.1_covars_esrgdp <- imp_3.1 |> 
  select(!3:9 & !11:12)
imp_3.1_covars_esrgdp <- model.matrix(~ . - 1, data = imp_3.1_covars_esrgdp)

imp_3.1_covars_esrgdppc <- imp_3.1 |> 
  select(!3:10 & !12)
imp_3.1_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_3.1_covars_esrgdppc)

#### both
imp_3.1_covars_bothmean <- imp_3.1 |> 
  select(!3:4 & !6:8 & !10:12)
imp_3.1_covars_bothmean <- model.matrix(~ . - 1, data = imp_3.1_covars_bothmean)

imp_3.1_covars_bothgdp <- imp_3.1 |> 
  select(!3:5 & !7:9 & !11:12)
imp_3.1_covars_bothgdp <- model.matrix(~ . - 1, data = imp_3.1_covars_bothgdp)

imp_3.1_covars_bothgdppc <- imp_3.1 |> 
  select(!3:6 & !8:10 & !12)
imp_3.1_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_3.1_covars_bothgdppc)

#### 3.2
#### cpr
imp_3.2_covars_cprmean <- imp_3.2 |> 
  select(!3:4 & !6:12)
imp_3.2_covars_cprmean <- model.matrix(~ . - 1, data = imp_3.2_covars_cprmean)

imp_3.2_covars_cprgdp <- imp_3.2 |> 
  select(!3:5 & !7:12)
imp_3.2_covars_cprgdp <- model.matrix(~ . - 1, data = imp_3.2_covars_cprgdp)

imp_3.2_covars_cprgdppc <- imp_3.2 |> 
  select(!3:6 & !8:12)
imp_3.2_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_3.2_covars_cprgdppc)

#### esr
imp_3.2_covars_esrmean <- imp_3.2 |> 
  select(!3:8 & !10:12)
imp_3.2_covars_esrmean <- model.matrix(~ . - 1, data = imp_3.2_covars_esrmean)

imp_3.2_covars_esrgdp <- imp_3.2 |> 
  select(!3:9 & !11:12)
imp_3.2_covars_esrgdp <- model.matrix(~ . - 1, data = imp_3.2_covars_esrgdp)

imp_3.2_covars_esrgdppc <- imp_3.2 |> 
  select(!3:10 & !12)
imp_3.2_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_3.2_covars_esrgdppc)

#### both
imp_3.2_covars_bothmean <- imp_3.2 |> 
  select(!3:4 & !6:8 & !10:12)
imp_3.2_covars_bothmean <- model.matrix(~ . - 1, data = imp_3.2_covars_bothmean)

imp_3.2_covars_bothgdp <- imp_3.2 |> 
  select(!3:5 & !7:9 & !11:12)
imp_3.2_covars_bothgdp <- model.matrix(~ . - 1, data = imp_3.2_covars_bothgdp)

imp_3.2_covars_bothgdppc <- imp_3.2 |> 
  select(!3:6 & !8:10 & !12)
imp_3.2_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_3.2_covars_bothgdppc)

#### 3.3
#### cpr
imp_3.3_covars_cprmean <- imp_3.3 |> 
  select(!3:4 & !6:12)
imp_3.3_covars_cprmean <- model.matrix(~ . - 1, data = imp_3.3_covars_cprmean)

imp_3.3_covars_cprgdp <- imp_3.3 |> 
  select(!3:5 & !7:12)
imp_3.3_covars_cprgdp <- model.matrix(~ . - 1, data = imp_3.3_covars_cprgdp)

imp_3.3_covars_cprgdppc <- imp_3.3 |> 
  select(!3:6 & !8:12)
imp_3.3_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_3.3_covars_cprgdppc)

#### esr
imp_3.3_covars_esrmean <- imp_3.3 |> 
  select(!3:8 & !10:12)
imp_3.3_covars_esrmean <- model.matrix(~ . - 1, data = imp_3.3_covars_esrmean)

imp_3.3_covars_esrgdp <- imp_3.3 |> 
  select(!3:9 & !11:12)
imp_3.3_covars_esrgdp <- model.matrix(~ . - 1, data = imp_3.3_covars_esrgdp)

imp_3.3_covars_esrgdppc <- imp_3.3 |> 
  select(!3:10 & !12)
imp_3.3_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_3.3_covars_esrgdppc)

#### both
imp_3.3_covars_bothmean <- imp_3.3 |> 
  select(!3:4 & !6:8 & !10:12)
imp_3.3_covars_bothmean <- model.matrix(~ . - 1, data = imp_3.3_covars_bothmean)

imp_3.3_covars_bothgdp <- imp_3.3 |> 
  select(!3:5 & !7:9 & !11:12)
imp_3.3_covars_bothgdp <- model.matrix(~ . - 1, data = imp_3.3_covars_bothgdp)

imp_3.3_covars_bothgdppc <- imp_3.3 |> 
  select(!3:6 & !8:10 & !12)
imp_3.3_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_3.3_covars_bothgdppc)

#### 3.4
#### cpr
imp_3.4_covars_cprmean <- imp_3.4 |> 
  select(!3:4 & !6:12)
imp_3.4_covars_cprmean <- model.matrix(~ . - 1, data = imp_3.4_covars_cprmean)

imp_3.4_covars_cprgdp <- imp_3.4 |> 
  select(!3:5 & !7:12)
imp_3.4_covars_cprgdp <- model.matrix(~ . - 1, data = imp_3.4_covars_cprgdp)

imp_3.4_covars_cprgdppc <- imp_3.4 |> 
  select(!3:6 & !8:12)
imp_3.4_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_3.4_covars_cprgdppc)

#### esr
imp_3.4_covars_esrmean <- imp_3.4 |> 
  select(!3:8 & !10:12)
imp_3.4_covars_esrmean <- model.matrix(~ . - 1, data = imp_3.4_covars_esrmean)

imp_3.4_covars_esrgdp <- imp_3.4 |> 
  select(!3:9 & !11:12)
imp_3.4_covars_esrgdp <- model.matrix(~ . - 1, data = imp_3.4_covars_esrgdp)

imp_3.4_covars_esrgdppc <- imp_3.4 |> 
  select(!3:10 & !12)
imp_3.4_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_3.4_covars_esrgdppc)

#### both
imp_3.4_covars_bothmean <- imp_3.4 |> 
  select(!3:4 & !6:8 & !10:12)
imp_3.4_covars_bothmean <- model.matrix(~ . - 1, data = imp_3.4_covars_bothmean)

imp_3.4_covars_bothgdp <- imp_3.4 |> 
  select(!3:5 & !7:9 & !11:12)
imp_3.4_covars_bothgdp <- model.matrix(~ . - 1, data = imp_3.4_covars_bothgdp)

imp_3.4_covars_bothgdppc <- imp_3.4 |> 
  select(!3:6 & !8:10 & !12)
imp_3.4_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_3.4_covars_bothgdppc)

#### 3.5
#### cpr
imp_3.5_covars_cprmean <- imp_3.5 |> 
  select(!3:4 & !6:12)
imp_3.5_covars_cprmean <- model.matrix(~ . - 1, data = imp_3.5_covars_cprmean)

imp_3.5_covars_cprgdp <- imp_3.5 |> 
  select(!3:5 & !7:12)
imp_3.5_covars_cprgdp <- model.matrix(~ . - 1, data = imp_3.5_covars_cprgdp)

imp_3.5_covars_cprgdppc <- imp_3.5 |> 
  select(!3:6 & !8:12)
imp_3.5_covars_cprgdppc <- model.matrix(~ . - 1, data = imp_3.5_covars_cprgdppc)

#### esr
imp_3.5_covars_esrmean <- imp_3.5 |> 
  select(!3:8 & !10:12)
imp_3.5_covars_esrmean <- model.matrix(~ . - 1, data = imp_3.5_covars_esrmean)

imp_3.5_covars_esrgdp <- imp_3.5 |> 
  select(!3:9 & !11:12)
imp_3.5_covars_esrgdp <- model.matrix(~ . - 1, data = imp_3.5_covars_esrgdp)

imp_3.5_covars_esrgdppc <- imp_3.5 |> 
  select(!3:10 & !12)
imp_3.5_covars_esrgdppc <- model.matrix(~ . - 1, data = imp_3.5_covars_esrgdppc)

#### both
imp_3.5_covars_bothmean <- imp_3.5 |> 
  select(!3:4 & !6:8 & !10:12)
imp_3.5_covars_bothmean <- model.matrix(~ . - 1, data = imp_3.5_covars_bothmean)

imp_3.5_covars_bothgdp <- imp_3.5 |> 
  select(!3:5 & !7:9 & !11:12)
imp_3.5_covars_bothgdp <- model.matrix(~ . - 1, data = imp_3.5_covars_bothgdp)

imp_3.5_covars_bothgdppc <- imp_3.5 |> 
  select(!3:6 & !8:10 & !12)
imp_3.5_covars_bothgdppc <- model.matrix(~ . - 1, data = imp_3.5_covars_bothgdppc)

## imp_1 ----
### cpr_mean ----
#### 1.1
set.seed(15275)
imp_1.1_cprmean_cv <- cv.glmnet(
  x = imp_1.1_covars_cprmean,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_cprmean_cv <- cv.glmnet(
  x = imp_1.2_covars_cprmean,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_cprmean_cv <- cv.glmnet(
  x = imp_1.3_covars_cprmean,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_cprmean_cv <- cv.glmnet(
  x = imp_1.4_covars_cprmean,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_cprmean_cv <- cv.glmnet(
  x = imp_1.5_covars_cprmean,
  y = dep_1,
  alpha = 1
)

### cpr_gdp ----
#### 1.1
set.seed(15275)
imp_1.1_cprgdp_cv <- cv.glmnet(
  x = imp_1.1_covars_cprgdp,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_cprgdp_cv <- cv.glmnet(
  x = imp_1.2_covars_cprgdp,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_cprgdp_cv <- cv.glmnet(
  x = imp_1.3_covars_cprgdp,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_cprgdp_cv <- cv.glmnet(
  x = imp_1.4_covars_cprgdp,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_cprgdp_cv <- cv.glmnet(
  x = imp_1.5_covars_cprgdp,
  y = dep_1,
  alpha = 1
)

### cpr_gdppc ----
#### 1.1
set.seed(15275)
imp_1.1_cprgdppc_cv <- cv.glmnet(
  x = imp_1.1_covars_cprgdppc,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_cprgdppc_cv <- cv.glmnet(
  x = imp_1.2_covars_cprgdppc,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_cprgdppc_cv <- cv.glmnet(
  x = imp_1.3_covars_cprgdppc,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_cprgdppc_cv <- cv.glmnet(
  x = imp_1.4_covars_cprgdppc,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_cprgdppc_cv <- cv.glmnet(
  x = imp_1.5_covars_cprgdppc,
  y = dep_1,
  alpha = 1
)

### esr_mean ----
#### 1.1
set.seed(15275)
imp_1.1_esrmean_cv <- cv.glmnet(
  x = imp_1.1_covars_esrmean,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_esrmean_cv <- cv.glmnet(
  x = imp_1.2_covars_esrmean,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_esrmean_cv <- cv.glmnet(
  x = imp_1.3_covars_esrmean,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_esrmean_cv <- cv.glmnet(
  x = imp_1.4_covars_esrmean,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_esrmean_cv <- cv.glmnet(
  x = imp_1.5_covars_esrmean,
  y = dep_1,
  alpha = 1
)

### esr_gdp ----
#### 1.1
set.seed(15275)
imp_1.1_esrgdp_cv <- cv.glmnet(
  x = imp_1.1_covars_esrgdp,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_esrgdp_cv <- cv.glmnet(
  x = imp_1.2_covars_esrgdp,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_esrgdp_cv <- cv.glmnet(
  x = imp_1.3_covars_esrgdp,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_esrgdp_cv <- cv.glmnet(
  x = imp_1.4_covars_esrgdp,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_esrgdp_cv <- cv.glmnet(
  x = imp_1.5_covars_esrgdp,
  y = dep_1,
  alpha = 1
)

### esr_gdppc ----
#### 1.1
set.seed(15275)
imp_1.1_esrgdppc_cv <- cv.glmnet(
  x = imp_1.1_covars_esrgdppc,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_esrgdppc_cv <- cv.glmnet(
  x = imp_1.2_covars_esrgdppc,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_esrgdppc_cv <- cv.glmnet(
  x = imp_1.3_covars_esrgdppc,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_esrgdppc_cv <- cv.glmnet(
  x = imp_1.4_covars_esrgdppc,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_esrgdppc_cv <- cv.glmnet(
  x = imp_1.5_covars_esrgdppc,
  y = dep_1,
  alpha = 1
)

### both_mean ----
#### 1.1
set.seed(15275)
imp_1.1_bothmean_cv <- cv.glmnet(
  x = imp_1.1_covars_bothmean,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_bothmean_cv <- cv.glmnet(
  x = imp_1.2_covars_bothmean,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_bothmean_cv <- cv.glmnet(
  x = imp_1.3_covars_bothmean,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_bothmean_cv <- cv.glmnet(
  x = imp_1.4_covars_bothmean,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_bothmean_cv <- cv.glmnet(
  x = imp_1.5_covars_bothmean,
  y = dep_1,
  alpha = 1
)

### both_gdp ----
#### 1.1
set.seed(15275)
imp_1.1_bothgdp_cv <- cv.glmnet(
  x = imp_1.1_covars_bothgdp,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_bothgdp_cv <- cv.glmnet(
  x = imp_1.2_covars_bothgdp,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_bothgdp_cv <- cv.glmnet(
  x = imp_1.3_covars_bothgdp,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_bothgdp_cv <- cv.glmnet(
  x = imp_1.4_covars_bothgdp,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_bothgdp_cv <- cv.glmnet(
  x = imp_1.5_covars_bothgdp,
  y = dep_1,
  alpha = 1
)

### both_gdppc ----
#### 1.1
set.seed(15275)
imp_1.1_bothgdppc_cv <- cv.glmnet(
  x = imp_1.1_covars_bothgdppc,
  y = dep_1,
  alpha = 1
)

#### 1.2
set.seed(15275)
imp_1.2_bothgdppc_cv <- cv.glmnet(
  x = imp_1.2_covars_bothgdppc,
  y = dep_1,
  alpha = 1
)

#### 1.3
set.seed(15275)
imp_1.3_bothgdppc_cv <- cv.glmnet(
  x = imp_1.3_covars_bothgdppc,
  y = dep_1,
  alpha = 1
)

### 1.4
set.seed(15275)
imp_1.4_bothgdppc_cv <- cv.glmnet(
  x = imp_1.4_covars_bothgdppc,
  y = dep_1,
  alpha = 1
)

### 1.5
set.seed(15275)
imp_1.5_bothgdppc_cv <- cv.glmnet(
  x = imp_1.5_covars_bothgdppc,
  y = dep_1,
  alpha = 1
)

## imp_2 ----
### cpr_mean ----
#### 2.1
set.seed(15275)
imp_2.1_cprmean_cv <- cv.glmnet(
  x = imp_2.1_covars_cprmean,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_cprmean_cv <- cv.glmnet(
  x = imp_2.2_covars_cprmean,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_cprmean_cv <- cv.glmnet(
  x = imp_2.3_covars_cprmean,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_cprmean_cv <- cv.glmnet(
  x = imp_2.4_covars_cprmean,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_cprmean_cv <- cv.glmnet(
  x = imp_2.5_covars_cprmean,
  y = dep_2,
  alpha = 1
)

### cpr_gdp ----
#### 2.1
set.seed(15275)
imp_2.1_cprgdp_cv <- cv.glmnet(
  x = imp_2.1_covars_cprgdp,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_cprgdp_cv <- cv.glmnet(
  x = imp_2.2_covars_cprgdp,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_cprgdp_cv <- cv.glmnet(
  x = imp_2.3_covars_cprgdp,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_cprgdp_cv <- cv.glmnet(
  x = imp_2.4_covars_cprgdp,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_cprgdp_cv <- cv.glmnet(
  x = imp_2.5_covars_cprgdp,
  y = dep_2,
  alpha = 1
)

### cpr_gdppc ----
#### 2.1
set.seed(15275)
imp_2.1_cprgdppc_cv <- cv.glmnet(
  x = imp_2.1_covars_cprgdppc,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_cprgdppc_cv <- cv.glmnet(
  x = imp_2.2_covars_cprgdppc,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_cprgdppc_cv <- cv.glmnet(
  x = imp_2.3_covars_cprgdppc,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_cprgdppc_cv <- cv.glmnet(
  x = imp_2.4_covars_cprgdppc,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_cprgdppc_cv <- cv.glmnet(
  x = imp_2.5_covars_cprgdppc,
  y = dep_2,
  alpha = 1
)

### esr_mean ----
#### 2.1
set.seed(15275)
imp_2.1_esrmean_cv <- cv.glmnet(
  x = imp_2.1_covars_esrmean,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_esrmean_cv <- cv.glmnet(
  x = imp_2.2_covars_esrmean,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_esrmean_cv <- cv.glmnet(
  x = imp_2.3_covars_esrmean,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_esrmean_cv <- cv.glmnet(
  x = imp_2.4_covars_esrmean,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_esrmean_cv <- cv.glmnet(
  x = imp_2.5_covars_esrmean,
  y = dep_2,
  alpha = 1
)

### esr_gdp ----
#### 2.1
set.seed(15275)
imp_2.1_esrgdp_cv <- cv.glmnet(
  x = imp_2.1_covars_esrgdp,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_esrgdp_cv <- cv.glmnet(
  x = imp_2.2_covars_esrgdp,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_esrgdp_cv <- cv.glmnet(
  x = imp_2.3_covars_esrgdp,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_esrgdp_cv <- cv.glmnet(
  x = imp_2.4_covars_esrgdp,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_esrgdp_cv <- cv.glmnet(
  x = imp_2.5_covars_esrgdp,
  y = dep_2,
  alpha = 1
)

### esr_gdppc ----
#### 2.1
set.seed(15275)
imp_2.1_esrgdppc_cv <- cv.glmnet(
  x = imp_2.1_covars_esrgdppc,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_esrgdppc_cv <- cv.glmnet(
  x = imp_2.2_covars_esrgdppc,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_esrgdppc_cv <- cv.glmnet(
  x = imp_2.3_covars_esrgdppc,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_esrgdppc_cv <- cv.glmnet(
  x = imp_2.4_covars_esrgdppc,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_esrgdppc_cv <- cv.glmnet(
  x = imp_2.5_covars_esrgdppc,
  y = dep_2,
  alpha = 1
)

### both_mean ----
#### 2.1
set.seed(15275)
imp_2.1_bothmean_cv <- cv.glmnet(
  x = imp_2.1_covars_bothmean,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_bothmean_cv <- cv.glmnet(
  x = imp_2.2_covars_bothmean,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_bothmean_cv <- cv.glmnet(
  x = imp_2.3_covars_bothmean,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_bothmean_cv <- cv.glmnet(
  x = imp_2.4_covars_bothmean,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_bothmean_cv <- cv.glmnet(
  x = imp_2.5_covars_bothmean,
  y = dep_2,
  alpha = 1
)

### both_gdp ----
#### 2.1
set.seed(15275)
imp_2.1_bothgdp_cv <- cv.glmnet(
  x = imp_2.1_covars_bothgdp,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_bothgdp_cv <- cv.glmnet(
  x = imp_2.2_covars_bothgdp,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_bothgdp_cv <- cv.glmnet(
  x = imp_2.3_covars_bothgdp,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_bothgdp_cv <- cv.glmnet(
  x = imp_2.4_covars_bothgdp,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_bothgdp_cv <- cv.glmnet(
  x = imp_2.5_covars_bothgdp,
  y = dep_2,
  alpha = 1
)

### both_gdppc ----
#### 2.1
set.seed(15275)
imp_2.1_bothgdppc_cv <- cv.glmnet(
  x = imp_2.1_covars_bothgdppc,
  y = dep_2,
  alpha = 1
)

#### 2.2
set.seed(15275)
imp_2.2_bothgdppc_cv <- cv.glmnet(
  x = imp_2.2_covars_bothgdppc,
  y = dep_2,
  alpha = 1
)

#### 2.3
set.seed(15275)
imp_2.3_bothgdppc_cv <- cv.glmnet(
  x = imp_2.3_covars_bothgdppc,
  y = dep_2,
  alpha = 1
)

### 2.4
set.seed(15275)
imp_2.4_bothgdppc_cv <- cv.glmnet(
  x = imp_2.4_covars_bothgdppc,
  y = dep_2,
  alpha = 1
)

### 2.5
set.seed(15275)
imp_2.5_bothgdppc_cv <- cv.glmnet(
  x = imp_2.5_covars_bothgdppc,
  y = dep_2,
  alpha = 1
)

## imp_3 ----
### cpr_mean ----
#### 3.1
set.seed(15275)
imp_3.1_cprmean_cv <- cv.glmnet(
  x = imp_3.1_covars_cprmean,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_cprmean_cv <- cv.glmnet(
  x = imp_3.2_covars_cprmean,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_cprmean_cv <- cv.glmnet(
  x = imp_3.3_covars_cprmean,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_cprmean_cv <- cv.glmnet(
  x = imp_3.4_covars_cprmean,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_cprmean_cv <- cv.glmnet(
  x = imp_3.5_covars_cprmean,
  y = dep_3,
  alpha = 1
)

### cpr_gdp ----
#### 3.1
set.seed(15275)
imp_3.1_cprgdp_cv <- cv.glmnet(
  x = imp_3.1_covars_cprgdp,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_cprgdp_cv <- cv.glmnet(
  x = imp_3.2_covars_cprgdp,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_cprgdp_cv <- cv.glmnet(
  x = imp_3.3_covars_cprgdp,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_cprgdp_cv <- cv.glmnet(
  x = imp_3.4_covars_cprgdp,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_cprgdp_cv <- cv.glmnet(
  x = imp_3.5_covars_cprgdp,
  y = dep_3,
  alpha = 1
)

### cpr_gdppc ----
#### 3.1
set.seed(15275)
imp_3.1_cprgdppc_cv <- cv.glmnet(
  x = imp_3.1_covars_cprgdppc,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_cprgdppc_cv <- cv.glmnet(
  x = imp_3.2_covars_cprgdppc,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_cprgdppc_cv <- cv.glmnet(
  x = imp_3.3_covars_cprgdppc,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_cprgdppc_cv <- cv.glmnet(
  x = imp_3.4_covars_cprgdppc,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_cprgdppc_cv <- cv.glmnet(
  x = imp_3.5_covars_cprgdppc,
  y = dep_3,
  alpha = 1
)

### esr_mean ----
#### 3.1
set.seed(15275)
imp_3.1_esrmean_cv <- cv.glmnet(
  x = imp_3.1_covars_esrmean,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_esrmean_cv <- cv.glmnet(
  x = imp_3.2_covars_esrmean,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_esrmean_cv <- cv.glmnet(
  x = imp_3.3_covars_esrmean,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_esrmean_cv <- cv.glmnet(
  x = imp_3.4_covars_esrmean,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_esrmean_cv <- cv.glmnet(
  x = imp_3.5_covars_esrmean,
  y = dep_3,
  alpha = 1
)

### esr_gdp ----
#### 3.1
set.seed(15275)
imp_3.1_esrgdp_cv <- cv.glmnet(
  x = imp_3.1_covars_esrgdp,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_esrgdp_cv <- cv.glmnet(
  x = imp_3.2_covars_esrgdp,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_esrgdp_cv <- cv.glmnet(
  x = imp_3.3_covars_esrgdp,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_esrgdp_cv <- cv.glmnet(
  x = imp_3.4_covars_esrgdp,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_esrgdp_cv <- cv.glmnet(
  x = imp_3.5_covars_esrgdp,
  y = dep_3,
  alpha = 1
)

### esr_gdppc ----
#### 3.1
set.seed(15275)
imp_3.1_esrgdppc_cv <- cv.glmnet(
  x = imp_3.1_covars_esrgdppc,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_esrgdppc_cv <- cv.glmnet(
  x = imp_3.2_covars_esrgdppc,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_esrgdppc_cv <- cv.glmnet(
  x = imp_3.3_covars_esrgdppc,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_esrgdppc_cv <- cv.glmnet(
  x = imp_3.4_covars_esrgdppc,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_esrgdppc_cv <- cv.glmnet(
  x = imp_3.5_covars_esrgdppc,
  y = dep_3,
  alpha = 1
)

### both_mean ----
#### 3.1
set.seed(15275)
imp_3.1_bothmean_cv <- cv.glmnet(
  x = imp_3.1_covars_bothmean,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_bothmean_cv <- cv.glmnet(
  x = imp_3.2_covars_bothmean,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_bothmean_cv <- cv.glmnet(
  x = imp_3.3_covars_bothmean,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_bothmean_cv <- cv.glmnet(
  x = imp_3.4_covars_bothmean,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_bothmean_cv <- cv.glmnet(
  x = imp_3.5_covars_bothmean,
  y = dep_3,
  alpha = 1
)

### both_gdp ----
#### 3.1
set.seed(15275)
imp_3.1_bothgdp_cv <- cv.glmnet(
  x = imp_3.1_covars_bothgdp,
  y = dep_3,
  alpha = 1
)

#### 3.2
set.seed(15275)
imp_3.2_bothgdp_cv <- cv.glmnet(
  x = imp_3.2_covars_bothgdp,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_bothgdp_cv <- cv.glmnet(
  x = imp_3.3_covars_bothgdp,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_bothgdp_cv <- cv.glmnet(
  x = imp_3.4_covars_bothgdp,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_bothgdp_cv <- cv.glmnet(
  x = imp_3.5_covars_bothgdp,
  y = dep_3,
  alpha = 1
)

### both_gdppc ----
#### 3.1
set.seed(15275)
imp_3.1_bothgdppc_cv <- cv.glmnet(
  x = imp_3.1_covars_bothgdppc,
  y = dep_3,
  alpha = 1
)

## imp 1 ----
### cpr_mean ----
imp_1.1_cprmean_minlam <- imp_1.1_cprmean_cv$lambda.min
imp_1.2_cprmean_minlam <- imp_1.2_cprmean_cv$lambda.min
imp_1.3_cprmean_minlam <- imp_1.3_cprmean_cv$lambda.min
imp_1.4_cprmean_minlam <- imp_1.4_cprmean_cv$lambda.min
imp_1.5_cprmean_minlam <- imp_1.5_cprmean_cv$lambda.min

### cpr_gdp ----
imp_1.1_cprgdp_minlam <- imp_1.1_cprgdp_cv$lambda.min
imp_1.2_cprgdp_minlam <- imp_1.2_cprgdp_cv$lambda.min
imp_1.3_cprgdp_minlam <- imp_1.3_cprgdp_cv$lambda.min
imp_1.4_cprgdp_minlam <- imp_1.4_cprgdp_cv$lambda.min
imp_1.5_cprgdp_minlam <- imp_1.5_cprgdp_cv$lambda.min

### cpr_gdppc ----
imp_1.1_cprgdppc_minlam <- imp_1.1_cprgdppc_cv$lambda.min
imp_1.2_cprgdppc_minlam <- imp_1.2_cprgdppc_cv$lambda.min
imp_1.3_cprgdppc_minlam <- imp_1.3_cprgdppc_cv$lambda.min
imp_1.4_cprgdppc_minlam <- imp_1.4_cprgdppc_cv$lambda.min
imp_1.5_cprgdppc_minlam <- imp_1.5_cprgdppc_cv$lambda.min

### esr_mean ----
imp_1.1_esrmean_minlam <- imp_1.1_esrmean_cv$lambda.min
imp_1.2_esrmean_minlam <- imp_1.2_esrmean_cv$lambda.min
imp_1.3_esrmean_minlam <- imp_1.3_esrmean_cv$lambda.min
imp_1.4_esrmean_minlam <- imp_1.4_esrmean_cv$lambda.min
imp_1.5_esrmean_minlam <- imp_1.5_esrmean_cv$lambda.min

### esr_gdp ----
imp_1.1_esrgdp_minlam <- imp_1.1_esrgdp_cv$lambda.min
imp_1.2_esrgdp_minlam <- imp_1.2_esrgdp_cv$lambda.min
imp_1.3_esrgdp_minlam <- imp_1.3_esrgdp_cv$lambda.min
imp_1.4_esrgdp_minlam <- imp_1.4_esrgdp_cv$lambda.min
imp_1.5_esrgdp_minlam <- imp_1.5_esrgdp_cv$lambda.min

### esr_gdppc ----
imp_1.1_esrgdppc_minlam <- imp_1.1_esrgdppc_cv$lambda.min
imp_1.2_esrgdppc_minlam <- imp_1.2_esrgdppc_cv$lambda.min
imp_1.3_esrgdppc_minlam <- imp_1.3_esrgdppc_cv$lambda.min
imp_1.4_esrgdppc_minlam <- imp_1.4_esrgdppc_cv$lambda.min
imp_1.5_esrgdppc_minlam <- imp_1.5_esrgdppc_cv$lambda.min

### both_mean ----
imp_1.1_bothmean_minlam <- imp_1.1_bothmean_cv$lambda.min
imp_1.2_bothmean_minlam <- imp_1.2_bothmean_cv$lambda.min
imp_1.3_bothmean_minlam <- imp_1.3_bothmean_cv$lambda.min
imp_1.4_bothmean_minlam <- imp_1.4_bothmean_cv$lambda.min
imp_1.5_bothmean_minlam <- imp_1.5_bothmean_cv$lambda.min

### both_gdp ----
imp_1.1_bothgdp_minlam <- imp_1.1_bothgdp_cv$lambda.min
imp_1.2_bothgdp_minlam <- imp_1.2_bothgdp_cv$lambda.min
imp_1.3_bothgdp_minlam <- imp_1.3_bothgdp_cv$lambda.min
imp_1.4_bothgdp_minlam <- imp_1.4_bothgdp_cv$lambda.min
imp_1.5_bothgdp_minlam <- imp_1.5_bothgdp_cv$lambda.min

### both_gdppc ----
imp_1.1_bothgdppc_minlam <- imp_1.1_bothgdppc_cv$lambda.min
imp_1.2_bothgdppc_minlam <- imp_1.2_bothgdppc_cv$lambda.min
imp_1.3_bothgdppc_minlam <- imp_1.3_bothgdppc_cv$lambda.min
imp_1.4_bothgdppc_minlam <- imp_1.4_bothgdppc_cv$lambda.min
imp_1.5_bothgdppc_minlam <- imp_1.5_bothgdppc_cv$lambda.min

## imp_2 ----
### cpr_mean ----
imp_2.1_cprmean_minlam <- imp_2.1_cprmean_cv$lambda.min
imp_2.2_cprmean_minlam <- imp_2.2_cprmean_cv$lambda.min
imp_2.3_cprmean_minlam <- imp_2.3_cprmean_cv$lambda.min
imp_2.4_cprmean_minlam <- imp_2.4_cprmean_cv$lambda.min
imp_2.5_cprmean_minlam <- imp_2.5_cprmean_cv$lambda.min

imp_2_cprmean_lam <- (imp_2.1_cprmean_minlam + imp_2.2_cprmean_minlam + imp_2.3_cprmean_minlam + imp_2.4_cprmean_minlam + imp_2.5_cprmean_minlam)/5

### cpr_gdp ----
imp_2.1_cprgdp_minlam <- imp_2.1_cprgdp_cv$lambda.min
imp_2.2_cprgdp_minlam <- imp_2.2_cprgdp_cv$lambda.min
imp_2.3_cprgdp_minlam <- imp_2.3_cprgdp_cv$lambda.min
imp_2.4_cprgdp_minlam <- imp_2.4_cprgdp_cv$lambda.min
imp_2.5_cprgdp_minlam <- imp_2.5_cprgdp_cv$lambda.min

imp_2_cprgdp_lam <- (imp_2.1_cprgdp_minlam + imp_2.2_cprgdp_minlam + imp_2.3_cprgdp_minlam + imp_2.4_cprgdp_minlam + imp_2.5_cprgdp_minlam)/5

### cpr_gdppc ----
imp_2.1_cprgdppc_minlam <- imp_2.1_cprgdppc_cv$lambda.min
imp_2.2_cprgdppc_minlam <- imp_2.2_cprgdppc_cv$lambda.min
imp_2.3_cprgdppc_minlam <- imp_2.3_cprgdppc_cv$lambda.min
imp_2.4_cprgdppc_minlam <- imp_2.4_cprgdppc_cv$lambda.min
imp_2.5_cprgdppc_minlam <- imp_2.5_cprgdppc_cv$lambda.min

imp_2_cprgdppc_lam <- (imp_2.1_cprgdppc_minlam + imp_2.2_cprgdppc_minlam + imp_2.3_cprgdppc_minlam + imp_2.4_cprgdppc_minlam + imp_2.5_cprgdppc_minlam)/5

### esr_mean ----
imp_2.1_esrmean_minlam <- imp_2.1_esrmean_cv$lambda.min
imp_2.2_esrmean_minlam <- imp_2.2_esrmean_cv$lambda.min
imp_2.3_esrmean_minlam <- imp_2.3_esrmean_cv$lambda.min
imp_2.4_esrmean_minlam <- imp_2.4_esrmean_cv$lambda.min
imp_2.5_esrmean_minlam <- imp_2.5_esrmean_cv$lambda.min

imp_2_esrmean_lam <- (imp_2.1_esrmean_minlam + imp_2.2_esrmean_minlam + imp_2.3_esrmean_minlam + imp_2.4_esrmean_minlam + imp_2.5_esrmean_minlam)/5

### esr_gdp ----
imp_2.1_esrgdp_minlam <- imp_2.1_esrgdp_cv$lambda.min
imp_2.2_esrgdp_minlam <- imp_2.2_esrgdp_cv$lambda.min
imp_2.3_esrgdp_minlam <- imp_2.3_esrgdp_cv$lambda.min
imp_2.4_esrgdp_minlam <- imp_2.4_esrgdp_cv$lambda.min
imp_2.5_esrgdp_minlam <- imp_2.5_esrgdp_cv$lambda.min

imp_2_esrgdp_lam <- (imp_2.1_esrgdp_minlam + imp_2.2_esrgdp_minlam + imp_2.3_esrgdp_minlam + imp_2.4_esrgdp_minlam + imp_2.5_esrgdp_minlam)/5

### esr_gdppc ----
imp_2.1_esrgdppc_minlam <- imp_2.1_esrgdppc_cv$lambda.min
imp_2.2_esrgdppc_minlam <- imp_2.2_esrgdppc_cv$lambda.min
imp_2.3_esrgdppc_minlam <- imp_2.3_esrgdppc_cv$lambda.min
imp_2.4_esrgdppc_minlam <- imp_2.4_esrgdppc_cv$lambda.min
imp_2.5_esrgdppc_minlam <- imp_2.5_esrgdppc_cv$lambda.min

imp_2_esrgdppc_lam <- (imp_2.1_esrgdppc_minlam + imp_2.2_esrgdppc_minlam + imp_2.3_esrgdppc_minlam + imp_2.4_esrgdppc_minlam + imp_2.5_esrgdppc_minlam)/5

### both_mean ----
imp_2.1_bothmean_minlam <- imp_2.1_bothmean_cv$lambda.min
imp_2.2_bothmean_minlam <- imp_2.2_bothmean_cv$lambda.min
imp_2.3_bothmean_minlam <- imp_2.3_bothmean_cv$lambda.min
imp_2.4_bothmean_minlam <- imp_2.4_bothmean_cv$lambda.min
imp_2.5_bothmean_minlam <- imp_2.5_bothmean_cv$lambda.min

imp_2_bothmean_lam <- (imp_2.1_bothmean_minlam + imp_2.2_bothmean_minlam + imp_2.3_bothmean_minlam + imp_2.4_bothmean_minlam + imp_2.5_bothmean_minlam)/5

### both_gdp ----
imp_2.1_bothgdp_minlam <- imp_2.1_bothgdp_cv$lambda.min
imp_2.2_bothgdp_minlam <- imp_2.2_bothgdp_cv$lambda.min
imp_2.3_bothgdp_minlam <- imp_2.3_bothgdp_cv$lambda.min
imp_2.4_bothgdp_minlam <- imp_2.4_bothgdp_cv$lambda.min
imp_2.5_bothgdp_minlam <- imp_2.5_bothgdp_cv$lambda.min

imp_2_bothgdp_lam <- (imp_2.1_bothgdp_minlam + imp_2.2_bothgdp_minlam + imp_2.3_bothgdp_minlam + imp_2.4_bothgdp_minlam + imp_2.5_bothgdp_minlam)/5

### both_gdppc ----
imp_2.1_bothgdppc_minlam <- imp_2.1_bothgdppc_cv$lambda.min
imp_2.2_bothgdppc_minlam <- imp_2.2_bothgdppc_cv$lambda.min
imp_2.3_bothgdppc_minlam <- imp_2.3_bothgdppc_cv$lambda.min
imp_2.4_bothgdppc_minlam <- imp_2.4_bothgdppc_cv$lambda.min
imp_2.5_bothgdppc_minlam <- imp_2.5_bothgdppc_cv$lambda.min

imp_2_bothgdppc_lam <- (imp_2.1_bothgdppc_minlam + imp_2.2_bothgdppc_minlam + imp_2.3_bothgdppc_minlam + imp_2.4_bothgdppc_minlam + imp_2.5_bothgdppc_minlam)/5

## imp_3 ----
### cpr_mean ----
imp_3.1_cprmean_minlam <- imp_3.1_cprmean_cv$lambda.min
imp_3.2_cprmean_minlam <- imp_3.2_cprmean_cv$lambda.min
imp_3.3_cprmean_minlam <- imp_3.3_cprmean_cv$lambda.min
imp_3.4_cprmean_minlam <- imp_3.4_cprmean_cv$lambda.min
imp_3.5_cprmean_minlam <- imp_3.5_cprmean_cv$lambda.min

imp_3_cprmean_lam <- (imp_3.1_cprmean_minlam + imp_3.2_cprmean_minlam + imp_3.3_cprmean_minlam + imp_3.4_cprmean_minlam + imp_3.5_cprmean_minlam)/5

### cpr_gdp ----
imp_3.1_cprgdp_minlam <- imp_3.1_cprgdp_cv$lambda.min
imp_3.2_cprgdp_minlam <- imp_3.2_cprgdp_cv$lambda.min
imp_3.3_cprgdp_minlam <- imp_3.3_cprgdp_cv$lambda.min
imp_3.4_cprgdp_minlam <- imp_3.4_cprgdp_cv$lambda.min
imp_3.5_cprgdp_minlam <- imp_3.5_cprgdp_cv$lambda.min

imp_3_cprgdp_lam <- (imp_3.1_cprgdp_minlam + imp_3.2_cprgdp_minlam + imp_3.3_cprgdp_minlam + imp_3.4_cprgdp_minlam + imp_3.5_cprgdp_minlam)/5

### cpr_gdppc ----
imp_3.1_cprgdppc_minlam <- imp_3.1_cprgdppc_cv$lambda.min
imp_3.2_cprgdppc_minlam <- imp_3.2_cprgdppc_cv$lambda.min
imp_3.3_cprgdppc_minlam <- imp_3.3_cprgdppc_cv$lambda.min
imp_3.4_cprgdppc_minlam <- imp_3.4_cprgdppc_cv$lambda.min
imp_3.5_cprgdppc_minlam <- imp_3.5_cprgdppc_cv$lambda.min

imp_3_cprgdppc_lam <- (imp_3.1_cprgdppc_minlam + imp_3.2_cprgdppc_minlam + imp_3.3_cprgdppc_minlam + imp_3.4_cprgdppc_minlam + imp_3.5_cprgdppc_minlam)/5

### esr_mean ----
imp_3.1_esrmean_minlam <- imp_3.1_esrmean_cv$lambda.min
imp_3.2_esrmean_minlam <- imp_3.2_esrmean_cv$lambda.min
imp_3.3_esrmean_minlam <- imp_3.3_esrmean_cv$lambda.min
imp_3.4_esrmean_minlam <- imp_3.4_esrmean_cv$lambda.min
imp_3.5_esrmean_minlam <- imp_3.5_esrmean_cv$lambda.min

imp_3_esrmean_lam <- (imp_3.1_esrmean_minlam + imp_3.2_esrmean_minlam + imp_3.3_esrmean_minlam + imp_3.4_esrmean_minlam + imp_3.5_esrmean_minlam)/5

### esr_gdp ----
imp_3.1_esrgdp_minlam <- imp_3.1_esrgdp_cv$lambda.min
imp_3.2_esrgdp_minlam <- imp_3.2_esrgdp_cv$lambda.min
imp_3.3_esrgdp_minlam <- imp_3.3_esrgdp_cv$lambda.min
imp_3.4_esrgdp_minlam <- imp_3.4_esrgdp_cv$lambda.min
imp_3.5_esrgdp_minlam <- imp_3.5_esrgdp_cv$lambda.min

imp_3_esrgdp_lam <- (imp_3.1_esrgdp_minlam + imp_3.2_esrgdp_minlam + imp_3.3_esrgdp_minlam + imp_3.4_esrgdp_minlam + imp_3.5_esrgdp_minlam)/5

### esr_gdppc ----
imp_3.1_esrgdppc_minlam <- imp_3.1_esrgdppc_cv$lambda.min
imp_3.2_esrgdppc_minlam <- imp_3.2_esrgdppc_cv$lambda.min
imp_3.3_esrgdppc_minlam <- imp_3.3_esrgdppc_cv$lambda.min
imp_3.4_esrgdppc_minlam <- imp_3.4_esrgdppc_cv$lambda.min
imp_3.5_esrgdppc_minlam <- imp_3.5_esrgdppc_cv$lambda.min

imp_3_esrgdppc_lam <- (imp_3.1_esrgdppc_minlam + imp_3.2_esrgdppc_minlam + imp_3.3_esrgdppc_minlam + imp_3.4_esrgdppc_minlam + imp_3.5_esrgdppc_minlam)/5

### both_mean ----
imp_3.1_bothmean_minlam <- imp_3.1_bothmean_cv$lambda.min
imp_3.2_bothmean_minlam <- imp_3.2_bothmean_cv$lambda.min
imp_3.3_bothmean_minlam <- imp_3.3_bothmean_cv$lambda.min
imp_3.4_bothmean_minlam <- imp_3.4_bothmean_cv$lambda.min
imp_3.5_bothmean_minlam <- imp_3.5_bothmean_cv$lambda.min

imp_3_bothmean_lam <- (imp_3.1_bothmean_minlam + imp_3.2_bothmean_minlam + imp_3.3_bothmean_minlam + imp_3.4_bothmean_minlam + imp_3.5_bothmean_minlam)/5

### both_gdp ----
imp_3.1_bothgdp_minlam <- imp_3.1_bothgdp_cv$lambda.min
imp_3.2_bothgdp_minlam <- imp_3.2_bothgdp_cv$lambda.min
imp_3.3_bothgdp_minlam <- imp_3.3_bothgdp_cv$lambda.min
imp_3.4_bothgdp_minlam <- imp_3.4_bothgdp_cv$lambda.min
imp_3.5_bothgdp_minlam <- imp_3.5_bothgdp_cv$lambda.min

imp_3_bothgdp_lam <- (imp_3.1_bothgdp_minlam + imp_3.2_bothgdp_minlam + imp_3.3_bothgdp_minlam + imp_3.4_bothgdp_minlam + imp_3.5_bothgdp_minlam)/5

### both_gdppc ----
imp_3.1_bothgdppc_minlam <- imp_3.1_bothgdppc_cv$lambda.min
imp_3.2_bothgdppc_minlam <- imp_3.2_bothgdppc_cv$lambda.min
imp_3.3_bothgdppc_minlam <- imp_3.3_bothgdppc_cv$lambda.min
imp_3.4_bothgdppc_minlam <- imp_3.4_bothgdppc_cv$lambda.min
imp_3.5_bothgdppc_minlam <- imp_3.5_bothgdppc_cv$lambda.min

imp_3_bothgdppc_lam <- (imp_3.1_bothgdppc_minlam + imp_3.2_bothgdppc_minlam + imp_3.3_bothgdppc_minlam + imp_3.4_bothgdppc_minlam + imp_3.5_bothgdppc_minlam)/5
#### 3.2
set.seed(15275)
imp_3.2_bothgdppc_cv <- cv.glmnet(
  x = imp_3.2_covars_bothgdppc,
  y = dep_3,
  alpha = 1
)

#### 3.3
set.seed(15275)
imp_3.3_bothgdppc_cv <- cv.glmnet(
  x = imp_3.3_covars_bothgdppc,
  y = dep_3,
  alpha = 1
)

### 3.4
set.seed(15275)
imp_3.4_bothgdppc_cv <- cv.glmnet(
  x = imp_3.4_covars_bothgdppc,
  y = dep_3,
  alpha = 1
)

### 3.5
set.seed(15275)
imp_3.5_bothgdppc_cv <- cv.glmnet(
  x = imp_3.5_covars_bothgdppc,
  y = dep_3,
  alpha = 1
)
