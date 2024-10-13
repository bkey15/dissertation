###Space, Time, Space vs. Time#####
###02/24/2021######################
###Replication Analyses##################

#Generic working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Get packages, attach them
pks=c("cshapes", "dplyr", "Matrix", "spdep", "spatialreg", "stargazer", "lubridate", "tidyverse", "DataCombine")
needed=setdiff(pks,installed.packages())

if( length(needed) > 0){
  options(Ncpus=parallel::detectCores() %/% 1.25)
  install.packages(needed)
}

sapply(pks, library, character.only=T)
session = sessionInfo()

#Source NT W maker
source("./ntspmat.R")
source("./stadl_models.R")
source("./st_effects.R")

#Reanalysis of Acemoglu et al. (2008) on Development and Democracy
#To Replicate Table 1, Column 1.

data<-read.csv("./aer_5year_APSR_full.csv")

data$country<-recode_factor(data$country,"Korea, Rep."="Korea, Republic of")
data$country<-recode_factor(data$country,"Egypt, Arab Rep."="Egypt")
data$country<-recode_factor(data$country,"Gambia, The"="Gambia")
data$country<-recode_factor(data$country,"Ethiopia 1993-"="Ethiopia")
data$country<-recode_factor(data$country,"Ethiopia -pre 1993"="Ethiopia")
data$country<-recode_factor(data$country,"Pakistan-pre-1972"="Pakistan")
data$country<-recode_factor(data$country,"Pakistan-post-1972"="Pakistan")
data$country<-recode_factor(data$country,"Belarus"="Belarus (Byelorussia)")
data$country<-recode_factor(data$country,"Syrian Arab Republic"="Syria")
data$country<-recode_factor(data$country,"Venezuela, RB"="Venezuela")
data$country<-recode_factor(data$country,"Vietnam"="Vietnam, Democratic Republic of")
data$country<-recode_factor(data$country,"Russia"="Russia (Soviet Union)")
data$country<-recode_factor(data$country,"Yemen"="Yemen (Arab Republic of Yemen)")
data$country<-recode_factor(data$country,"United States"="United States of America")
data$country<-recode_factor(data$country,"Tanzania"="Tanzania (Tanganyika)")
data$country<-recode_factor(data$country,"Burkina Faso"="Burkina Faso (Upper Volta)")
data$country<-recode_factor(data$country,"Cambodia"="Cambodia (Kampuchea)")
data$country<-recode_factor(data$country,"Zimbabwe"="Zimbabwe (Rhodesia)")
data$country<-recode_factor(data$country,"Cote d'Ivoire"="Cote D'Ivoire")
data$country<-recode_factor(data$country,"Germany"="German Federal Republic")
data$country<-recode_factor(data$country,"Iran"="Iran (Persia)")
data$country<-recode_factor(data$country,"Italy"="Italy/Sardinia")
data$country<-recode_factor(data$country,"Turkey"="Turkey (Ottoman Empire)")
data$country<-recode_factor(data$country,"Congo, Dem. Rep."="Congo, Democratic Republic of (Zaire)")
data$country<-recode_factor(data$country,"Congo, Rep."="Congo")
data$country<-recode_factor(data$country,"Kyrgyzstan"="Kyrgyz Republic")
data$country<-recode_factor(data$country,"Madagascar"="Madagascar (Malagasy)")
data$country<-recode_factor(data$country,"Sri Lanka"="Sri Lanka (Ceylon)")
data$country<-recode_factor(data$country,"Macedonia, FYR"="Macedonia (FYROM/North Macedonia)")
data$country<-recode_factor(data$country,"Romania"="Rumania")

reg<-lm(formula = polity4 ~ lrgdpchL,  data = data)
wm <- make_ntspmat(reg,country,year,10)
sar <- ntspreg(reg,wm)
summary(sar)
BIC(sar)


#To Replicate Table 1, Column 2.
reg2<-lm(formula = polity4 ~ lrgdpchL + as.factor(year), data = data)
wm2 <- make_ntspmat(reg2,country,year,10)
sar2 <- ntspreg(reg2,wm2)
summary(sar2)
BIC(sar2)


#To Replicate Table 1, Column 3.
reg3<-lm(formula = polity4 ~ lrgdpchL + as.factor(country) + as.factor(year), data = data)
wm3 <- make_ntspmat(reg3,country,year,10)
sar3 <- ntspreg(reg3,wm3)
summary(sar3)
BIC(sar3)

#To Replicate Table 1, Column 4.
reg4<-lm(formula = polity4 ~ polity4L + lrgdpchL + as.factor(country) + as.factor(year), data = data)
summary(reg4)
logLik(reg4)
BIC(reg4)

#To Replicate Table 1, Column 5.
reg5<-lm(formula = polity4 ~ polity4L + lrgdpchL + as.factor(year), data = data)
wm5 <- make_ntspmat(reg5,country,year,10)
sar5 <- ntspreg(reg5,wm5)
summary(sar5)
BIC(sar5)

#Spatiotemporal Effects
st_effects(sar5,wm5,polity4L,lrgdpchL)

s#Reanalysis of Luhrmann et al. (2020) on Accountability and Infant Mortality
#To Replicate Table 2, Column 1.

data<-read.csv("./accountability_data_regressions.csv")

reg<-lm(formula = infant ~ Accountability + aid + loggdp + gdp_grow +
    resourcesdep_hm + gini2 + lnpop + urban_cow + violence_domestic +
    communist + rx_infant + v2x_corr + as.factor(country_name) + as.factor(year), data = data)
summary(reg)

#To Replicate Table 2, Column 2.

data <- slide(data = data, Var = 'infant', GroupVar = 'country_id',
  TimeVar='year', NewVar = 'lag_inf', slideBy = -1)

data$country_name<-recode_factor(data$country_name,"Burma/Myanmar"="Myanmar (Burma)")
data$country_name<-recode_factor(data$country_name,"Korea, South"="Korea, Republic of")
data$country_name<-recode_factor(data$country_name,"Russia"="Russia (Soviet Union)")
data$country_name<-recode_factor(data$country_name,"Yemen"="Yemen (Arab Republic of Yemen)")
data$country_name<-recode_factor(data$country_name,"United States"="United States of America")
data$country_name<-recode_factor(data$country_name,"Kosovo"="Yugoslavia/Serbia")
data$country_name<-recode_factor(data$country_name,"Tanzania"="Tanzania (Tanganyika)")
data$country_name<-recode_factor(data$country_name,"Burkina Faso"="Burkina Faso (Upper Volta)")
data$country_name<-recode_factor(data$country_name,"Cambodia"="Cambodia (Kampuchea)")
data$country_name<-recode_factor(data$country_name,"Zimbabwe"="Zimbabwe (Rhodesia)")
data$country_name<-recode_factor(data$country_name,"Ivory Coast"="Cote D'Ivoire")
data$country_name<-recode_factor(data$country_name,"Germany"="German Federal Republic")
data$country_name<-recode_factor(data$country_name,"Iran"="Iran (Persia)")
data$country_name<-recode_factor(data$country_name,"Italy"="Italy/Sardinia")
data$country_name<-recode_factor(data$country_name,"Turkey"="Turkey (Ottoman Empire)")
data$country_name<-recode_factor(data$country_name,"Belarus"="Belarus (Byelorussia)")
data$country_name<-recode_factor(data$country_name,"Congo, Democratic Republic of"="Congo, Democratic Republic of (Zaire)")
data$country_name<-recode_factor(data$country_name,"Congo, Republic of the"="Congo")
data$country_name<-recode_factor(data$country_name,"Kyrgyzstan"="Kyrgyz Republic")
data$country_name<-recode_factor(data$country_name,"Madagascar"="Madagascar (Malagasy)")
data$country_name<-recode_factor(data$country_name,"Sri Lanka"="Sri Lanka (Ceylon)")
data$country_name<-recode_factor(data$country_name,"Swaziland"="Swaziland (Eswatini)")
data$country_name<-recode_factor(data$country_name,"Bosnia and Herzegovina"="Bosnia-Herzegovina")
data$country_name<-recode_factor(data$country_name,"Macedonia"="Macedonia (FYROM/North Macedonia)")
data$country_name<-recode_factor(data$country_name,"Romania"="Rumania")

data$diff_inf <- data$infant - data$lag_inf


reg2<-lm(formula = diff_inf ~ lag_inf + Accountability + aid + loggdp + gdp_grow +
    resourcesdep_hm + gini2 + lnpop + urban_cow + violence_domestic + rx_infant +
    communist + v2x_corr + as.factor(year) + as.factor(country_name), data = data)

wm2 <- make_ntspmat(reg2,country_name,year,5)
sar2 <- ntspreg(reg2,wm2)
summary(sar2)

#To Replicate Table 2, Column 3.

reg3<-lm(formula = diff_inf ~ lag_inf + Accountability + aid + loggdp + gdp_grow +
    resourcesdep_hm + gini2 + lnpop + urban_cow + violence_domestic + rx_infant +
    communist + v2x_corr + as.factor(year), data = data)

wm3 <- make_ntspmat(reg3,country_name,year,5)
w3 <- as.matrix(wm3[[2]])
sar3 <- ntspreg(reg3,w3)
summary(sar3)


