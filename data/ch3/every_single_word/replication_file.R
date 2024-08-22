library(ggplot2)
library(readstata13)
library(dplyr)
library(stringr)
library(tidyr)
library(ggpubr)
library(gridExtra)


##############################################################
########################## FIGURE 1 ########################## 
##############################################################
### load 'every single word' data
load("C:/Users/corin/Dropbox/Drucksachen Bundestag/FINAL DATA/every_single_word.RData")

### load 'requests' data
load("C:/Users/corin/Dropbox/Drucksachen Bundestag/FINAL DATA/request_split.RData")


### collapse 'every single word' by year and type of document
every_single_word$year <- as.numeric(format(every_single_word$date, "%Y"))

### categories for document types
every_single_word$document_type2 <- NA
every_single_word$document_type2 <- ifelse(grepl("Gesetz",every_single_word$document_type), "bill or decree", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Berichtigung", "bill or decree", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="?nderungsantrag", "bill or decree", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Verordnung", "bill or decree", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Anordnung", "bill or decree", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Zusammenstellung", "bill or decree", every_single_word$document_type2)

every_single_word$document_type2 <- ifelse(grepl("Antrag",every_single_word$document_type), "proposal", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(grepl("antrag",every_single_word$document_type), "proposal", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Begr?ndung", "proposal", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Wahlvorschlag", "proposal", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(grepl("entwurf",every_single_word$document_type), "proposal", every_single_word$document_type2)

every_single_word$document_type2 <- ifelse(grepl("Antwort",every_single_word$document_type), "response or briefing", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(grepl("Unterrichtung",every_single_word$document_type), "response or briefing", every_single_word$document_type2)

every_single_word$document_type2 <- ifelse(grepl("Bericht",every_single_word$document_type), "report", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(grepl("bericht",every_single_word$document_type), "report", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Beschlussempfehlung", "report", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(grepl("Beschlussempfehlung",every_single_word$document_type), "report", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="?bersicht", "report", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Sammel?bersicht", "report", every_single_word$document_type2)

every_single_word$document_type2 <- ifelse(grepl("Ordnungsruf",every_single_word$document_type), "others", every_single_word$document_type2)

every_single_word$document_type2 <- ifelse(grepl("Anfrage",every_single_word$document_type), "request", every_single_word$document_type2)
every_single_word$document_type2 <- ifelse(every_single_word$document_type=="Interpellation", "request", every_single_word$document_type2)



### collapse every single word by year and document type
every_single_word2 <- every_single_word %>%
  group_by(year, document_type2) %>%
  summarize(n_documents =n())


### collapse requests split by document number
requests_split$year <- as.numeric(format(requests_split$date, "%Y"))

requests_split2 <- requests_split %>%
  group_by(number) %>%
  summarize(year =first(year))
requests_split2$document_type2 <- "request"

### collapse requests split by year
requests_split3 <- requests_split2 %>%
  group_by(year) %>%
  summarize(n_documents=n(), document_type2 =first(document_type2))


every_single_word3 <- rbind.data.frame(every_single_word2, requests_split3, stringsAsFactors = FALSE)

### collapse if twice request
every_single_word4 <- every_single_word3 %>%
  group_by(year, document_type2) %>%
  summarize(n_documents =sum(n_documents))

every_single_word4$document_type2 <- factor(every_single_word4$document_type2, 
                                            levels = c("others", "request",  "bill or decree", "report", 
                                                      "proposal",  "response or briefing"))




ggplot(every_single_word4, aes(x=year, y=n_documents, fill=document_type2)) + 
  geom_line(aes(linetype=document_type2, color=document_type2), size=c(1))+
  scale_linetype_manual(values=c("solid","longdash", "dotted", "longdash", "solid", "dotted"), name = " ")+
  scale_color_manual(values=c("gray46", "black", "gray73", "gray73", "gray46", "black"), name = " ")+
  theme_classic()+
  ylab("N documents")+
  xlab("year")

  
##############################################################
########################## FIGURE 3 ########################## 
##############################################################

#load full dataset
load("C:/Users/Remschel/Dropbox/Drucksachen Bundestag/FINAL DATA/every_single_word.RData")

#subset document types to be analysed
#proposals
Antrag_X <- subset(every_single_word, document_type=="Änderungsantrag" | document_type=="Antrag" | document_type=="Entschließungsantrag")
#(minor) interpellations
Anfrage_X <- subset(every_single_word, document_type=="Kleine Anfrage" | document_type=="Interpellation")

#exclude 17th and 18th legislative period
Antrag_X <- subset(Antrag_X, elec_period!="18" & elec_period!="17")
Anfrage_X <- subset(Anfrage_X, elec_period!=18 & elec_period!=17)

#subset documents that were not tabled by a fraction, but by MPs
#proposals
Antrag_X <- subset(Antrag_X, author_mp_1!="" | !is.na(author_mp_1))
Antrag_X <- subset(Antrag_X, author_i_1=="" | is.na(author_i_1))

#interpellations
Anfrage_X <- subset(Anfrage_X, author_mp_1!="" | !is.na(author_mp_1))
Anfrage_X <- subset(Anfrage_X, author_i_1=="" | is.na(author_i_1))


#split documents by legislative period and count number of tabled proposals/interpellations per individual MP 
#and prepare data for later merging with Sieberer et al.'s mp_characteristics dataset
#proposals
Antrag_X_1 <- subset(Antrag_X, elec_period=="1")
Antrag_count1 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count1$WP <- "1"
Antrag_count1$mp_id <- Antrag_count1$Var1
Antrag_count1$Var1 <- NULL

Antrag_X_2 <- subset(Antrag_X, elec_period=="2")
Antrag_count2 <- as.data.frame(table(unlist(Antrag_X_2[464:911])))
Antrag_count2$WP <- "2"
Antrag_count2$mp_id <- Antrag_count2$Var1
Antrag_count2$Var1 <- NULL

#legislative period 3
Antrag_X_3 <- subset(Antrag_X, elec_period=="3")
Antrag_count3 <- as.data.frame(table(unlist(Antrag_X_3[464:911])))
Antrag_count3$WP <- "3"
Antrag_count3$mp_id <- Antrag_count3$Var1
Antrag_count3$Var1 <- NULL

Antrag_X_4 <- subset(Antrag_X, elec_period=="4")
Antrag_count4 <- as.data.frame(table(unlist(Antrag_X_4[464:911])))
Antrag_count4$WP <- "4"
Antrag_count4$mp_id <- Antrag_count4$Var1
Antrag_count4$Var1 <- NULL

Antrag_X_5 <- subset(Antrag_X, elec_period=="5")
Antrag_count5 <- as.data.frame(table(unlist(Antrag_X_5[464:911])))
Antrag_count5$WP <- "5"
Antrag_count5$mp_id <- Antrag_count5$Var1
Antrag_count5$Var1 <- NULL

Antrag_X_6 <- subset(Antrag_X, elec_period=="6")
Antrag_count6 <- as.data.frame(table(unlist(Antrag_X_6[464:911])))
Antrag_count6$WP <- "6"
Antrag_count6$mp_id <- Antrag_count6$Var1
Antrag_count6$Var1 <- NULL

Antrag_X_7 <- subset(Antrag_X, elec_period=="7")
Antrag_count7 <- as.data.frame(table(unlist(Antrag_X_7[464:911])))
Antrag_count7$WP <- "7"
Antrag_count7$mp_id <- Antrag_count7$Var1
Antrag_count7$Var1 <- NULL

Antrag_X_8 <- subset(Antrag_X, elec_period=="8")
Antrag_count8 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count8$WP <- "8"
Antrag_count8$mp_id <- Antrag_count8$Var1
Antrag_count8$Var1 <- NULL

Antrag_X_9 <- subset(Antrag_X, elec_period=="9")
Antrag_count9 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count9$WP <- "9"
Antrag_count9$mp_id <- Antrag_count9$Var1
Antrag_count9$Var1 <- NULL

Antrag_X_10 <- subset(Antrag_X, elec_period=="10")
Antrag_count10 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count10$WP <- "10"
Antrag_count10$mp_id <- Antrag_count10$Var1
Antrag_count10$Var1 <- NULL

Antrag_X_11 <- subset(Antrag_X, elec_period=="11")
Antrag_count11 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count11$WP <- "11"
Antrag_count11$mp_id <- Antrag_count11$Var1
Antrag_count11$Var1 <- NULL

Antrag_X_12 <- subset(Antrag_X, elec_period=="12")
Antrag_count12 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count12$WP <- "12"
Antrag_count12$mp_id <- Antrag_count12$Var1
Antrag_count12$Var1 <- NULL

Antrag_X_13 <- subset(Antrag_X, elec_period=="13")
Antrag_count13 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count13$WP <- "13"
Antrag_count13$mp_id <- Antrag_count13$Var1
Antrag_count13$Var1 <- NULL

Antrag_X_14 <- subset(Antrag_X, elec_period=="14")
Antrag_count14 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count14$WP <- "14"
Antrag_count14$mp_id <- Antrag_count14$Var1
Antrag_count14$Var1 <- NULL

Antrag_X_15 <- subset(Antrag_X, elec_period=="15")
Antrag_count15 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count15$WP <- "15"
Antrag_count15$mp_id <- Antrag_count15$Var1
Antrag_count15$Var1 <- NULL

Antrag_X_16 <- subset(Antrag_X, elec_period=="16")
Antrag_count16 <- as.data.frame(table(unlist(Antrag_X_1[464:911])))
Antrag_count16$WP <- "16"
Antrag_count16$mp_id <- Antrag_count16$Var1
Antrag_count16$Var1 <- NULL

Antrag_X_count <- rbind(Antrag_count1, Antrag_count2, Antrag_count3, Antrag_count4, Antrag_count5, Antrag_count6, Antrag_count7, Antrag_count8, Antrag_count9, Antrag_count10, Antrag_count11, Antrag_count12, Antrag_count13, Antrag_count14, Antrag_count15, Antrag_count16)



#interpellations
Anfrage_X_1 <- subset(Anfrage_X, elec_period=="1")
Anfrage_count1 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count1$WP <- "1"
Anfrage_count1$mp_id <- Anfrage_count1$Var1
Anfrage_count1$Var1 <- NULL

Anfrage_X_2 <- subset(Anfrage_X, elec_period=="2")
Anfrage_count2 <- as.data.frame(table(unlist(Anfrage_X_2[464:911])))
Anfrage_count2$WP <- "2"
Anfrage_count2$mp_id <- Anfrage_count2$Var1
Anfrage_count2$Var1 <- NULL

Anfrage_X_3 <- subset(Anfrage_X, elec_period=="3")
Anfrage_count3 <- as.data.frame(table(unlist(Anfrage_X_3[464:911])))
Anfrage_count3$WP <- "3"
Anfrage_count3$mp_id <- Anfrage_count3$Var1
Anfrage_count3$Var1 <- NULL

Anfrage_X_4 <- subset(Anfrage_X, elec_period=="4")
Anfrage_count4 <- as.data.frame(table(unlist(Anfrage_X_4[464:911])))
Anfrage_count4$WP <- "4"
Anfrage_count4$mp_id <- Anfrage_count4$Var1
Anfrage_count4$Var1 <- NULL

Anfrage_X_5 <- subset(Anfrage_X, elec_period=="5")
Anfrage_count5 <- as.data.frame(table(unlist(Anfrage_X_5[464:911])))
Anfrage_count5$WP <- "5"
Anfrage_count5$mp_id <- Anfrage_count5$Var1
Anfrage_count5$Var1 <- NULL

Anfrage_X_6 <- subset(Anfrage_X, elec_period=="6")
Anfrage_count6 <- as.data.frame(table(unlist(Anfrage_X_6[464:911])))
Anfrage_count6$WP <- "6"
Anfrage_count6$mp_id <- Anfrage_count6$Var1
Anfrage_count6$Var1 <- NULL

Anfrage_X_7 <- subset(Anfrage_X, elec_period=="7")
Anfrage_count7 <- as.data.frame(table(unlist(Anfrage_X_7[464:911])))
Anfrage_count7$WP <- "7"
Anfrage_count7$mp_id <- Anfrage_count7$Var1
Anfrage_count7$Var1 <- NULL

Anfrage_X_8 <- subset(Anfrage_X, elec_period=="8")
Anfrage_count8 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count8$WP <- "8"
Anfrage_count8$mp_id <- Anfrage_count8$Var1
Anfrage_count8$Var1 <- NULL

Anfrage_X_9 <- subset(Anfrage_X, elec_period=="9")
Anfrage_count9 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count9$WP <- "9"
Anfrage_count9$mp_id <- Anfrage_count9$Var1
Anfrage_count9$Var1 <- NULL

Anfrage_X_10 <- subset(Anfrage_X, elec_period=="10")
Anfrage_count10 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count10$WP <- "10"
Anfrage_count10$mp_id <- Anfrage_count10$Var1
Anfrage_count10$Var1 <- NULL

Anfrage_X_11 <- subset(Anfrage_X, elec_period=="11")
Anfrage_count11 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count11$WP <- "11"
Anfrage_count11$mp_id <- Anfrage_count11$Var1
Anfrage_count11$Var1 <- NULL

Anfrage_X_12 <- subset(Anfrage_X, elec_period=="12")
Anfrage_count12 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count12$WP <- "12"
Anfrage_count12$mp_id <- Anfrage_count12$Var1
Anfrage_count12$Var1 <- NULL

Anfrage_X_13 <- subset(Anfrage_X, elec_period=="13")
Anfrage_count13 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count13$WP <- "13"
Anfrage_count13$mp_id <- Anfrage_count13$Var1
Anfrage_count13$Var1 <- NULL

Anfrage_X_14 <- subset(Anfrage_X, elec_period=="14")
Anfrage_count14 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count14$WP <- "14"
Anfrage_count14$mp_id <- Anfrage_count14$Var1
Anfrage_count14$Var1 <- NULL

Anfrage_X_15 <- subset(Anfrage_X, elec_period=="15")
Anfrage_count15 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count15$WP <- "15"
Anfrage_count15$mp_id <- Anfrage_count15$Var1
Anfrage_count15$Var1 <- NULL

Anfrage_X_16 <- subset(Anfrage_X, elec_period=="16")
Anfrage_count16 <- as.data.frame(table(unlist(Anfrage_X_1[464:911])))
Anfrage_count16$WP <- "16"
Anfrage_count16$mp_id <- Anfrage_count16$Var1
Anfrage_count16$Var1 <- NULL

Anfrage_X_count <- rbind(Anfrage_count1, Anfrage_count2, Anfrage_count3, Anfrage_count4, Anfrage_count5, Anfrage_count6, Anfrage_count7, Anfrage_count8, Anfrage_count9, Anfrage_count10, Anfrage_count11, Anfrage_count12, Anfrage_count13, Anfrage_count14, Anfrage_count15, Anfrage_count16)

#load Sieberer et al.'s mp_characteristics dataset
library(haven)
mp_characteristics <- read_dta("C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/sieberer mp characteristics/mp_characteristics.dta")

#create subset of all committee chairs and prepare data for later merging
mp_characteristics$WP <- mp_characteristics$elecper
commchairs <- subset(mp_characteristics, mp_characteristics$commchair==1)

#create list with identifieres of all committee chairs
commchairs_id <- data.frame(table(commchairs$mp_id))

#merge list of all committee chairs' identifiers with Sieberer et al.'s mp_characterstics
commchairs_id$mp_id <- commchairs_id$Var1 
future_commchairs <- merge(commchairs_id, mp_characteristics, by = "mp_id")

#save dataframe of all MPs who held a commitee chair at some point in their legislative career
library(xlsx)
write.xlsx(future_commchairs, file = "C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/future_commchairs.xlsx")

# The data frame of (future) committee chairs was then edited in MS Excel to exclude rows/observations where
# (1) MPs did hold a committee chair in a given legislative period, 
# (2) an MP's entry was duplicate during one legislative period because of irrelevant changes in office spell variable,
# (3) MPs did not hold a committee chair, but had done so in previous office spells or legislative periods,
# leaving us with a list of 946 unique combinations of an MP (who would in the future hold a committee chair) and a legislative period.


#merge data frame of future committee chairs with data frame containing numbers of proposals tabled without fraction support per MP
#prepare data
Antrag_X_count$mp_id <- as.numeric(as.character(Antrag_X_count$mp_id))
Antrag_X_count$WP <- as.numeric(as.character(Antrag_X_count$WP))
future_commchairs$mp_id <- as.numeric(as.character(future_commchairs$mp_id))

#merge
library(dplyr)
future_commchairs_count_Antrag <- left_join(future_commchairs, Antrag_X_count, by=c("mp_id", "WP"))

#change NAs to value 0 for later calculations 
future_commchairs_count_Antrag$Freq.y[is.na(future_commchairs_count_Antrag$Freq.y)] <- 0

#sort merged dataframe by legislative period and party affiliation of MP
future_commchairs_count_Antrag <- future_commchairs_count_Antrag[order(future_commchairs_count_Antrag$WP, future_commchairs_count_Antrag$party_elec),]

#save merged dataframe as xlsx file
write.xlsx(future_commchairs_count_Antrag, file = "C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/future_commchairs_count_motion.xlsx")



#merge data frame of future committee chairs with data frame containing numbers of interpellations tabled without fraction support per MP
#prepare data
Anfrage_X_count$mp_id <- as.numeric(as.character(Anfrage_X_count$mp_id))
Anfrage_X_count$WP <- as.numeric(as.character(Anfrage_X_count$WP))
future_commchairs$mp_id <- as.numeric(as.character(future_commchairs$mp_id))

#merge
library(dplyr)
future_commchairs_count_Anfrage <- left_join(future_commchairs, Anfrage_X_count, by=c("mp_id", "WP"))

#change NAs to value 0 for later calculations 
future_commchairs_count_Anfrage$Freq.y[is.na(future_commchairs_count_Anfrage$Freq.y)] <- 0

#sort merged dataframe by legislative period and party affiliation of MP
future_commchairs_count_Anfrage <- future_commchairs_count_Anfrage[order(future_commchairs_count_Anfrage$WP, future_commchairs_count_Anfrage$party_elec),]

#save merged dataframe as xlsx file
write.xlsx(future_commchairs_count_Anfrage, file = "C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/future_commchairs_count_motion.xlsx")


#create same dataframe of number of proposals/interpellations tabled per MP and legislative period for all MPs from 1st to 16th legislative period
mp_characteristics$WP <- mp_characteristics$elecper

#exlcude duplicate rows/observations due to changes in office spell variable
mp_characteristics <- subset(mp_characteristics, mp_characteristics$office_spell==1)

#prepare data for merging
mp_characteristics$mp_id <- as.numeric(all_mps$mp_id)

#merge data frame of all MPs with data frame containing numbers of proposals tabled without fraction support per MP
mp_characteristics_count_Antrag <- left_join(mp_characteristics, Antrag_X_count, by=c("mp_id", "WP"), all = TRUE)

#change NAs to value 0 for later calculations
mp_characteristics_count_Antrag$Freq[is.na(mp_characteristics_count_Antrag$Freq)] <- 0

#order rows by legislative period and MP's party affiliation
mp_characteristics_count_Antrag <- mp_characteristics_count_Antrag[order(mp_characteristics_count_Antrag$WP, mp_characteristics_count_Antrag$party_elec),]

#save merged dataframe as xlsx file
write.xlsx(mp_characteristics_count_Antrag, file = "C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/mp_characteristics_count_Antrag.xlsx")


#create same dataframe of number of proposals/interpellations tabled per MP and legislative period for all MPs from 1st to 16th legislative period
mp_characteristics$WP <- mp_characteristics$elecper

#exlcude duplicate rows/observations due to changes in office spell variable
mp_characteristics <- subset(mp_characteristics, mp_characteristics$office_spell==1)

#prepare data for merging
mp_characteristics$mp_id <- as.numeric(all_mps$mp_id)

#merge data frame of all MPs with data frame containing numbers of proposals tabled without fraction support per MP
mp_characteristics_count_Anfrage <- left_join(mp_characteristics, Anfrage_X_count, by=c("mp_id", "WP"), all = TRUE)

#change NAs to value 0 for later calculations
mp_characteristics_count_Anfrage$Freq[is.na(mp_characteristics_count_Anfrage$Freq)] <- 0

#order rows by legislative period and MP's party affiliation
mp_characteristics_count_Anfrage <- mp_characteristics_count_Anfrage[order(mp_characteristics_count_Anfrage$WP, mp_characteristics_count_Anfrage$party_elec),]

#save merged dataframe as xlsx file
write.xlsx(mp_characteristics_count_Anfrage, file = "C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/mp_characteristics_count_Anfrage.xlsx")


#The data frames containing the number of proposals/interpellations tabled without fraction support by all MPs and future committee chairs were
#then analysed further using Excel. Average numbers for all future committee chairs and MPs, as well as only for those belonging to the SPD and
#the CDU/CSU were calculated and entered into new Excel spreadsheets.


#create graph for all mps
library(ggplot2)
library(readxl)

#load file containing average values for interpellations of all MPs/future committee chairs
counts_anfragen_all <- read_excel("C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/Analysebeispiel Tobi/counts_anfragen_all.xlsx")

ggplot(counts_anfragen_all, aes(x=WP, y=mean)) +
  geom_line(aes(linetype=group)) +
  labs(title = "non-party interpellations by all MPs") +
  ylab("interpellations per MP (mean)") +
  xlab("legislative period") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("all future chairpersons", "all MPs")) +
  scale_x_continuous(breaks = round(seq(min(counts_anfragen_all$WP), max(counts_anfragen_all$WP), by = 1),1), 
                     labels=c("1 (1949-1953)","2 (1953-1957)","3 (1957-1961)","4 (1961-1965)","5 (1965-1969)","6 (1969-1972)","7 (1972-1976)","8 (1976-1980)","9 (1980-1983)","10 (1983-1987)","11 (1987-1990)","12 (1990-1994)","13 (1994-1998)","14 (1998-2002)","15 (2002-2005)","16 (2005-2009)"))


#load file containing average values for proposals of all MPs/future committee chairs
counts_antraege_all <- read_excel("C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/Analysebeispiel Tobi/counts_antraege_all.xlsx")

ggplot(counts_antraege_all, aes(x=WP, y=mean)) +
  geom_line(aes(linetype=group)) +
  labs(title = "non-party proposals by all MPs") +
  ylab("proposals per MP (mean)") +
  xlab("legislative period") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("all future chairpersons", "all MPs")) +
  scale_x_continuous(breaks = round(seq(min(counts_antraege_all$WP), max(counts_antraege_all$WP), by = 1),1), 
                     labels=c("1 (1949-1953)","2 (1953-1957)","3 (1957-1961)","4 (1961-1965)","5 (1965-1969)","6 (1969-1972)","7 (1972-1976)","8 (1976-1980)","9 (1980-1983)","10 (1983-1987)","11 (1987-1990)","12 (1990-1994)","13 (1994-1998)","14 (1998-2002)","15 (2002-2005)","16 (2005-2009)"))

#Both plots were combined manually into figure 3

##############################################################
########################## FIGURE 4 ########################## 
##############################################################


#load file containing average values for interpellations of CDU/CSU MPs/future committee chairs
counts_anfragen_cdu <- read_excel("C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/Analysebeispiel Tobi/counts_anfragen_cdu.xlsx")

ggplot(counts_anfragen_cdu, aes(x=WP, y=mean)) +
  geom_line(aes(linetype=group)) +
  labs(title = "non-party interpellations by CDU/CSU MPs") +
  ylab("interpellations per MP (mean)") +
  xlab("legislative period") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("future chairpersons (CDU/CSU)", "all MPs (CDU/CSU)")) +
  scale_x_continuous(breaks = round(seq(min(counts_anfragen_all$WP), max(counts_anfragen_all$WP), by = 1),1), 
                     labels=c("1 (1949-1953)","2 (1953-1957)","3 (1957-1961)","4 (1961-1965)","5 (1965-1969)","6 (1969-1972)","7 (1972-1976)","8 (1976-1980)","9 (1980-1983)","10 (1983-1987)","11 (1987-1990)","12 (1990-1994)","13 (1994-1998)","14 (1998-2002)","15 (2002-2005)","16 (2005-2009)"))


#load file containing average values for proposals of CDU/CSU MPs/future committee chairs
counts_antraege_cdu <- read_excel("C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/Analysebeispiel Tobi/counts_antraege_cdu.xlsx")

ggplot(counts_antraege_cdu, aes(x=WP, y=mean)) +
  geom_line(aes(linetype=group)) +
  labs(title = "non-party proposals by CDU/CSU MPs") +
  ylab("proposals per MP (mean)") +
  xlab("legislative period") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("future chairpersons (CDU/CSU)", "all MPs (CDU/CSU)")) +
  scale_x_continuous(breaks = round(seq(min(counts_antraege_all$WP), max(counts_antraege_all$WP), by = 1),1), 
                     labels=c("1 (1949-1953)","2 (1953-1957)","3 (1957-1961)","4 (1961-1965)","5 (1965-1969)","6 (1969-1972)","7 (1972-1976)","8 (1976-1980)","9 (1980-1983)","10 (1983-1987)","11 (1987-1990)","12 (1990-1994)","13 (1994-1998)","14 (1998-2002)","15 (2002-2005)","16 (2005-2009)"))



#load file containing average values for interpellations of SPD MPs/future committee chairs
counts_anfragen_spd <- read_excel("C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/Analysebeispiel Tobi/counts_anfragen_spd.xlsx")

ggplot(counts_anfragen_spd, aes(x=WP, y=mean)) +
  geom_line(aes(linetype=group)) +
  labs(title = "non-party interpellations by SPD MPs") +
  ylab("interpellations per MP (mean)") +
  xlab("legislative period") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("future chairpersons (SPD)", "all MPs (SPD)")) +
  scale_x_continuous(breaks = round(seq(min(counts_anfragen_all$WP), max(counts_anfragen_all$WP), by = 1),1), 
                     labels=c("1 (1949-1953)","2 (1953-1957)","3 (1957-1961)","4 (1961-1965)","5 (1965-1969)","6 (1969-1972)","7 (1972-1976)","8 (1976-1980)","9 (1980-1983)","10 (1983-1987)","11 (1987-1990)","12 (1990-1994)","13 (1994-1998)","14 (1998-2002)","15 (2002-2005)","16 (2005-2009)"))


#load file containing average values for proposals of CDU/CSU MPs/future committee chairs
counts_antraege_spd <- read_excel("C:/Users/Remschel/HiDrive/PhD/Daten/Bundestagsdaten/Drucksachen/Analysebeispiel Tobi/counts_antraege_spd.xlsx")

ggplot(counts_antraege_spd, aes(x=WP, y=mean)) +
  geom_line(aes(linetype=group)) +
  labs(title = "non-party proposals by SPD MPs") +
  ylab("proposals per MP (mean)") +
  xlab("legislative period") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("future chairpersons (SPD)", "all MPs (SPD)")) +
  scale_x_continuous(breaks = round(seq(min(counts_antraege_all$WP), max(counts_antraege_all$WP), by = 1),1), 
                     labels=c("1 (1949-1953)","2 (1953-1957)","3 (1957-1961)","4 (1961-1965)","5 (1965-1969)","6 (1969-1972)","7 (1972-1976)","8 (1976-1980)","9 (1980-1983)","10 (1983-1987)","11 (1987-1990)","12 (1990-1994)","13 (1994-1998)","14 (1998-2002)","15 (2002-2005)","16 (2005-2009)"))

#All plots were combined manually into figure 4



##############################################################
########################## FIGURE 5 ########################## 
##############################################################
### load 'requests' data
load("C:/Users/corin/Dropbox/Drucksachen Bundestag/FINAL DATA/request_split.RData")

### drop WP 18, because no ID
requests_split <- subset(requests_split, requests_split$elec_period<18)

### load MP data
mp_data <- read.dta13("C:/Users/corin/Dropbox/Corinna/Reserach Projects/Parlamentarische Anfragen im Bundestag/mp_characteristics_Sieberer_et_al.dta", convert.factors=FALSE)

### prepare MP data
mp_data$elec_period <- mp_data$elecper
mp_data$elecper <- NULL
mp_data <- subset(mp_data, mp_data$office_spell==1)
mp_data$women <- ifelse(mp_data$gender==0, 1, 0)


### join requests and MP data
request_split_merged <- left_join(requests_split, mp_data)


### identify requests that relate to childcare
request_split_merged$child <- str_detect(request_split_merged$text, "Kind", negate=FALSE)
request_split_merged$child <- ifelse(request_split_merged$child==TRUE, "true", "false")


##generate year variable
request_split_merged$year <- as.numeric(format(request_split_merged$date, "%Y"))

### collapse data by date and gender
requests_split_merged2 <- request_split_merged %>%
  group_by(year, women, child) %>%
  summarize(n_questions =n())

### long to wide format
requests_split_merged2 <- subset(requests_split_merged2, requests_split_merged2$women==1 | requests_split_merged2$women== 0)
requests_split_merged2$women <- as.factor(requests_split_merged2$women)
requests_split_merged2$child <- as.factor(requests_split_merged2$child)

requests_split_merged3 <- spread(requests_split_merged2, key = child, value = n_questions)


requests_split_merged3$false <- ifelse(is.na(requests_split_merged3$false), 0, requests_split_merged3$false)
requests_split_merged3$true <- ifelse(is.na(requests_split_merged3$true), 0, requests_split_merged3$true)
requests_split_merged3$perc <- requests_split_merged3$true/(requests_split_merged3$true+requests_split_merged3$false)*100
requests_split_merged3$perc <- ifelse(is.na(requests_split_merged3$perc), 0, requests_split_merged3$perc)


### Figure 4


p4 <- ggplot(data=requests_split_merged3, aes(x=year, y=perc,  group=women))+ 
  geom_line(aes(color=women))+
  scale_color_manual(values = c("grey", "black"), name = " ", labels=c("men", "women"))+
  theme_classic()+
  ylab("% questions")+
  xlab("year")
p4

ggsave("figure4.jpg")




