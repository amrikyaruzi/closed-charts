rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdata <- read_csv("closedchartsdata.csv")
View(closedchartsdata)
dt <- tbl_df(closedchartsdata)
glimpse(dt)
doctorslist <- dt %>% filter(DEPT == "DOG") %>% group_by(DOCTOR) %>% count() %>% arrange(desc(n))
View(doctorslist)

##DOG
#DOCUMENTED
DOGPenultimate <- dt %>%  filter(TYPE == "P") %>% filter(DOCTOR == "Kidanto Hussein") %>% filter(SNN == 1.1 | SNN == 2.1 | SNN== 3.2 | SNN == 5.2 |SNN == 5.4 | SNN == 7.1 | SNN == 7.3 | SNN == 8.1) %>% filter(DEPT == "DOG")
DOG <- select(DOGPenultimate, "DOCUMENTED" : "MET")

DOGDFINAL <- mutate(DOG, DOCUMENTEDNUMERIC = ifelse(DOG$DOCUMENTED %in% "D", 1, ifelse(DOG$DOCUMENTED %in% "ND", 1, 0)))
ALLDDOG <- filter(DOGDFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateDOG <- filter(ALLDDOG, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCOREDOG <- sum(DOCUMENTEDPenultimateDOG$DOCUMENTEDNUMERIC)
TOTALDSCOREDOG <- sum(ALLDDOG$DOCUMENTEDNUMERIC)
PercentDocumentedDOG <- (DOCUMENTEDSCOREDOG*100)/TOTALDSCOREDOG


#TIMELY
DOGTFINAL <- mutate(DOG, TIMELYNUMERIC = ifelse(DOG$TIMELY %in% "Y", 1, ifelse(DOG$TIMELY %in% "N", 1, 0)))
ALLTDOG <- filter(DOGTFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateDOG <- filter(ALLTDOG, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCOREDOG <- sum(TIMELYPenultimateDOG$TIMELYNUMERIC)
TOTALTSCOREDOG <- sum(ALLTDOG$TIMELYNUMERIC)
PercentTimelyDOG <- (TIMELYSCOREDOG*100)/TOTALTSCOREDOG


#LEGIBLE
DOGLFINAL <- mutate(DOG, LEGIBLENUMERIC = ifelse(DOG$LEGIBLE %in% "Y", 1, ifelse(DOG$LEGIBLE %in% "N", 1, 0)))
ALLLDOG <- filter(DOGLFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateDOG <- filter(ALLLDOG, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCOREDOG <- sum(LEGIBLEPenultimateDOG$LEGIBLENUMERIC)
TOTALLSCOREDOG <- sum(ALLLDOG$LEGIBLENUMERIC)
PercentLegibleDOG <- (LEGIBLESCOREDOG*100)/TOTALLSCOREDOG



#COMPLETE
DOGCFINAL <- mutate(DOG, COMPLETENUMERIC = ifelse(DOG$COMPLETE %in% "Y", 1, ifelse(DOG$COMPLETE %in% "N", 1, 0)))
ALLCDOG <- filter(DOGCFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateDOG <- filter(ALLCDOG, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCOREDOG <- sum(COMPLETEPenultimateDOG$COMPLETENUMERIC)
TOTALCSCOREDOG <- sum(ALLCDOG$COMPLETENUMERIC)
PercentCompleteDOG <- (COMPLETESCOREDOG*100)/TOTALCSCOREDOG


#MET
DOGMFINAL <- mutate(DOG, METNUMERIC = ifelse(DOG$MET %in% "M", 1, ifelse(DOG$MET %in% "NM", 1, 0)))
ALLMDOG <- filter(DOGMFINAL, METNUMERIC == "1")
METPenultimateDOG <- filter(ALLMDOG, METNUMERIC == "1" & MET == "M")
METSCOREDOG <- sum(METPenultimateDOG$METNUMERIC)
TOTALMSCOREDOG <- sum(ALLMDOG$METNUMERIC)
PercentMetDOG <- (METSCOREDOG*100)/TOTALMSCOREDOG

DOCAVERAGE <- (PercentTimelyDOG + PercentLegibleDOG + PercentCompleteDOG)/3

#Maternity Doctor's Scores:
PercentDocumentedDOG
PercentTimelyDOG
PercentLegibleDOG
PercentCompleteDOG
PercentMetDOG
