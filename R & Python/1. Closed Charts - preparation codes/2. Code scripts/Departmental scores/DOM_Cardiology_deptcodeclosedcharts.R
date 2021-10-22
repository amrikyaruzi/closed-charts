rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdata <- read_csv("closedchartsdata.csv")
View(closedchartsdata)
dt <- tbl_df(closedchartsdata)
glimpse(dt)

##DOM
#DOCUMENTED
DOMPenultimate <- dt %>%  filter(TYPE == "P" | TYPE == "N") %>% filter(DOCTOR == "Robert, Mvungi" | DOCTOR == "Soni Amit" | DOCTOR == "Jamal Nasiruddin" | DOCTOR == "Jamal Nasirudin" | DOCTOR == "Robert Mvungi" | DOCTOR == "MUSTAAFA BAPUMIA" | DOCTOR == "Jamal, Nasiruddin") %>% filter(SNN == 1.1 | SNN == 1.2 | SNN == 1.4 | SNN== 2.1 | SNN == 2.2 | SNN == 2.3 |SNN == 3.1 | SNN == 3.2 | SNN == 4.1 | SNN == 5.2 | SNN == 5.4 | SNN == 5.5 |SNN == 6.1 | SNN == 7.1 | SNN == 7.3 |  SNN == 8.1) %>% filter(DEPT == "DOM")
DOM <- select(DOMPenultimate, "DOCUMENTED" : "MET")

DOMDFINAL <- mutate(DOM, DOCUMENTEDNUMERIC = ifelse(DOM$DOCUMENTED %in% "D", 1, ifelse(DOM$DOCUMENTED %in% "ND", 1, 0)))
ALLDDOM <- filter(DOMDFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateDOM <- filter(ALLDDOM, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCOREDOM <- sum(DOCUMENTEDPenultimateDOM$DOCUMENTEDNUMERIC)
TOTALDSCOREDOM <- sum(ALLDDOM$DOCUMENTEDNUMERIC)
PercentDocumentedCardiology <- (DOCUMENTEDSCOREDOM*100)/TOTALDSCOREDOM


#TIMELY
DOMTFINAL <- mutate(DOM, TIMELYNUMERIC = ifelse(DOM$TIMELY %in% "Y", 1, ifelse(DOM$TIMELY %in% "N", 1, 0)))
ALLTDOM <- filter(DOMTFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateDOM <- filter(ALLTDOM, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCOREDOM <- sum(TIMELYPenultimateDOM$TIMELYNUMERIC)
TOTALTSCOREDOM <- sum(ALLTDOM$TIMELYNUMERIC)
PercentTimelyCardiology <- (TIMELYSCOREDOM*100)/TOTALTSCOREDOM


#LEGIBLE
DOMLFINAL <- mutate(DOM, LEGIBLENUMERIC = ifelse(DOM$LEGIBLE %in% "Y", 1, ifelse(DOM$LEGIBLE %in% "N", 1, 0)))
ALLLDOM <- filter(DOMLFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateDOM <- filter(ALLLDOM, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCOREDOM <- sum(LEGIBLEPenultimateDOM$LEGIBLENUMERIC)
TOTALLSCOREDOM <- sum(ALLLDOM$LEGIBLENUMERIC)
PercentLegibleCardiology <- (LEGIBLESCOREDOM*100)/TOTALLSCOREDOM



#COMPLETE
DOMCFINAL <- mutate(DOM, COMPLETENUMERIC = ifelse(DOM$COMPLETE %in% "Y", 1, ifelse(DOM$COMPLETE %in% "N", 1, 0)))
ALLCDOM <- filter(DOMCFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateDOM <- filter(ALLCDOM, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCOREDOM <- sum(COMPLETEPenultimateDOM$COMPLETENUMERIC)
TOTALCSCOREDOM <- sum(ALLCDOM$COMPLETENUMERIC)
PercentCompleteCardiology <- (COMPLETESCOREDOM*100)/TOTALCSCOREDOM


#MET
DOMMFINAL <- mutate(DOM, METNUMERIC = ifelse(DOM$MET %in% "M", 1, ifelse(DOM$MET %in% "NM", 1, 0)))
ALLMDOM <- filter(DOMMFINAL, METNUMERIC == "1")
METPenultimateDOM <- filter(ALLMDOM, METNUMERIC == "1" & MET == "M")
METSCOREDOM <- sum(METPenultimateDOM$METNUMERIC)
TOTALMSCOREDOM <- sum(ALLMDOM$METNUMERIC)
PercentMetCardiology <- (METSCOREDOM*100)/TOTALMSCOREDOM
UNIT_TLC_AVERAGE <- (PercentTimelyCardiology + PercentLegibleCardiology + PercentCompleteCardiology)/3


#Cardiology Scores
PercentDocumentedCardiology
PercentTimelyCardiology
PercentLegibleCardiology
PercentCompleteCardiology
PercentMetCardiology

