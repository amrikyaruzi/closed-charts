rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdata <- read_csv("closedchartsdata.csv")
View(closedchartsdata)
dt <- tbl_df(closedchartsdata)
glimpse(dt)

##RM
#DOCUMENTED
RMPenultimate <- dt %>% filter(TYPE == "P" | TYPE == "N") %>%  filter(SNN == 1.4 | SNN == 1.6 | SNN == 1.7 | SNN== 2.1 | SNN == 3.1) %>% filter(DEPT == "RM")
RM <- select(RMPenultimate, "DOCUMENTED" : "MET")

RMDFINAL <- mutate(RM, DOCUMENTEDNUMERIC = ifelse(RM$DOCUMENTED %in% "D", 1, ifelse(RM$DOCUMENTED %in% "ND", 1, 0)))
ALLDRM <- filter(RMDFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateRM <- filter(ALLDRM, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCORERM <- sum(DOCUMENTEDPenultimateRM$DOCUMENTEDNUMERIC)
TOTALDSCORERM <- sum(ALLDRM$DOCUMENTEDNUMERIC)
PercentDocumentedRM <- (DOCUMENTEDSCORERM*100)/TOTALDSCORERM


#TIMELY
RMTFINAL <- mutate(RM, TIMELYNUMERIC = ifelse(RM$TIMELY %in% "Y", 1, ifelse(RM$TIMELY %in% "N", 1, 0)))
ALLTRM <- filter(RMTFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateRM <- filter(ALLTRM, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCORERM <- sum(TIMELYPenultimateRM$TIMELYNUMERIC)
TOTALTSCORERM <- sum(ALLTRM$TIMELYNUMERIC)
PercentTimelyRM <- (TIMELYSCORERM*100)/TOTALTSCORERM


#LEGIBLE
RMLFINAL <- mutate(RM, LEGIBLENUMERIC = ifelse(RM$LEGIBLE %in% "Y", 1, ifelse(RM$LEGIBLE %in% "N", 1, 0)))
ALLLRM <- filter(RMLFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateRM <- filter(ALLLRM, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCORERM <- sum(LEGIBLEPenultimateRM$LEGIBLENUMERIC)
TOTALLSCORERM <- sum(ALLLRM$LEGIBLENUMERIC)
PercentLegibleRM <- (LEGIBLESCORERM*100)/TOTALLSCORERM



#COMPLETE
RMCFINAL <- mutate(RM, COMPLETENUMERIC = ifelse(RM$COMPLETE %in% "Y", 1, ifelse(RM$COMPLETE %in% "N", 1, 0)))
ALLCRM <- filter(RMCFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateRM <- filter(ALLCRM, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCORERM <- sum(COMPLETEPenultimateRM$COMPLETENUMERIC)
TOTALCSCORERM <- sum(ALLCRM$COMPLETENUMERIC)
PercentCompleteRM <- (COMPLETESCORERM*100)/TOTALCSCORERM


#MET
RMMFINAL <- mutate(RM, METNUMERIC = ifelse(RM$MET %in% "M", 1, ifelse(RM$MET %in% "NM", 1, 0)))
ALLMRM <- filter(RMMFINAL, METNUMERIC == "1")
METPenultimateRM <- filter(ALLMRM, METNUMERIC == "1" & MET == "M")
METSCORERM <- sum(METPenultimateRM$METNUMERIC)
TOTALMSCORERM <- sum(ALLMRM$METNUMERIC)
PercentMetRM <- (METSCORERM*100)/TOTALMSCORERM

DEPT_TLC_AVERAGE <- (PercentTimelyRM + PercentLegibleRM + PercentCompleteRM)/3

#Department of Physiotherapy Scores:
PercentDocumentedRM
PercentTimelyRM
PercentLegibleRM
PercentCompleteRM
PercentMetRM