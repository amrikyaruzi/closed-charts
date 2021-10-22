rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdata <- read_csv("closedchartsdata.csv")
View(closedchartsdata)
dt <- tbl_df(closedchartsdata)
glimpse(dt)

##DON
#DOCUMENTED
DONPenultimate <- dt  %>%  filter(TYPE == "P" | TYPE == "N") %>% filter(SNN == 1.5 | SNN == 2.1 | SNN == 3.1 | SNN== 3.2) %>% filter(DEPT == "DON")
DON <- select(DONPenultimate, "DOCUMENTED" : "MET")

DONDFINAL <- mutate(DON, DOCUMENTEDNUMERIC = ifelse(DON$DOCUMENTED %in% "D", 1, ifelse(DON$DOCUMENTED %in% "ND", 1, 0)))
ALLDDON <- filter(DONDFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateDON <- filter(ALLDDON, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCOREDON <- sum(DOCUMENTEDPenultimateDON$DOCUMENTEDNUMERIC)
TOTALDSCOREDON <- sum(ALLDDON$DOCUMENTEDNUMERIC)
PercentDocumentedDON <- (DOCUMENTEDSCOREDON*100)/TOTALDSCOREDON


#TIMELY
DONTFINAL <- mutate(DON, TIMELYNUMERIC = ifelse(DON$TIMELY %in% "Y", 1, ifelse(DON$TIMELY %in% "N", 1, 0)))
ALLTDON <- filter(DONTFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateDON <- filter(ALLTDON, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCOREDON <- sum(TIMELYPenultimateDON$TIMELYNUMERIC)
TOTALTSCOREDON <- sum(ALLTDON$TIMELYNUMERIC)
PercentTimelyDON <- (TIMELYSCOREDON*100)/TOTALTSCOREDON


#LEGIBLE
DONLFINAL <- mutate(DON, LEGIBLENUMERIC = ifelse(DON$LEGIBLE %in% "Y", 1, ifelse(DON$LEGIBLE %in% "N", 1, 0)))
ALLLDON <- filter(DONLFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateDON <- filter(ALLLDON, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCOREDON <- sum(LEGIBLEPenultimateDON$LEGIBLENUMERIC)
TOTALLSCOREDON <- sum(ALLLDON$LEGIBLENUMERIC)
PercentLegibleDON <- (LEGIBLESCOREDON*100)/TOTALLSCOREDON



#COMPLETE
DONCFINAL <- mutate(DON, COMPLETENUMERIC = ifelse(DON$COMPLETE %in% "Y", 1, ifelse(DON$COMPLETE %in% "N", 1, 0)))
ALLCDON <- filter(DONCFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateDON <- filter(ALLCDON, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCOREDON <- sum(COMPLETEPenultimateDON$COMPLETENUMERIC)
TOTALCSCOREDON <- sum(ALLCDON$COMPLETENUMERIC)
PercentCompleteDON <- (COMPLETESCOREDON*100)/TOTALCSCOREDON


#MET
DONMFINAL <- mutate(DON, METNUMERIC = ifelse(DON$MET %in% "M", 1, ifelse(DON$MET %in% "NM", 1, 0)))
ALLMDON <- filter(DONMFINAL, METNUMERIC == "1")
METPenultimateDON <- filter(ALLMDON, METNUMERIC == "1" & MET == "M")
METSCOREDON <- sum(METPenultimateDON$METNUMERIC)
TOTALMSCOREDON <- sum(ALLMDON$METNUMERIC)
PercentMetDON <- (METSCOREDON*100)/TOTALMSCOREDON

DEPT_TLC_AVERAGE <- (PercentTimelyDON + PercentLegibleDON + PercentCompleteDON)/3

#Department of Nutrition Scores:
PercentDocumentedDON
PercentTimelyDON
PercentLegibleDON
PercentCompleteDON
PercentMetDON