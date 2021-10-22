rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdata <- read_csv("closedchartsdata.csv")
View(closedchartsdata)
dt <- tbl_df(closedchartsdata)
glimpse(dt)

##DOS
#DOCUMENTED
DOSPenultimate <- dt %>% filter(TYPE == "P" | TYPE == "N") %>% filter(SNN == 1.1 | SNN == 1.2 | SNN == 1.4 | SNN == 2.1 | SNN == 2.2 | SNN == 2.3 | SNN == 3.1| SNN== 3.2| SNN == 3.3 | SNN == 4.1 | SNN == 5.2 | SNN == 5.4 | SNN == 5.5 | SNN == 6.1 |SNN == 7.1 | SNN == 7.3 | SNN == 8.1) %>% filter(DEPT == "DOS")
DOS <- select(DOSPenultimate, "DOCUMENTED" : "MET")

DOSDFINAL <- mutate(DOS, DOCUMENTEDNUMERIC = ifelse(DOS$DOCUMENTED %in% "D", 1, ifelse(DOS$DOCUMENTED %in% "ND", 1, 0)))
ALLDDOS <- filter(DOSDFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateDOS <- filter(ALLDDOS, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCOREDOS <- sum(DOCUMENTEDPenultimateDOS$DOCUMENTEDNUMERIC)
TOTALDSCOREDOS <- sum(ALLDDOS$DOCUMENTEDNUMERIC)
PercentDocumentedDOS <- (DOCUMENTEDSCOREDOS*100)/TOTALDSCOREDOS


#TIMELY
DOSTFINAL <- mutate(DOS, TIMELYNUMERIC = ifelse(DOS$TIMELY %in% "Y", 1, ifelse(DOS$TIMELY %in% "N", 1, 0)))
ALLTDOS <- filter(DOSTFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateDOS <- filter(ALLTDOS, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCOREDOS <- sum(TIMELYPenultimateDOS$TIMELYNUMERIC)
TOTALTSCOREDOS <- sum(ALLTDOS$TIMELYNUMERIC)
PercentTimelyDOS <- (TIMELYSCOREDOS*100)/TOTALTSCOREDOS


#LEGIBLE
DOSLFINAL <- mutate(DOS, LEGIBLENUMERIC = ifelse(DOS$LEGIBLE %in% "Y", 1, ifelse(DOS$LEGIBLE %in% "N", 1, 0)))
ALLLDOS <- filter(DOSLFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateDOS <- filter(ALLLDOS, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCOREDOS <- sum(LEGIBLEPenultimateDOS$LEGIBLENUMERIC)
TOTALLSCOREDOS <- sum(ALLLDOS$LEGIBLENUMERIC)
PercentLegibleDOS <- (LEGIBLESCOREDOS*100)/TOTALLSCOREDOS


#COMPLETE
DOSCFINAL <- mutate(DOS, COMPLETENUMERIC = ifelse(DOS$COMPLETE %in% "Y", 1, ifelse(DOS$COMPLETE %in% "N", 1, 0)))
ALLCDOS <- filter(DOSCFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateDOS <- filter(ALLCDOS, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCOREDOS <- sum(COMPLETEPenultimateDOS$COMPLETENUMERIC)
TOTALCSCOREDOS <- sum(ALLCDOS$COMPLETENUMERIC)
PercentCompleteDOS <- (COMPLETESCOREDOS*100)/TOTALCSCOREDOS


#MET
DOSMFINAL <- mutate(DOS, METNUMERIC = ifelse(DOS$MET %in% "M", 1, ifelse(DOS$MET %in% "NM", 1, 0)))
ALLMDOS <- filter(DOSMFINAL, METNUMERIC == "1")
METPenultimateDOS <- filter(ALLMDOS, METNUMERIC == "1" & MET == "M")
METSCOREDOS <- sum(METPenultimateDOS$METNUMERIC)
TOTALMSCOREDOS <- sum(ALLMDOS$METNUMERIC)
PercentMetDOS <- (METSCOREDOS*100)/TOTALMSCOREDOS

DEPT_TLC_AVERAGE <- (PercentTimelyDOS + PercentLegibleDOS + PercentCompleteDOS)/3

#Department of Surgery Scores:
PercentDocumentedDOS
PercentTimelyDOS
PercentLegibleDOS
PercentCompleteDOS
PercentMetDOS