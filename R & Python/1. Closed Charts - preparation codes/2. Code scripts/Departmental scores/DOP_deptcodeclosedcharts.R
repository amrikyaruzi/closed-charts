rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdata <- read_csv("closedchartsdata.csv")
View(closedchartsdata)
dt <- tbl_df(closedchartsdata)
glimpse(dt)

##DOP
#DOCUMENTED
DOPPenultimate <- dt  %>%  filter(TYPE == "P" | TYPE == "N") %>% filter(SNN == 1.1 | SNN == 1.2 | SNN == 1.4 | SNN== 2.1 | SNN == 2.2 | SNN == 2.3 | SNN == 3.1 | SNN == 3.2 | SNN == 3.3 | SNN == 4.1 | SNN == 5.2 | SNN == 5.4 | SNN == 5.5 | SNN == 6.1 | SNN == 7.1 | SNN == 8.1) %>% filter(DEPT == "DOP")
DOP <- select(DOPPenultimate, "DOCUMENTED" : "MET")

DOPDFINAL <- mutate(DOP, DOCUMENTEDNUMERIC = ifelse(DOP$DOCUMENTED %in% "D", 1, ifelse(DOP$DOCUMENTED %in% "ND", 1, 0)))
ALLDDOP <- filter(DOPDFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateDOP <- filter(ALLDDOP, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCOREDOP <- sum(DOCUMENTEDPenultimateDOP$DOCUMENTEDNUMERIC)
TOTALDSCOREDOP <- sum(ALLDDOP$DOCUMENTEDNUMERIC)
PercentDocumentedDOP <- (DOCUMENTEDSCOREDOP*100)/TOTALDSCOREDOP


#TIMELY
DOPTFINAL <- mutate(DOP, TIMELYNUMERIC = ifelse(DOP$TIMELY %in% "Y", 1, ifelse(DOP$TIMELY %in% "N", 1, 0)))
ALLTDOP <- filter(DOPTFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateDOP <- filter(ALLTDOP, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCOREDOP <- sum(TIMELYPenultimateDOP$TIMELYNUMERIC)
TOTALTSCOREDOP <- sum(ALLTDOP$TIMELYNUMERIC)
PercentTimelyDOP <- (TIMELYSCOREDOP*100)/TOTALTSCOREDOP


#LEGIBLE
DOPLFINAL <- mutate(DOP, LEGIBLENUMERIC = ifelse(DOP$LEGIBLE %in% "Y", 1, ifelse(DOP$LEGIBLE %in% "N", 1, 0)))
ALLLDOP <- filter(DOPLFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateDOP <- filter(ALLLDOP, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCOREDOP <- sum(LEGIBLEPenultimateDOP$LEGIBLENUMERIC)
TOTALLSCOREDOP <- sum(ALLLDOP$LEGIBLENUMERIC)
PercentLegibleDOP <- (LEGIBLESCOREDOP*100)/TOTALLSCOREDOP



#COMPLETE
DOPCFINAL <- mutate(DOP, COMPLETENUMERIC = ifelse(DOP$COMPLETE %in% "Y", 1, ifelse(DOP$COMPLETE %in% "N", 1, 0)))
ALLCDOP <- filter(DOPCFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateDOP <- filter(ALLCDOP, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCOREDOP <- sum(COMPLETEPenultimateDOP$COMPLETENUMERIC)
TOTALCSCOREDOP <- sum(ALLCDOP$COMPLETENUMERIC)
PercentCompleteDOP <- (COMPLETESCOREDOP*100)/TOTALCSCOREDOP


#MET
DOPMFINAL <- mutate(DOP, METNUMERIC = ifelse(DOP$MET %in% "M", 1, ifelse(DOP$MET %in% "NM", 1, 0)))
ALLMDOP <- filter(DOPMFINAL, METNUMERIC == "1")
METPenultimateDOP <- filter(ALLMDOP, METNUMERIC == "1" & MET == "M")
METSCOREDOP <- sum(METPenultimateDOP$METNUMERIC)
TOTALMSCOREDOP <- sum(ALLMDOP$METNUMERIC)
PercentMetDOP <- (METSCOREDOP*100)/TOTALMSCOREDOP

DEPT_TLC_AVERAGE <- (PercentTimelyDOP + PercentLegibleDOP + PercentCompleteDOP)/3

#Department of Paediatrics Scores:
PercentDocumentedDOP
PercentTimelyDOP
PercentLegibleDOP
PercentCompleteDOP
PercentMetDOP