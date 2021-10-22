rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdataDAE <- read_csv("closedchartsdataDAE.csv")
View(closedchartsdataDAE)
dt <- tbl_df(closedchartsdataDAE)
glimpse(dt)

doctorslist <- dt %>% group_by(DOCTOR) %>% count() %>% arrange(desc(n))
View(doctorslist)

##DAE
#DOCUMENTED
DAEPenultimate <- dt %>% filter(TYPE == "P") %>% filter(DOCTOR == "Renatus Tarimo") %>% filter(SNN == 1.1 | SNN == 5.2 | SNN == 5.4 | SNN == 8.1) 
DAE <- select(DAEPenultimate, "DOCUMENTED" : "MET")

DAEDFINAL <- mutate(DAE, DOCUMENTEDNUMERIC = ifelse(DAE$DOCUMENTED %in% "D", 1, ifelse(DAE$DOCUMENTED %in% "ND", 1, 0)))
ALLDDAE <- filter(DAEDFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateDAE <- filter(ALLDDAE, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCOREDAE <- sum(DOCUMENTEDPenultimateDAE$DOCUMENTEDNUMERIC)
TOTALDSCOREDAE <- sum(ALLDDAE$DOCUMENTEDNUMERIC)
PercentDocumentedDAE <- (DOCUMENTEDSCOREDAE*100)/TOTALDSCOREDAE


#TIMELY
DAETFINAL <- mutate(DAE, TIMELYNUMERIC = ifelse(DAE$TIMELY %in% "Y", 1, ifelse(DAE$TIMELY %in% "N", 1, 0)))
ALLTDAE <- filter(DAETFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateDAE <- filter(ALLTDAE, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCOREDAE <- sum(TIMELYPenultimateDAE$TIMELYNUMERIC)
TOTALTSCOREDAE <- sum(ALLTDAE$TIMELYNUMERIC)
PercentTimelyDAE <- (TIMELYSCOREDAE*100)/TOTALTSCOREDAE


#LEGIBLE
DAELFINAL <- mutate(DAE, LEGIBLENUMERIC = ifelse(DAE$LEGIBLE %in% "Y", 1, ifelse(DAE$LEGIBLE %in% "N", 1, 0)))
ALLLDAE <- filter(DAELFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateDAE <- filter(ALLLDAE, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCOREDAE <- sum(LEGIBLEPenultimateDAE$LEGIBLENUMERIC)
TOTALLSCOREDAE <- sum(ALLLDAE$LEGIBLENUMERIC)
PercentLegibleDAE <- (LEGIBLESCOREDAE*100)/TOTALLSCOREDAE


#COMPLETE
DAECFINAL <- mutate(DAE, COMPLETENUMERIC = ifelse(DAE$COMPLETE %in% "Y", 1, ifelse(DAE$COMPLETE %in% "N", 1, 0)))
ALLCDAE <- filter(DAECFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateDAE <- filter(ALLCDAE, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCOREDAE <- sum(COMPLETEPenultimateDAE$COMPLETENUMERIC)
TOTALCSCOREDAE <- sum(ALLCDAE$COMPLETENUMERIC)
PercentCompleteDAE <- (COMPLETESCOREDAE*100)/TOTALCSCOREDAE


#MET
DAEMFINAL <- mutate(DAE, METNOTMETNUMERIC = ifelse(DAE$MET == "M", 1, ifelse(DAE$MET == "NM", 1, ifelse(DAE$MET == "NA", 0, 0))))
ALLMDAE <- filter(DAEMFINAL, METNOTMETNUMERIC == "1")
METPenultimateDAE <- filter(ALLMDAE, METNOTMETNUMERIC == "1" & MET == "M")
METSCOREDAE <- sum(METPenultimateDAE$METNOTMETNUMERIC)
TOTALMSCOREDAE <- sum(ALLMDAE$METNOTMETNUMERIC)
PercentMetDAE <- (METSCOREDAE*100)/TOTALMSCOREDAE

DOCAVERAGE <- (PercentTimelyDAE + PercentLegibleDAE + PercentCompleteDAE)/3

#DAE Scores for Doctors:
PercentDocumentedDAE
PercentTimelyDAE
PercentLegibleDAE
PercentCompleteDAE
PercentMetDAE