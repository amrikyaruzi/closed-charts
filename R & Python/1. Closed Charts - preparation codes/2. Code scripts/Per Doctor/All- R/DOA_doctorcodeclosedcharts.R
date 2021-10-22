rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
closedchartsdata <- read_csv("C:/Users/amri.kyaruzi/Documents/Python/Projects/Closed Charts Audits/Monthly/3. March 2020/closedchartsdata.csv")
View(closedchartsdata)
dt <- tbl_df(closedchartsdata)
glimpse(dt)

dt %>% group_by(DEPT) %>% count() %>% arrange(desc(n))
doctorslist <- dt %>% filter(DEPT == "DOA") %>% group_by(DOCTOR) %>% count() %>% arrange(desc(n))
View(doctorslist)

##DOA
#DOCUMENTED
DOAPenultimate <- dt %>% filter(TYPE == "P") %>% filter(DOCTOR == "Tembo, Frederick") %>% filter(SNN == 1.3 | SNN == 4.1 | SNN == 5.1 | SNN== 5.2 | SNN == 7.2) %>% filter(DEPT == "DOA") 
DOA <- select(DOAPenultimate, "DOCUMENTED" : "MET")

DOADFINAL <- mutate(DOA, DOCUMENTEDNUMERIC = ifelse(DOA$DOCUMENTED %in% "D", 1, ifelse(DOA$DOCUMENTED %in% "ND", 1, 0)))
ALLDDOA <- filter(DOADFINAL, DOCUMENTEDNUMERIC == "1")
DOCUMENTEDPenultimateDOA <- filter(ALLDDOA, DOCUMENTEDNUMERIC == "1" & DOCUMENTED == "D")
DOCUMENTEDSCOREDOA <- sum(DOCUMENTEDPenultimateDOA$DOCUMENTEDNUMERIC)
TOTALDSCOREDOA <- sum(ALLDDOA$DOCUMENTEDNUMERIC)
PercentDocumentedDOA <- (DOCUMENTEDSCOREDOA*100)/TOTALDSCOREDOA


#TIMELY
DOATFINAL <- mutate(DOA, TIMELYNUMERIC = ifelse(DOA$TIMELY %in% "Y", 1, ifelse(DOA$TIMELY %in% "N", 1, 0)))
ALLTDOA <- filter(DOATFINAL, TIMELYNUMERIC == "1")
TIMELYPenultimateDOA <- filter(ALLTDOA, TIMELYNUMERIC == "1" & TIMELY == "Y")
TIMELYSCOREDOA <- sum(TIMELYPenultimateDOA$TIMELYNUMERIC)
TOTALTSCOREDOA <- sum(ALLTDOA$TIMELYNUMERIC)
PercentTimelyDOA <- (TIMELYSCOREDOA*100)/TOTALTSCOREDOA


#LEGIBLE
DOALFINAL <- mutate(DOA, LEGIBLENUMERIC = ifelse(DOA$LEGIBLE %in% "Y", 1, ifelse(DOA$LEGIBLE %in% "N", 1, 0)))
ALLLDOA <- filter(DOALFINAL, LEGIBLENUMERIC == "1")
LEGIBLEPenultimateDOA <- filter(ALLLDOA, LEGIBLENUMERIC == "1" & LEGIBLE == "Y")
LEGIBLESCOREDOA <- sum(LEGIBLEPenultimateDOA$LEGIBLENUMERIC)
TOTALLSCOREDOA <- sum(ALLLDOA$LEGIBLENUMERIC)
PercentLegibleDOA <- (LEGIBLESCOREDOA*100)/TOTALLSCOREDOA


#COMPLETE
DOACFINAL <- mutate(DOA, COMPLETENUMERIC = ifelse(DOA$COMPLETE %in% "Y", 1, ifelse(DOA$COMPLETE %in% "N", 1, 0)))
ALLCDOA <- filter(DOACFINAL, COMPLETENUMERIC == "1")
COMPLETEPenultimateDOA <- filter(ALLCDOA, COMPLETENUMERIC == "1" & COMPLETE == "Y")
COMPLETESCOREDOA <- sum(COMPLETEPenultimateDOA$COMPLETENUMERIC)
TOTALCSCOREDOA <- sum(ALLCDOA$COMPLETENUMERIC)
PercentCompleteDOA <- (COMPLETESCOREDOA*100)/TOTALCSCOREDOA


#MET
DOAMFINAL <- mutate(DOA, METNOTMETNUMERIC = ifelse(DOA$MET == "M", 1, ifelse(DOA$MET == "NM", 1, ifelse(DOA$MET == "NA", 0, 0))))
ALLMDOA <- filter(DOAMFINAL, METNOTMETNUMERIC == "1")
METPenultimateDOA <- filter(ALLMDOA, METNOTMETNUMERIC == "1" & MET == "M")
METSCOREDOA <- sum(METPenultimateDOA$METNOTMETNUMERIC)
TOTALMSCOREDOA <- sum(ALLMDOA$METNOTMETNUMERIC)
PercentMetDOA <- (METSCOREDOA*100)/TOTALMSCOREDOA

#DOCAVERAGE
DOCAVERAGE <- (PercentTimelyDOA + PercentLegibleDOA + PercentCompleteDOA)/3

#DOA Doctor's scores:
PercentDocumentedDOA
PercentTimelyDOA
PercentLegibleDOA
PercentCompleteDOA
PercentMetDOA