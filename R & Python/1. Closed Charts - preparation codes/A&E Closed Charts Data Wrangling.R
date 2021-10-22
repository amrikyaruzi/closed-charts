#A&E Closed Charts Data Wrangling

rm(list=ls())
library(tidyverse)
data <- read.csv("C:/Users/amri.kyaruzi/Desktop/A&E OME Details October 2020.csv")
View(data)
data1 <- data %>% select(MR.Number, DOCTOR, RMO, NURSE, SNN, DOCUMENTS, TYPE:COMPLETE)
View(data1)
data2 <- data1 %>% filter(DOCUMENTED == 'D' | DOCUMENTED == 'ND')
View(data2)
data3 <- data2 %>% mutate(MET = case_when(DOCUMENTED %in% 'D' & TIMELY %in% 'Y' & LEGIBLE %in% 'Y' & COMPLETE %in% 'Y' ~ 'M', TRUE ~ 'NM' ))
View(data3)
write.csv(data3, 'A&E Oct - Nov Closed Charts.csv')