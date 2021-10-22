library(tidyverse)
data <- read.csv("C:/Users/amri.kyaruzi/Desktop/APRIL 2020 OME DETAILS.csv")
View(data)
data1 <- data %>% filter(DEPT == 'DOA' & COMPLETE == 'N') %>% select(AKNO:ADMNO, DOCUMENTS, COMPLETE)

View(data1)
