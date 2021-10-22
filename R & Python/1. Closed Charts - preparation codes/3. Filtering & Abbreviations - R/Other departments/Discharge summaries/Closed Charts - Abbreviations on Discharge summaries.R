library(tidyverse)
data <- read.csv("C:/Users/amri.kyaruzi/Documents/OME DETAILS JULY - SEPTEMBER.csv")
View(data)
data1 <- data %>% filter(DOCUMENTED == 'D' & SNN == 5.4 & DEPT != "DOA")
View(data1)
data2 <- data1 %>% select(DOCTOR:DOCUMENTS, LEGIBLE)
View(data2)

write.csv(data2, "Q3 - Use of Abbreviations on Discharge Summaries.csv")

reticulate::repl_python()
import pandas as pd
data = pd.read_csv(r"C:/Users/amri.kyaruzi/Documents/Q2 - Use of Abbreviations on Discharge Summaries.csv")
data.head(5)
data.groupby('DEPT')['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype('str') + '%'