library(tidyverse)
data <- read.csv("C:/Users/amri.kyaruzi/Documents/A&E July - September Closed Charts.csv")
View(data)
data1 <- data %>% filter(DOCUMENTED == 'D' & SNN == 5.4)
View(data1)
data2 <- data1 %>% select(DOCTOR:DOCUMENTS, LEGIBLE)
View(data2)

data3 <- data %>% filter(DOCUMENTED == 'D' & SNN == 5.5)
View(data3)
data4 <- data3 %>% select(DOCTOR:DOCUMENTS, LEGIBLE)
View(data4)

write.csv(data2, "A&E - Use of Abbreviations on Discharge Summaries.csv")
write.csv(data4, "A&E - Use of Abbreviations on Informed consent forms.csv")

reticulate::repl_python()
import pandas as pd
data = pd.read_csv(r"C:/Users/amri.kyaruzi/Documents/A&E March - Use of Abbreviations on Discharge Summaries.csv")
data.head(5)
data['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype('str') + '%'