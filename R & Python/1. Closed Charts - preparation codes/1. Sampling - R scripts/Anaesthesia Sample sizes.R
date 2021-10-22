#Anaesthesia Sample size

rm(list=ls())
library(tidyverse)
data <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021/Source/Minor departments/Anaesthesia July 2021.csv")
View(data)

data$Ward <- str_to_lower(data$Ward)
data$Kindy.of.Anaesthesia <- str_to_upper(data$Kindy.of.Anaesthesia)

data <- data %>% filter(Ward != 'sds') %>% filter(Kindy.of.Anaesthesia != "LA" & Kindy.of.Anaesthesia != "Local")
data <- data %>% select(Date, Case.Type:Patient.Name, Anaesthetist)

#data <- na.omit(data)
#subset_data <- data[, c("Anaesthetist")]
#data <- data[complete.cases(subset_data),]

data <- data %>% mutate(Required = case_when(Anaesthetist %in% "" ~ "Not required", TRUE ~ "Required"))
data <- data %>% filter(Required == "Required") %>% select(-Required)

data <- data %>% separate(MR.., c('First', 'Second', 'Third'), sep = '-', remove = FALSE) %>% arrange(Anaesthetist, Third, Second, First) %>% select(-(c('First', 'Second', 'Third')))
View(data)

sampling <- data %>% group_by(Anaesthetist) %>% count()
sampling <- sampling %>% rename(Patients = n) %>% 
  mutate(Sample = case_when(Patients > 30 ~ (0.2 * Patients), 
                            TRUE ~ ((6.0025 * Patients)/(Patients + 6.0025 - 1))))
sampling <- sampling %>% mutate(Department = 'Anaesthesia') %>% rename(Consultant = Anaesthetist) %>% select(Department, Consultant:Sample)
sampling$Sample <- sampling$Sample %>% round(digits = 1)
sampling$Sample <- ceiling(sampling$Sample)
View(sampling)

 cwd <- getwd()
 setwd("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021")

openxlsx::write.xlsx(data, 'Anaesthesia July 2021 Sampling frame.xlsx')
openxlsx::write.xlsx(sampling, 'Anaesthesia July 2021 Sample sizes.xlsx')

 setwd(cwd)