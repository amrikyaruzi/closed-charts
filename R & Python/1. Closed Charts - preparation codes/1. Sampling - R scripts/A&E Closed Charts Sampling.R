#A&E Closed Charts Sampling
rm(list=ls())

library(tidyverse)
data <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021/Source/Minor departments/A&E July 2021.csv")
colnames(data) <- c("Complaint", "CTAS.Level", "Patient.s.Name", "Age", "Gender", "MR.Number", "Triage.Nurse", "Primary.Nurse",
                    "RMO.s.name", "Consultant")

data$Patient.s.Name <- str_to_title(data$Patient.s.Name) 
data$Primary.Nurse <- str_to_title(data$Primary.Nurse)
data$Triage.Nurse <- str_to_title(data$Triage.Nurse)
data$RMO.s.name <- str_to_title(data$RMO.s.name)
data$Consultant <- str_to_title(data$Consultant)

data <- data %>% select(Patient.s.Name, MR.Number, Triage.Nurse, Primary.Nurse:Consultant)
data <- na.omit(data)

data <- data %>% separate(MR.Number, c('First', 'Second', 'Third'), remove = FALSE, sep = '-') %>% arrange(Consultant, Third, Second, First) %>% select(-(c(First, Second, Third)))
data1 <- data %>% group_by(Consultant) %>% count() %>% rename(Patients = n) %>% mutate(Sample = case_when(Patients > 30 ~ (0.2 * Patients), 
                                                                                                          TRUE ~ ((6.0025 * Patients)/(Patients + 6.0025 - 1)))) %>% mutate(Department = 'Accidents & Emergencies')
data1 <- data1 %>% select(Department, Consultant:Sample)
data1$Sample <- data1$Sample %>% round(digits = 1)
data1$Sample <- ceiling(data1$Sample)

view(data)
View(data1)

cwd <- getwd()
setwd("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021")

openxlsx::write.xlsx(data, 'A&E July 2021 Closed Charts Sampling Frame.xlsx', sheetName = 'Sheet1')
openxlsx::write.xlsx(data1, 'A&E July 2021 Closed Charts Sample sizes.xlsx', sheetName = 'Sheet1')

setwd(cwd)