
#Physiotherapy Closed Charts Sampling
rm(list=ls())

library(tidyverse)
data <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021/Source/Minor departments/Physiotherapy Closed Charts - July 2021.csv")

#colnames(data) <- c("Date", "MRNumber", "Admission Number", "Physiotherapist")
#colnames(data) <- c("Date", "MRNumber", "Physiotherapist")

data <- data[,c(2, 3)] #Selecting column 2 and 3 only (removing the date column)

colnames(data) <- c("MRNumber", "Physiotherapist")

#data <- data %>% select(Date, MRNumber, Physiotherapist)
data <- data %>% select(MRNumber, Physiotherapist)

data <- data %>% mutate(Physiotherapist = case_when(Physiotherapist %in% 'DC' ~ 'Dietrich Chusi',
                                                    Physiotherapist %in% 'JK' ~ 'Joseph Kisima',
                                                    Physiotherapist %in% 'JM' ~ 'Jerusalem Muburirwa',
                                                    Physiotherapist %in% 'NM' ~ 'Noela Mushi',
                                                    Physiotherapist %in% 'AH' ~ 'Aisha Hussein',
                                                    Physiotherapist %in% 'SG' ~ 'Simon Gundah',
                                                    Physiotherapist %in% 'FA' ~ 'Fatuma Ayoub',
                                                    Physiotherapist %in% 'CN' ~ 'Christopher Nyoni',
                                                    Physiotherapist %in% 'CF' ~ 'Christabella Mwageni',
                                                    Physiotherapist %in% 'SF' ~ 'Sandra Frisch',
                                                    TRUE ~ ' '))
data <- data %>% separate(MRNumber, c('First', 'Second', 'Third'), remove = FALSE, sep = '-') %>% arrange(Physiotherapist, Third, Second, First) %>% select(-(c(First, Second, Third)))

data1 <- data %>% count(Physiotherapist) %>% rename(Patients = n)%>% mutate(Sample = case_when(Patients > 30 ~ (0.2 * Patients), 
                                                                                                              TRUE ~ ((6.0025 * Patients)/(Patients + 6.0025 - 1)))) %>% mutate(Department = 'Physiotherapy')
data1 <- data1 %>% select(Department, Physiotherapist:Sample) %>% rename(Consultant = Physiotherapist)
data1$Sample <- data1$Sample %>% round(digits = 1)
data1$Sample <- ceiling(data1$Sample)

view(data)
View(data1)

#cwd <- getwd()
#setwd("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/4. April 2021")

openxlsx::write.xlsx(data, 'Physiotherapy July 2021 Closed Charts Sampling Frame.xlsx', sheetName = 'Sheet1')
openxlsx::write.xlsx(data1, 'Physiotherapy July 2021 Closed Charts Sample sizes.xlsx', sheetName = 'Sheet1')

#setwd(cwd)