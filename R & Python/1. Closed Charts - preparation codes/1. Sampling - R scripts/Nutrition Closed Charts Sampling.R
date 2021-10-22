#Nutrition Closed Charts Sampling

rm(list=ls())

library(tidyverse)
# library(tabulizer)
# library(naniar)
library(docxtractr)

#Louisa's data (csv)
data1 <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021/Source/Minor departments/Louisa's patient log July 2021.csv")

#Rozina's data (pdf/csv)
# data <- extract_tables("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/3. March 2021/Source/Dietetics/LISA MARCH PATIENT LIST.pdf", method = "decide", output = "data.frame")
# data <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/6. June 2021/Source/Minor/Dietetics/Rozina Patient Log June .csv")

#Maureen's data (csv)
data2 <- read_docx("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021/Source/Minor departments/Maureen -JULY PATIENT LIST.docx")
data2 <- docx_extract_tbl(data2)

data2 <- data2 %>% select(Date, Name, Admission.Number, MR.Number)
data2 <- data.frame(data2)

##Preparation
#Rozina's data
# data <- data %>% pluck(1) %>% as_tibble()
# data %>% head() %>% knitr::kable()

#data <- as.data.frame(data[2:nrow(data), ])

# colnames(data) <- c("Date", "Admission.no", "Patient.name", "MR.number")
# 
# data$MR.number <- gsub("O", "0", data$MR.number)
# data$MR.number <- str_pad(data$MR.number, 7, side = "left", pad = "0")
# 
# data$Patient.name <- str_to_title(data$Patient.name)
# 
# data <- data %>% mutate(Consultant = "Rozina")
# 
# data <- data %>% mutate(Required = case_when(Consultant %in% "" ~ "Not required", TRUE ~ "Required"))
# data <- data %>%filter(Required == "Required")  %>% select(-Required)
# data <- data %>% extract(MR.number, c("First", "Second", "Third"), "(.{3})(.{2})(.{2})") %>% arrange(Consultant, Third, Second, First) %>% unite("MR.number", c("First", "Second", "Third"), sep = "-", remove = TRUE)
# data <- data %>% separate(MR.number, c('First', 'Second', 'Third'), remove = FALSE, sep = '-') %>% arrange(Consultant, Third, Second, First) %>% select(-(c(First, Second, Third)))

#Louisa's data
colnames(data1) <- c("Date", "Patient.name", "MR.number")
data1 <- data1 %>% select(Date, Patient.name, MR.number)

data1$MR.number <- str_pad(data1$MR.number, 7, side = "left", pad = "0")

data1$Patient.name <- str_to_title(data1$Patient.name)

data1 <- data1 %>% mutate(Consultant = "Louisa")
data1 <- data1 %>% separate(MR.number, c("First", "Second", "Third"), sep = "-") %>% arrange(Third, Second, First) %>% unite("MR.number", c("First", "Second", "Third"), sep = "-", remove = TRUE)

#Maureen's data 
colnames(data2) <- c("Date", "Patient.name", "Admission.no", "MR.number")
data2 <- data2 %>% select(Date, Patient.name, MR.number)

data2$MR.number <- str_pad(data2$MR.number, 7, side = "left", pad = "0")

data2$Patient.name <- str_to_title(data2$Patient.name)

data2 <- data2 %>% mutate(Consultant = "Maureen")
data2 <- data2 %>% separate(MR.number, c("First", "Second", "Third"), sep = "-") %>% arrange(Third, Second, First) %>% unite("MR.number", c("First", "Second", "Third"), sep = "-", remove = TRUE)


##Combining the three datasets
data <- rbind(data1, data2)
rm(data1, data2)

#Combined - Analysis
data1 <- data %>% count(Consultant) %>% rename(Patients = n)%>% mutate(Sample = case_when(Patients > 30 ~ (0.2 * Patients), 
                                                                                               TRUE ~ ((6.0025 * Patients)/(Patients + 6.0025 - 1)))) %>% mutate(Department = 'Nutrition')
data1 <- data1 %>% select(Department, Consultant:Sample)
data1$Sample <- data1$Sample %>% round(digits = 1)
data1$Sample <- ceiling(data1$Sample)

view(data)
View(data1)

cwd <- getwd()

setwd("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/7. July 2021")

openxlsx::write.xlsx(data, 'Nutrition July 2021 Closed Charts Sampling Frame.xlsx', sheetName = 'Sheet1')
openxlsx::write.xlsx(data1, 'Nutrition July 2021 Closed Charts Sample sizes.xlsx', sheetName = 'Sheet1')

setwd(cwd)





# ###Food for thought
# firstrow <- c(colnames(data2))
# 
# convert <- function(element){
#   
#   stringr::str_replace_all(string = element, pattern = "X", replacement = "")
#   
# }
# 
# 
# for (item in firstrow) {
#   
#   output <- vector()
#   
#   new_value <- convert(item)
#   
#   output <- append(output, new_value)
#   
#   print(output)
#   
# }