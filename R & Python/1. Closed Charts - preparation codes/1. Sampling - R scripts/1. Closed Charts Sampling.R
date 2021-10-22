#Closed Charts Sampling
rm(list=ls())

library(tidyverse)
data <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/8. August 2021/Source/August 2021 Closed Charts.csv")
View(data)
data <- data %>% select(M.R..:Patient)
data <- na.omit(data)
data$Managed.Ward <- as.character(data$Managed.Ward)
data$Patient <- str_to_title(data$Patient)

data <- data %>% mutate(Ward = case_when(Managed.Ward %in% 'Cathlab' ~ 'Medicine', Managed.Ward %in% 'MEDICINE' ~ 'Medicine', Managed.Ward %in% 'PEADS' ~ 'Paediatrics', TRUE ~ Managed.Ward))
data <- data %>% arrange(Ward, Adm..Physician) %>% select(-(Managed.Ward))
data <- data %>% mutate(Department = case_when(Adm..Physician %in% c('Abeid, Muzdalifat', 'Damas, Wilson', 'Jaiswal, Shweta', 'Kaguta, Munawar', 'Kidanto, Hussein', 'Mdachi, Ernest', 'Mgonja, Miriam', 'Moshi, Lynn','Muzo, Jane', 'Ngarina, Matilda', 'Mujumali, Nyasinde') ~ 'Maternity', 
                                               Adm..Physician %in% c('Adatia, Aleesha', 'Adebayo, Philip', 'Amirali, Muzhar', 'Virani, Akbarali', 'Bakshi, Fatima', 'Bakshi, Fatma', 'Bapumia, Mustafa', 'Chuwa, Harrison', 'Epafra, Emmanuel', 'Shayo, Grace', 'Foi, Andrew', 'Hameed, Kamran', 'Hussain, Bilal', 'Jamal, Nasiruddin', 'Khuzeima, Kaderbhai', 'Khuzeima, Kanbhai', 'Lyuu, Tuzo','Makakala, Mandela', 'Masolwa, Deodatus', 'Mbatina, Consolata', 'Mbonea Salehe Yonazi', 'Mwambene, Kissah', 'Nyagori, Harun', 'Robert, Mvungi', 'Sangeti, Saningo', 'Somji, Samina', 'Tungaraza, Kheri', 'Wambura, Casmir', 'Khan, Zahra') ~ 'Medicine', 
                                               Adm..Physician %in% c('Abdallah, Yaser', 'Bulimba, Maria', 'Ebrahim. Mohamedraza', 'Kija. Edward', 'Kubhoja, Sulende', 'Mbise, Roger Lewis', 'Mwamanenge, Naomi', 'Noorani, Mariam', 'Patel, Sonal', 'Walli, Nahida') ~ 'Paediatrics', 
                                               Adm..Physician %in% c('Ali, Athar', 'Assey, Anthony', 'Bajsar, Ally', 'Soomro, Hussam', 'Clement, Mughisha', 'Dinda, Julius', 'Joseph, Alex', 'Kimu, Njiku', 'Kumar, Rajeev', 'Laurent, Lemery', 'Liyombo, Edwin', 'Lyimo, Elias', 'Mavura, Maurice', 'Mcharo, Bryson', 'Moshi, Ndeserua', 'Mrema, Edwin', 'Mrita, Felix', 'Mtanda, Tawakali', 'Mugabo, Rajab', 'Mwanga, Ally', 'Mwansasu, Christopher', 'Mbwambo, John', 'Ngerageza, Japhet', 'Ngiloi, Petronilla', 'Njau, Aidan', 'Padhani, Dilawar', 'Patel, Miten', 'Richard, Enica', 'Ringo, Yona', 'Sianga, William', 'Walter Charles Mbando (ENT)', 'Zehri, AliAkbar') ~ 'Surgery', TRUE ~ ''))
data <- data %>% select(-(Ward))
data$Adm..Physician <- as.character(data$Adm..Physician)
data <- data %>% mutate(Consultant = case_when(Adm..Physician %in% 'Bakshi, Fatima' ~ 'Bakshi, Fatma', TRUE ~ Adm..Physician)) %>% select(-(Adm..Physician))
data <- data %>% rename(MRN = M.R.., Admission_No = Admission., Discharge_Date = Discharge.Date)
data <- data %>% mutate(Discharge_Date = strptime(Discharge_Date, format = "%d/%m/%Y")) %>%
                 mutate(Discharge_Date = format(Discharge_Date, "%d/%m/%Y"))

data <- data %>% separate(MRN, c('First', 'Second', 'Third'), sep = '-', remove = FALSE)
data <- data %>% arrange(Department, Consultant, Third, Second, First, Discharge_Date) %>% select(-(c(First, Second, Third)))

data1 <- data %>% group_by(Department) %>% count(Consultant) %>% rename(Patients = n) %>% 
  mutate(Sample = case_when(Patients > 30 ~ (0.2 * Patients), 
                            TRUE ~ ((6.0025 * Patients)/(Patients + 6.0025 - 1))))
data1$Sample <- data1$Sample %>% round(digits = 1)
data1$Sample <- ceiling(data1$Sample)
View(data1)

setwd("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/8. August 2021")

openxlsx::write.xlsx(data, 'August 2021 Closed Charts Sampling Frame.xlsx', sheetName = 'Sheet1', overwrite = TRUE)
openxlsx::write.xlsx(data1, 'August 2021 Closed Charts Sample sizes.xlsx', sheetName = 'Sheet1', overwrite = TRUE)
