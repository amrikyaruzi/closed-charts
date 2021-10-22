
library(tidyverse)
data <- read.csv("C:/Users/amri.kyaruzi/Desktop/OHC Masaki Q1 2020.csv")
View(data)
sampling <- data %>% group_by(DOCTOR) %>% count() %>% rename(Patients = n)
sampling <- sampling %>% mutate(Sample = case_when(Patients > 30 ~ (0.2 * Patients), 
                            TRUE ~ ((6.0025 * Patients)/(Patients + 6.0025 - 1))))
sampling$Sample <- sampling$Sample %>% round(digits = 1)
sampling$Sample <- ceiling(sampling$Sample)
View(sampling)

sampling[16 , 2] <- sum(sampling$Patients)
sampling[16 , 3] <- sum(sampling$Sample, na.rm = TRUE)
sampling$DOCTOR <- as.character(sampling$DOCTOR)
sampling[16,1] <- 'SUM'

write.csv(sampling, 'OHC Masaki Q1 sample sizes.csv')
file.exists('OHC Masaki Q1 sample sizes.csv')
