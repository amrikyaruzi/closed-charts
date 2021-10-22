rm(list = ls())
start.time <- Sys.time()

library(tidyverse)
library(glue)
library(ggtext)
library(officer)
library(rvg)



thequarter <- "Q3"  ###Remember to change it as well as change line 145-ish



##Loading and preparing data

data <- read.csv("C:/Users/amri.kyaruzi/OneDrive - Aga Khan Health Service, Tanzania/Documents/OME DETAILS JAN - AUG 2021.csv")
emergency <- read.csv("C:/Users/amri.kyaruzi/OneDrive - Aga Khan Health Service, Tanzania/Documents/A&E Closed Charts - July 2021.csv")

data <- data %>% filter(((DEPT == "DOA") &
                           (SNN == 1.3 | SNN == 4.1 | SNN == 4.2
                            #| SNN == 4.3
                            | SNN == 5.1 | SNN == 5.2 | SNN == 5.3 | SNN == 5.5 | SNN == 7.2)) |
                          ((DEPT == "DOG") & 
                             (SNN == 1.1 | SNN == 1.2 | SNN == 1.4 | SNN == 1.8 | SNN == 2.1 | SNN == 2.2 | SNN == 2.3 | SNN == 3.1 | SNN == 3.2 | SNN == 3.3 | SNN == 4.1 | SNN == 4.2 | SNN == 4.3 | SNN == 5.2 | SNN == 5.4 | SNN == 5.5 | SNN == 6.1 | SNN == 6.2 | SNN == 7.1 | SNN == 7.3 | SNN == 8.1)) |
                          ((DEPT == "DOM") &
                             (SNN == 1.1 | SNN == 1.2 | SNN == 1.3 | SNN == 1.4 | SNN == 1.8 | SNN == 2.1 | SNN == 2.2 | SNN == 2.3 | SNN == 3.1 | SNN == 3.2 | SNN == 3.3 | SNN == 4.1 | SNN == 4.2 | SNN == 5.2 | SNN == 5.4 | SNN == 5.5 | SNN == 6.1 | SNN == 6.2 | SNN == 7.1 | SNN == 7.3 | SNN == 8.1)) |
                          ((DEPT == "DON") & 
                             (SNN == 1.5 | SNN == 2.1 | SNN == 3.1 | SNN == 3.2)) |
                          ((DEPT == "DOP") & 
                             (SNN == 1.1 | SNN == 1.2 | SNN == 1.4 | SNN == 1.8 | SNN == 2.1 | SNN == 2.2 | SNN == 2.3 | SNN == 3.1 | SNN == 3.2 | SNN == 3.3 | SNN == 4.1 | SNN == 4.2 | SNN == 5.2 | SNN == 5.4 | SNN == 5.5 | SNN == 6.1 | SNN == 6.2 | SNN == 8.1)) |
                          ((DEPT == "DOS") & 
                             (SNN == 1.1 | SNN == 1.2 | SNN == 1.4 | SNN == 1.8 | SNN == 2.1 | SNN == 2.2 | SNN == 2.3 | SNN == 3.1 | SNN == 3.2 | SNN == 3.3 | SNN == 4.1 | SNN == 4.2 | SNN == 5.2 | SNN == 5.4 | SNN == 5.5 | SNN == 6.1 | SNN == 6.2 | SNN == 7.1 | SNN == 7.3 | SNN == 8.1)) |
                          ((DEPT == "RM") & 
                             (SNN == 1.4 | SNN == 1.6 | SNN == 1.7 | SNN == 2.1 | SNN == 3.1)))


data <- data %>% mutate(ADMDATE = strptime(ADMDATE, format = "%m/%d/%Y"), DISCHDATE = strptime(DISCHDATE, format = "%m/%d/%Y"), Month = format(DISCHDATE, "%b")) %>% select(AKNO:DISCHDATE, Month, DOC_CODE:MET) #Change month to come from DISCHDATE everywhere
data <- data %>% mutate(Quarter = quarters(DISCHDATE)) ##Added
data <- data %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

data1 <- data %>% mutate(DEPT = case_when(DEPT %in% "DOA" ~ "Anaesthesia", DEPT %in% "DOG" ~ "Maternity", DEPT %in% "DOM" ~ "Medicine", DEPT %in% "DON" ~ "Nutrition", DEPT %in% "DOP" ~ "Paediatrics", DEPT %in% "DOS" ~ "Surgery", DEPT %in% "RM" ~ "Physiotherapy"))

data1 <- data1 %>% select(AKNO, Month, Quarter, DOCTOR, DEPT, SNN:MET)

#Remember to put quarter here as well
emergency1 <- emergency %>% mutate(Quarter = case_when(Month %in% c("January", "February", "March") ~ "Q1",
                                                       Month %in% c("April", "May", "June") ~ "Q2",
                                                       Month %in% c("July", "August", "September") ~ "Q3",
                                                       Month %in% c("October", "November", "December") ~ "Q4"))

rm(emergency)

emergency1 <- emergency1 %>% select(MR.Number, Month, Quarter, DOCTOR, RMO, NURSE, DEPT:COMPLETE)
emergency1 <- emergency1 %>% filter(DOCUMENTED %in% c("D", "ND"))
emergency1 <- emergency1 %>%  mutate(MET = case_when(DOCUMENTED %in% "D" &
                                                       TIMELY %in% "Y" &
                                                       LEGIBLE %in% "Y" &
                                                       COMPLETE %in% "Y" ~ "M",
                                                     TRUE ~ "NM"))


emergency1$Month <- substr(emergency1$Month, 1,3)

emergency1 <- emergency1 %>% rename(AKNO = MR.Number)
emergency1 <- emergency1 %>% mutate(DEPT = "A&E")
emergency1 <- emergency1 %>% mutate(DOCUMENTED = case_when(DOCUMENTED %in% "D" ~ "Y",
                                                           DOCUMENTED %in% "ND" ~ "N",
                                                           TRUE ~ DOCUMENTED)) %>% 
  
  mutate(MET = case_when(MET %in% "M" ~ "Y",
                         MET %in% "NM" ~ "N",
                         TRUE ~ MET))


#Merging the datasets for Consultants

data1 <- rbind(data1, emergency1[, !names(emergency1) %in% c("RMO", "NURSE")])
rm(data, emergency1)

data1 <- data1 %>% mutate(DOCTOR = case_when(DEPT %in% "A&E" ~ DOCTOR,
                                             TRUE ~ str_replace_all(string = DOCTOR,
                                                                    pattern = "(.*)\\s(.*)",
                                                                    replacement = "\\2 \\1")))

data1 <- data1 %>% mutate(DOCTOR = gsub(",", "", DOCTOR))



##Preparing data

mutate.doc.names <- function(dataframe){
  
  dataframe %>% mutate(DOCUMENTS = case_when(
    DOCUMENTS == "Physician Initial Assessment All components" ~ "Physician Initial Assessment",
    DOCUMENTS == "Nursing Initial Assessment All components" ~ "Nursing Initial Assessment",
    DOCUMENTS == "Appropriate pain assessment, intervention and reassessment" ~ "Pain Assessment & Reassessment",
    DOCUMENTS == "Daily Round Notes by Physicians (ward/interdiciplinary ICU round)" ~ "Round Notes (SBAR)",
    DOCUMENTS == "Nursing Re-Assessment (MEWS, PEWS & MEOWS)" | DOCUMENTS == "Nursing Re-Assessment and Appropriate Fall reassessment" ~ "MEWS, NEWS, PEWS & MEOWS",
    DOCUMENTS == "Fall Risk Assessment, Intervention and Reassessment" ~ "Fall Risk Assessment",
    DOCUMENTS == "Multidisciplinary Patient and family education Form from arrival to discharge" ~ "Multidisciplinary Patient and Family Education",
    DOCUMENTS == "Patient care orders on uniform location (Doctor's part)" ~ "Physician Orders",
    DOCUMENTS == "Daily CPOE orders and Nursing signature for administration" ~ "CPOE orders - Nurse's Signature",
    DOCUMENTS == "Informed Consent Form (all procedures requiring consent i.e. anesthesia, sedation, BT, chemo, dialys" ~ "Informed Consent",
    DOCUMENTS == "Check the Discharge summary and DNR forms for any abbreviations" ~ "Discharge summaries without Abbreviations",
    DOCUMENTS == "Check Consent forms for any abbreviations" ~ "Consent forms without Abbreviations",
    DOCUMENTS == "Blood Transfusion Monitoring Form" ~ "Blood Transfusion Monitoring",
    DOCUMENTS == "Discharge/Transfer/Referral Summary at exit" ~ "Discharge summary",
    DOCUMENTS == "Pre-Operative Anesthesia/Sedation Assessment" ~ "Pre-Anaesthesia/Sedation Assessment",
    DOCUMENTS == "Intraoperative form for Site marking, sign in, time out and sign out" ~ "Site Marking, Sign in, Time out & Sign out",
    DOCUMENTS == "Surgical Procedure/ Operative notes" ~ "Procedure/ Operative notes",
    DOCUMENTS == "Pre-Operative (Anaesthesia) Induction Assessment" ~ "Pre-Induction Assessment",
    DOCUMENTS == "Post Anesthesia Recovery Form" ~ "Post-Anaesthesia Recovery",
    DOCUMENTS == "Intraoperative Notes of Anesthesia" ~ "Intraoperative Notes of Anesthesia",
    DOCUMENTS == "Physiotherapy Assessment All components" ~ "Physiotherapy Assessment",
    DOCUMENTS == "Physiotherapy Fall Risk Assessment" ~ "Physiotherapy Fall Risk Assessment",
    DOCUMENTS == "Nutritional Assessment All components" ~ "Nutritional Assessment",
    DOCUMENTS == "Patient care orders on uniform location (Nurse's part)" ~ "Physician Orders - Countersigning",
    DOCUMENTS == "STAT medications administration" ~ "STAT medications",
    DOCUMENTS == "Nursing Care Plan" ~ "Nursing Care Plan",
    DOCUMENTS == "Blood Transfusion Order" ~ "Blood Transfusion Order",
    DOCUMENTS == "Daily CPOE orders and Physician signature for administration" ~ "CPOE orders - Doctor's Signature",
    DOCUMENTS == "Nursing Re-Assessment and Appropriate Fall reassessment" ~ "MEWS, NEWS, PEWS & MEOWS",
    DOCUMENTS == "Check Consent, DNR and discharge summary for any abbreviations" ~ "Consent forms without Abbreviations",
    TRUE ~ DOCUMENTS)) %>%
    
    mutate(DOCUMENTED = case_when(DOCUMENTED %in% "D" ~ "Y",
                                  DOCUMENTED %in% "ND" ~ "N",
                                  TRUE ~ DOCUMENTED)) %>% 
    
    mutate(MET = case_when(MET %in% "M" ~ "Y",
                           MET %in% "NM" ~ "N",
                           TRUE ~ MET))
  
}


data1 <- mutate.doc.names(data1)



###Important - to be changed

data1 <- data1 %>% filter(Month %in% c('Jul', 'Aug', 'Sep'))    ####Remember to come and change this later!!!!

### a. For Control charts

##Functions
#Prepare the data

prepare.control <- function(dataframe, column){
  
  col_quosure <- rlang::enquo(column)
  
  intermediate <- dataframe %>% select(Month, DEPT, TYPE, !!col_quosure) %>% group_by(Month, DEPT, !!col_quosure) %>% 
    filter(!!col_quosure %in% c("Y", "N")) %>% summarize(Frequency = n()) %>%
    mutate(Percentage = (100*Frequency)/sum(Frequency))
  
  
  intermediate1 <-intermediate %>% filter(!!col_quosure == "N" & Percentage == 100)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate1 <- intermediate1 %>% mutate(!!col_quosure := "Y") %>%
    mutate(Frequency = 0) %>%
    mutate(Percentage = 0)
  
  intermediate <- rbind(intermediate, intermediate1)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate <- intermediate %>% group_by(Month) %>%
    mutate(Frequency = sum(Frequency), Percentage = mean(Percentage)) %>% mutate(DEPT = "Institutional") %>%
    distinct(., Month, .keep_all = TRUE) %>% mutate_if(is.numeric, round, 1)
  
  rm(intermediate1) 
  return(intermediate)
  
}


#Create the control charts

finalize.control <- function(dataframe, benchmark){
  dataframe %>% mutate(Mean = mean(dataframe$Percentage)) %>% 
    mutate(UCL = Mean + 2*(sd(dataframe$Percentage)),
           LCL = Mean - 2*(sd(dataframe$Percentage)),
           Benchmark = benchmark) %>% mutate_if(is.numeric, round, 1)
}


#Output the control charts dataframes
documented <- prepare.control(data1, DOCUMENTED) %>% finalize.control(., 100)

timely <- prepare.control(data1, TIMELY) %>% finalize.control(., 90)

legible <- prepare.control(data1, LEGIBLE) %>% finalize.control(., 90)

complete <- prepare.control(data1, COMPLETE) %>% finalize.control(., 90)

overall <- prepare.control(data1, MET) %>% finalize.control(., 90)


##Draw the graphs
#Function for drawing control charts

controlchart.plot <- function(dataframe){
  
  ggplot(data = dataframe, aes(x = Month, y = Percentage, group = 1)) +
    geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) +
    geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) +
    geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) +
    geom_hline(aes(yintercept = Mean, colour = "Mean")) +
    geom_hline(aes(yintercept = LCL, colour = "LCL")) +
    scale_y_continuous(breaks = seq(-20,200,10)) +
    
    geom_text(label = dataframe$Percentage,
              hjust = -0.5,
              size = 2,
              position = position_dodge(width = 1)) +
    
    theme(plot.title = element_text(hjust = 0.5),
          axis.line = element_line(),
          panel.background = element_rect(fill = "gray98"),
          legend.background = element_rect(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    xlab("Month") + ylab("Compliance (%)")
  
}


#Drawing the graphs

documented.controlchart <- controlchart.plot(documented)

timely.controlchart <- controlchart.plot(timely)

legible.controlchart <- controlchart.plot(legible)

complete.controlchart <- controlchart.plot(complete)

overall.controlchart <- controlchart.plot(overall)


### 2. For graphs per form

wrangle.it <- function(dataframe, column){
  
  col_quosure <- rlang::enquo(column)
  
  intermediate <- dataframe %>%
    
    filter(Quarter == {thequarter}) %>%                       ###Remember this
    select(Quarter, DEPT, DOCUMENTS, TYPE, !!col_quosure) %>%
    group_by(Quarter, DOCUMENTS, DEPT, !!col_quosure) %>%
    summarise(Count = n()) %>% filter(!!col_quosure %in% c("Y", "N")) %>%
    mutate(Percentage = (100*Count)/sum(Count))
  
  intermediate1 <-intermediate %>% filter(!!col_quosure == "N" & Percentage == 100)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate1 <- intermediate1 %>% mutate(!!col_quosure := "Y") %>%
    mutate(Count = 0) %>%
    mutate(Percentage = 0)
  
  intermediate <- rbind(intermediate, intermediate1)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate <- intermediate %>% group_by(Quarter, DOCUMENTS) %>%
    mutate(Count = sum(Count), Percentage = mean(Percentage)) %>% mutate(DEPT = "Institutional") %>%
    distinct(., Quarter, DOCUMENTS, .keep_all = TRUE) %>% mutate_if(is.numeric, round, 1)
  
  rm(intermediate1) 
  return(intermediate)
  
}


#Graphs - Aesthetics for all

prepare.it <- function(dataframe, benchmark){
  
  dataframe %>%
    
    mutate(Category = case_when(Percentage < 50 ~ "Below 50",
                                (Percentage >= 50 & Percentage < benchmark) ~ "Above 50 but below benchmark",
                                Percentage >= benchmark ~ "At or above benchmark")) %>%
    
    mutate(Category = factor(Category, levels = c("At or above benchmark",
                                                  "Above 50 but below benchmark",
                                                  "Below 50")))
  
}


#Graphing - plotting it and adding more layers

plot.it <- function(dataframe){
  
  ggplot(dataframe, aes(x = reorder(DOCUMENTS, Percentage), y = Percentage, fill = Category)) +
    geom_col() +
    coord_flip() + 
    labs(x = "Form", y = "Compliance (%)") +
    theme(legend.position = "bottom", legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5), axis.line = element_line(),
          panel.background = element_rect(fill = "gray96"),
          axis.text.x = element_text(hjust = 0.5, vjust = 0.8)) +
    scale_fill_manual(values = c("At or above benchmark" = "#00b159",
                                 "Above 50 but below benchmark" = "orange",
                                 "Below 50" = "red")) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    geom_text(label = dataframe$Percentage,
              hjust = -0.5,  #changed from 0.5
              size = 2,
              position = position_dodge(width = 1)) +
    
    facet_wrap(~Quarter)
  
}


##The dataframes & graphs

#Documented
documented_documents <- wrangle.it(data1, DOCUMENTED) %>% prepare.it(., 100) %>%
  plot.it()


#Timely
timely_documents <- wrangle.it(data1, TIMELY) %>% prepare.it(., 90) %>% 
  plot.it()


#Legible
legible_documents <- wrangle.it(data1, LEGIBLE) %>% prepare.it(., 90) %>% 
  plot.it()


#Complete
complete_documents <- wrangle.it(data1, COMPLETE) %>% prepare.it(., 90) %>% 
  plot.it()


#Overall
met_documents <- wrangle.it(data1, MET) %>% prepare.it(., 90) %>% 
  plot.it()


###Exporting the plots
##Exporting them
#Creating the directory

cwd <- getwd()

direc.institutional <- "D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/Outputs/Institutional"

dir.create(direc.institutional, showWarnings = FALSE)
setwd(direc.institutional)

##Final exportation

read_pptx() %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Institutional - Availability of Required Documents', location = ph_location_type(type="title")) %>%
  ph_with(value = documented.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Documented Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = documented_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Institutional - Timeliness', location = ph_location_type(type="title")) %>%
  ph_with(value = timely.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Timeliness Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = timely_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Institutional - Legibility', location = ph_location_type(type="title")) %>%
  ph_with(value = legible.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Legibility Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = legible_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Institutional - Completeness', location = ph_location_type(type="title")) %>%
  ph_with(value = complete.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Completeness Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = complete_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Institutional - Overall', location = ph_location_type(type="title")) %>%
  ph_with(value = overall.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Overall Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = met_documents, location = ph_location_type(type="body")) %>%
  
  print('Institutional - Closed Charts Audit Report.pptx')
  
setwd(cwd)

rm(list = ls())



###Institutional RCGC summaries

summarize.institutional <- function(dataframe, column){
  
  col_quosure <- rlang::enquo(column)
  
  intermediate <- dataframe %>% select(Month, DEPT, !!col_quosure) %>% group_by(Month, DEPT, !!col_quosure) %>% 
    filter(!!col_quosure %in% c("Y", "N")) %>% summarize(Frequency = n()) %>%
    mutate(Percentage = (100*Frequency)/sum(Frequency))
  
  
  intermediate1 <-intermediate %>% filter(!!col_quosure == "N" & Percentage == 100)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate1 <- intermediate1 %>% mutate(!!col_quosure := "Y") %>%
    mutate(Frequency = 0) %>%
    mutate(Percentage = 0)
  
  intermediate <- rbind(intermediate, intermediate1)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate <- intermediate %>% group_by(Month) %>%
    mutate(Frequency = sum(Frequency), Percentage = mean(Percentage)) %>% mutate(DEPT = "Institutional") %>%
    distinct(., Month, .keep_all = TRUE) %>% mutate_if(is.numeric, round, 1)
  
  rm(intermediate1)
  
quarterly.results <- intermediate %>% ungroup() %>% summarize(Month = 'Quarter',
                                           TIMELY = 'Y',
                                           Frequency = sum(Frequency),
                                           Percentage = mean(Percentage))
  
  #return(intermediate)
  return(quarterly.results)
  
}


timely <- summarize.institutional(data1, TIMELY)
legible <- summarize.institutional(data1, LEGIBLE)
complete <- summarize.institutional(data1, COMPLETE)

quarterly_average <- round(mean(c(timely$Percentage, legible$Percentage, complete$Percentage)), 1)

print(quarterly_average)
print(timely$Percentage)
print(legible$Percentage)
print(complete$Percentage)

###Couldn't use
##Function to export it

# export_it <- function(title, object) {
#   
#   add_slide(layout='Title and Content',master='Office Theme') %>%
#     ph_with((title), location = ph_location_type(type="title")) %>%
#     ph_with(value = object, location = ph_location_type(type="body"))
#   
# }


# read_pptx() %>%
#   
#   export_it('Institutional - Availability of Required Documents', documented.controlchart) %>%
#   export_it('Documented Scores per Form/ Parameter', documented_documents) %>%
#   export_it('Institutional - Timeliness', timely.controlchart) %>% 
#   export_it('Timeliness Scores per Form/ Parameter', timely_documents) %>%
#   export_it('Institutional - Legibility', legible.controlchart) %>% 
#   export_it('Legibility Scores per Form/ Parameter', legible_documents) %>%
#   export_it('Institutional - Completeness', complete.controlchart) %>%
#   export_it('Completeness Scores per Form/ Parameter', complete_documents) %>% 
#   export_it('Institutional - Overall', overall.controlchart) %>%
#   export_it(title = 'Overall Scores per Form/ Parameter', met_documents) %>% 
#   
#   print('Institutional - Closed Charts Audit Report.pptx')

#rm(list = ls())
