rm(list = ls())
start.time <- Sys.time()

library(tidyverse)
library(glue)
library(ggtext)
library(officer)
library(rvg)



thequarter <- "Q2"  ###Remember to change it as well as change line 145-ish



##Loading and preparing data

data <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/OME Details/OME DETAILS JAN - JUNE 2021.csv")
emergency <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/OME Details/A&E Closed Charts - Q1 - Q2 2021.csv")

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
rm(data)

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

data1 <- data1 %>% filter(Month != "Jul")    ####Remember to come and change this later!!!!

####Analysis and drawing
### 1. For comparative bar graphs


prepare.it.bar <- function(column){
  
  col_quosure <- rlang::enquo(column)
  
  intermediate <- data1 %>% group_by(Month, DEPT, !!col_quosure) %>% filter(!!col_quosure %in% c("Y", "N")) %>%
    summarise(Count = n()) %>% mutate(Percentage = (100 * Count)/sum(Count))
  
  
  intermediate1 <-intermediate %>% filter(!!col_quosure == "N" & Percentage == 100)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate1 <- intermediate1 %>% mutate(!!col_quosure := "Y") %>%
    mutate(Count = 0) %>%
    mutate(Percentage = 0)
  
  intermediate <- rbind(intermediate, intermediate1)
  intermediate <- intermediate %>% arrange(desc(Percentage)) %>% mutate_if(is.numeric, round, 1)
  
  rm(intermediate1) 
  return(intermediate)
  
}



finalize.it.bar <- function(dataframe, benchmark){
  
  dataframe %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50",
                                           (Percentage >= 50 & Percentage < benchmark) ~ "Above 50 but below benchmark",
                                            Percentage >= benchmark ~ "At or above benchmark")) %>% 
                mutate(Category = factor(Category, levels = c("At or above benchmark",
                                                              "Above 50 but below benchmark",
                                                              "Below 50")))
}


plot.it.bar <- function(dataframe){
  
  ggplot(dataframe, aes(x = Month, y = Percentage, fill = Category)) +
    geom_bar(stat = "identity") +
    facet_wrap(~DEPT, nrow = 2) +
    labs(x = "Month", y = "Compliance (%)") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.line = element_line(),
          panel.background = element_rect(fill = "gray96"),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) +
    scale_fill_manual(values = c("Below 50" = "red",
                                 "Above 50 but below benchmark" = "orange",
                                 "At or above benchmark" = "#00b159")) +
    scale_y_continuous(breaks = seq(0, 100, 10))
  
}
  
  
#Documented
  documented.bar <- prepare.it.bar(DOCUMENTED) %>% finalize.it.bar(., 100)
  
  documented.bar <- plot.it.bar(documented.bar)
  

#Timeliness
  timely.bar <- prepare.it.bar(TIMELY) %>% finalize.it.bar(., 90)
  
  timely.bar <- plot.it.bar(timely.bar)
  
#Legibility
  legible.bar <- prepare.it.bar(LEGIBLE) %>% finalize.it.bar(., 90)
  
  legible.bar <- plot.it.bar(legible.bar)
  
#Completeness
  complete.bar <- prepare.it.bar(COMPLETE) %>% finalize.it.bar(., 90)
  
  complete.bar <- plot.it.bar(complete.bar)
  
#Overall
  met.bar <- prepare.it.bar(MET) %>% finalize.it.bar(., 90)
  
  met.bar <- plot.it.bar(met.bar)

  
### 2. Special summaries

  special.documents <- function(dataset, column){
    
    
    col_quosure <- rlang::enquo(column)
    
    intermediate <- dataset %>% filter(!!col_quosure %in% c("Y", "N")) %>% group_by(Month, DEPT, !!col_quosure) %>%
      summarize(Count = n()) %>% mutate(Percentage = (100*Count/sum(Count)))
    
    
    intermediate1 <-intermediate %>% filter(!!col_quosure == "N" & Percentage == 100)
    intermediate <- intermediate %>% filter(!!col_quosure == "Y")
    intermediate1 <- intermediate1 %>% mutate(!!col_quosure := "Y") %>%
      mutate(Count = 0) %>%
      mutate(Percentage = 0)
    
    intermediate <- rbind(intermediate, intermediate1)
    intermediate <- intermediate %>% arrange(desc(Percentage)) %>% mutate_if(is.numeric, round, 1)
    
    rm(intermediate1) 
    
    
    intermediate <- intermediate %>% filter(!!col_quosure == "Y") %>% select(-Count) %>% mutate_if(is.numeric, round, 1) %>%
      mutate(Category = case_when(Percentage < 50 ~ "Below 50",
                                  (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark",
                                  Percentage >= 100 ~ "At or above benchmark")) %>%
      mutate(Category = factor(Category, levels = c("At or above benchmark", "Above 50 but below benchmark", "Below 50"))) %>% 
      
      ggplot(data = ., aes(x = Month, y = Percentage, fill = Category)) + geom_col() +
      scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange",
                                   "At or above benchmark" = "#00b159")) +
      scale_y_continuous(breaks = seq(0,100,10)) + theme(panel.background = element_rect(fill = "gray96"),
                                                         axis.line = element_line(),
                                                         plot.title = element_text(hjust = 0.5),
                                                         legend.title = element_blank(),
                                                         legend.position = "bottom") +
      labs(x = "Month", y = "Compliance (%)") + facet_wrap(~DEPT)
    
    return(intermediate)
    
  }
    
##a. Physician reports - Form without abbreviations

#Informed Consent - Use of abbreviations
  
  consent.abbreviations <- data1 %>% filter((DEPT %in% c("A&E", "Anaesthesia", "Maternity", "Medicine", "Paediatrics", "Surgery")) &
                     SNN == 5.5) %>% special.documents(., LEGIBLE)
  

#Discharge summaries
  
  discharge.abbreviations <- data1 %>% filter((DEPT %in% c("A&E", "Maternity", "Medicine", "Paediatrics", "Surgery"))
                             & DOCUMENTS == "Discharge summaries without Abbreviations") %>%
    special.documents(., LEGIBLE)
  
  
##b. Nursing reports
#Nursing Care Plan

  #Availability
  nursing.plan.documented <- data1 %>% filter((DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery")) &
                                                SNN == 1.8) %>% special.documents(., DOCUMENTED)
  
  #Completeness when documented
  nursing.plan.completeness <- data1 %>% filter((DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery")) &
                                                  SNN == 1.8) %>% special.documents(., COMPLETE)
  
#STAT medications

  stat.med.timely <- data1 %>% filter((DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery")) &
                                        SNN == 4.2) %>% special.documents(., TIMELY)
  
### 3. For loop
  
  departments <- unique(data1$DEPT)
  
  for(department in departments){
    
    data2 <- data1 %>% filter(DEPT == {department})

    
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
    intermediate <- intermediate %>% arrange(desc(Percentage)) %>% mutate_if(is.numeric, round, 1)
    
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
  documented <- prepare.control(data2, DOCUMENTED) %>% finalize.control(., 100)
  
  timely <- prepare.control(data2, TIMELY) %>% finalize.control(., 90)
  
  legible <- prepare.control(data2, LEGIBLE) %>% finalize.control(., 90)
  
  complete <- prepare.control(data2, COMPLETE) %>% finalize.control(., 90)
  
  overall <- prepare.control(data2, MET) %>% finalize.control(., 90)
  
  
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
    select(Quarter, DOCUMENTS, TYPE, !!col_quosure) %>%
    group_by(Quarter, DOCUMENTS, TYPE, !!col_quosure) %>%
    summarise(Count = n()) %>% filter(!!col_quosure %in% c("Y", "N")) %>%
    mutate(Percentage = (100*Count)/sum(Count))
  
  intermediate1 <-intermediate %>% filter(!!col_quosure == "N" & Percentage == 100)
  intermediate <- intermediate %>% filter(!!col_quosure == "Y")
  intermediate1 <- intermediate1 %>% mutate(!!col_quosure := "Y") %>%
                                     mutate(Count = 0) %>%
                                     mutate(Percentage = 0)
  
  intermediate <- rbind(intermediate, intermediate1)
  intermediate <- intermediate %>% arrange(desc(Percentage)) %>% mutate_if(is.numeric, round, 1)
  
  rm(intermediate1) 
  return(intermediate)
  
}


#Graphs - Aesthetics for all but Documented scores
  
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
documented_documents <- wrangle.it(data2, DOCUMENTED) %>% prepare.it(., 100) %>%
                          plot.it()


#Timely
timely_documents <- wrangle.it(data2, TIMELY) %>% prepare.it(., 90) %>% 
                      plot.it()


#Legible
legible_documents <- wrangle.it(data2, LEGIBLE) %>% prepare.it(., 90) %>% 
                      plot.it()


#Complete
complete_documents <- wrangle.it(data2, COMPLETE) %>% prepare.it(., 90) %>% 
                        plot.it()


#Overall
met_documents <- wrangle.it(data2, MET) %>% prepare.it(., 90) %>% 
                  plot.it()


##Exporting the graphs
#Setting the working directory

cwd <- getwd()

direc <- glue("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/Outputs/Departmental/{department}")

dir.create(direc, showWarnings = FALSE)
setwd(direc)

#Saving the pptx
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with(glue('{department} - Availability of Required Documents'), location = ph_location_type(type="title")) %>%
  ph_with(value = documented.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Documented Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = documented_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Comparative Documented Scores', location = ph_location_type(type="title")) %>%
  ph_with(value = documented.bar, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with(glue('{department} - Timeliness'), location = ph_location_type(type="title")) %>%
  ph_with(value = timely.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Timeliness Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = timely_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Comparative Timeliness Scores', location = ph_location_type(type="title")) %>%
  ph_with(value = timely.bar, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with(glue('{department} - Legibility'), location = ph_location_type(type="title")) %>%
  ph_with(value = legible.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Legibility Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = legible_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Comparative Legibility Scores', location = ph_location_type(type="title")) %>%
  ph_with(value = legible.bar, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with(glue('{department} - Completeness'), location = ph_location_type(type="title")) %>%
  ph_with(value = complete.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Completeness Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = complete_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Comparative Completeness Scores', location = ph_location_type(type="title")) %>%
  ph_with(value = complete.bar, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with(glue('{department} - Overall'), location = ph_location_type(type="title")) %>%
  ph_with(value = overall.controlchart, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Overall Scores per Form', location = ph_location_type(type="title")) %>%
  ph_with(value = met_documents, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Comparative Overall Scores', location = ph_location_type(type="title")) %>%
  ph_with(value = met.bar, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Consent forms without Abbreviations', location = ph_location_type(type="title")) %>%
  ph_with(value = consent.abbreviations, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Discharge Summaries without Abbreviations', location = ph_location_type(type="title")) %>%
  ph_with(value = discharge.abbreviations, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('STAT Medications - Timeliness', location = ph_location_type(type="title")) %>%
  ph_with(value = stat.med.timely, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing Care Plan - Availability', location = ph_location_type(type="title")) %>%
  ph_with(value = nursing.plan.documented, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing Care Plan - Completeness', location = ph_location_type(type="title")) %>%
  ph_with(value = nursing.plan.completeness, location = ph_location_type(type="body")) %>%
  
  print(glue("{department} - Closed Charts Results.pptx"))

setwd(cwd)

  # }
  
  
# rm(list = setdiff(ls(), "data1"))
  

###Scores per Consultant
doctors <- unique(data2$DOCTOR)

data3 <- data2 %>% filter((SNN != 7.1) & (TYPE == "P"))

for (doctor in doctors){
  
  if(nrow(data3[((data3$DOCTOR == {doctor}) & (data3$Quarter == thequarter)),]) == 0){
    
    next
  }
  
  data4 <- data3 %>% filter(DOCTOR == {doctor})

  #Control charts
  #Control charts - preparation & drawing
  documented.consultant <- prepare.control(data4, DOCUMENTED) %>% finalize.control(., 100) %>%
                            controlchart.plot()
  
  timely.consultant <- prepare.control(data4, TIMELY) %>% finalize.control(., 90) %>%
                            controlchart.plot()
  
  legible.consultant <- prepare.control(data4, LEGIBLE) %>% finalize.control(., 90) %>%
                            controlchart.plot()
  
  complete.consultant <- prepare.control(data4, COMPLETE) %>% finalize.control(., 90) %>%
                            controlchart.plot()
  
  overall.consultant <- prepare.control(data4, MET) %>% finalize.control(., 90) %>%
                            controlchart.plot()
  

  

  #Per form
  
  #Documented
  documented_documents_doc <- wrangle.it(data4, DOCUMENTED) %>% prepare.it(., 100) %>%
                                plot.it()
  
  
  #Timely
  timely_documents_doc <- wrangle.it(data4, TIMELY) %>% prepare.it(., 90) %>% 
                          plot.it()
  
  
  #Legible
  legible_documents_doc <- wrangle.it(data4, LEGIBLE) %>% prepare.it(., 90) %>% 
                            plot.it()
  
  
  #Complete
  complete_documents_doc <- wrangle.it(data4, COMPLETE) %>% prepare.it(., 90) %>% 
                              plot.it()
  
  
  #Overall
  met_documents_doc <- wrangle.it(data4, MET) %>% prepare.it(., 90) %>% 
                        plot.it()
  
  
  ##Exporting the Consultants graphs
  #Setting the working directory
  
  direc.doc <- glue("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/Outputs/Departmental/{department}/Consultants")
  
  dir.create(direc.doc, showWarnings = FALSE)
  setwd(direc.doc)
  
  
  #Saving the plots
  read_pptx() %>%
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with(glue('{doctor} - Availability of Required Documents'), location = ph_location_type(type="title")) %>%
    ph_with(value = documented.consultant, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with('Documented Scores per Form', location = ph_location_type(type="title")) %>%
    ph_with(value = documented_documents_doc, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with(glue('{doctor} - Timeliness'), location = ph_location_type(type="title")) %>%
    ph_with(value = timely.consultant, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with('Timeliness Scores per Form', location = ph_location_type(type="title")) %>%
    ph_with(value = timely_documents_doc, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with(glue('{doctor} - Legibility'), location = ph_location_type(type="title")) %>%
    ph_with(value = legible.consultant, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with('Legibility Scores per Form', location = ph_location_type(type="title")) %>%
    ph_with(value = legible_documents_doc, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with(glue('{doctor} - Completeness'), location = ph_location_type(type="title")) %>%
    ph_with(value = complete.consultant, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with('Completeness Scores per Form', location = ph_location_type(type="title")) %>%
    ph_with(value = complete_documents_doc, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with(glue('{doctor} - Overall'), location = ph_location_type(type="title")) %>%
    ph_with(value = overall.consultant, location = ph_location_type(type="body")) %>%
    
    add_slide(layout='Title and Content',master='Office Theme') %>%
    ph_with('Overall Scores per Form', location = ph_location_type(type="title")) %>%
    ph_with(value = met_documents_doc, location = ph_location_type(type="body")) %>%
    
    print(glue("{doctor} - Closed Charts Results.pptx"))
  
  setwd(cwd)
  
}

# rm(list = setdiff(ls(), c("data1", "start.time")))

##drawing graphs for A&E RMOs and Nurse at the same time

if (department == "A&E"){  
  
  data5 <- emergency1[, !names(emergency1) %in% c("DOCTOR")] #data5 used as data4 has already been used
  data5 <- mutate.doc.names(data5) %>%
    mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar",
                                            "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep",
                                            "Oct", "Nov", "Dec")))
  
  ##For RMOs
  data6 <- data5 %>% filter(RMO != "")
  
  rmos <- unique(data6$RMO)
  
  #For loop
  for (rmo in rmos){
    
    if(nrow(data6[((data6$RMO == {rmo}) & (data6$Quarter == thequarter)),]) == 0){
      
      next
    }
    
  
    
  data7 <- data6 %>% filter((RMO == {rmo}) & (TYPE == "P"))
    
    #Control charts
    #Control charts - preparation & drawing
    documented.rmo <- prepare.control(data7, DOCUMENTED) %>% finalize.control(., 100) %>%
      controlchart.plot()
    
    timely.rmo <- prepare.control(data7, TIMELY) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    legible.rmo <- prepare.control(data7, LEGIBLE) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    complete.rmo <- prepare.control(data7, COMPLETE) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    overall.rmo <- prepare.control(data7, MET) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    
    
    #Per form
    
    #Documented
    documented_documents_rmo <- wrangle.it(data7, DOCUMENTED) %>% prepare.it(., 100) %>%
      plot.it()
    
    
    #Timely
    timely_documents_rmo <- wrangle.it(data7, TIMELY) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    #Legible
    legible_documents_rmo <- wrangle.it(data7, LEGIBLE) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    #Complete
    complete_documents_rmo <- wrangle.it(data7, COMPLETE) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    #Overall
    met_documents_rmo <- wrangle.it(data7, MET) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    ##Exporting the RMOs graphs
    #Setting the working directory
    
    direc.rmo <- glue("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/Outputs/Departmental/{department}/RMOs")
    
    dir.create(direc.rmo, showWarnings = FALSE)
    setwd(direc.rmo)
    
    
    #Saving the plots
    read_pptx() %>%
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{rmo} - Availability of Required Documents'), location = ph_location_type(type="title")) %>%
      ph_with(value = documented.rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Documented Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = documented_documents_rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{rmo} - Timeliness'), location = ph_location_type(type="title")) %>%
      ph_with(value = timely.rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Timeliness Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = timely_documents_rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{rmo} - Legibility'), location = ph_location_type(type="title")) %>%
      ph_with(value = legible.rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Legibility Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = legible_documents_rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{rmo} - Completeness'), location = ph_location_type(type="title")) %>%
      ph_with(value = complete.rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Completeness Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = complete_documents_rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{rmo} - Overall'), location = ph_location_type(type="title")) %>%
      ph_with(value = overall.rmo, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Overall Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = met_documents_rmo, location = ph_location_type(type="body")) %>%
      
      print(glue("{rmo} - Closed Charts Results.pptx"))
    
  }
 
  
  ##For Nurses
  data8 <- data5 %>% filter(NURSE != "")
  
  nurses <- unique(data8$NURSE)
  
  #For loop
  for (nurse in nurses){
    
    if(nrow(data8[((data8$NURSE == {nurse}) & (data8$Quarter == thequarter)),]) == 0){
      
      next
    }
    
    
    
    data9 <- data8 %>% filter((NURSE == {nurse}) & (TYPE == "N"))
    
    #Control charts
    #Control charts - preparation & drawing
    documented.nurse <- prepare.control(data9, DOCUMENTED) %>% finalize.control(., 100) %>%
      controlchart.plot()
    
    timely.nurse <- prepare.control(data9, TIMELY) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    legible.nurse <- prepare.control(data9, LEGIBLE) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    complete.nurse <- prepare.control(data9, COMPLETE) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    overall.nurse <- prepare.control(data9, MET) %>% finalize.control(., 90) %>%
      controlchart.plot()
    
    
    
    #Per form
    
    #Documented
    documented_documents_nurse <- wrangle.it(data9, DOCUMENTED) %>% prepare.it(., 100) %>%
      plot.it()
    
    
    #Timely
    timely_documents_nurse <- wrangle.it(data9, TIMELY) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    #Legible
    legible_documents_nurse <- wrangle.it(data9, LEGIBLE) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    #Complete
    complete_documents_nurse <- wrangle.it(data9, COMPLETE) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    #Overall
    met_documents_nurse <- wrangle.it(data9, MET) %>% prepare.it(., 90) %>% 
      plot.it()
    
    
    ##Exporting the RMOs graphs
    #Setting the working directory
    
    direc.nurse <- glue("D:/Work/QPS/Closed Charts/Monthly working files/Monthly Closed Chart Audits/2021/Outputs/Departmental/{department}/Nurses")
    
    dir.create(direc.nurse, showWarnings = FALSE)
    setwd(direc.nurse)
    
    
    #Saving the plots
    read_pptx() %>%
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{nurse} - Availability of Required Documents'), location = ph_location_type(type="title")) %>%
      ph_with(value = documented.nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Documented Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = documented_documents_nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{nurse} - Timeliness'), location = ph_location_type(type="title")) %>%
      ph_with(value = timely.nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Timeliness Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = timely_documents_nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{nurse} - Legibility'), location = ph_location_type(type="title")) %>%
      ph_with(value = legible.nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Legibility Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = legible_documents_nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{nurse} - Completeness'), location = ph_location_type(type="title")) %>%
      ph_with(value = complete.nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Completeness Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = complete_documents_nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with(glue('{nurse} - Overall'), location = ph_location_type(type="title")) %>%
      ph_with(value = overall.nurse, location = ph_location_type(type="body")) %>%
      
      add_slide(layout='Title and Content',master='Office Theme') %>%
      ph_with('Overall Scores per Form', location = ph_location_type(type="title")) %>%
      ph_with(value = met_documents_nurse, location = ph_location_type(type="body")) %>%
      
      print(glue("{nurse} - Closed Charts Results.pptx"))
    
  }
   
}


}
 
end.time <- Sys.time()

time.diff <- round(end.time - start.time, 1)

time.diff