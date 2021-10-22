#Control charts
rm(list = ls())
library(tidyverse)
library(officer)
library(rvg)
library(openxlsx)

data <- read.csv("C:/Users/amri.kyaruzi/Desktop/Closed Charts - to be sent 2020/OME January - December 2020.csv")
emergency <- read.csv("C:/Users/amri.kyaruzi/Desktop/Closed Charts - to be sent 2020/A&E January - December 2020.csv")

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


data <- data %>% mutate(ADMDATE = strptime(ADMDATE, format = "%m/%d/%Y"), DISCHDATE = strptime(DISCHDATE, format = "%m/%d/%Y"), Month = format(ADMDATE, "%b")) %>% select(AKNO:DISCHDATE, Month, DOC_CODE:MET)
data <- data %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

#data <- data %>% filter(Month %in% c("Jul", "Aug", "Sept"))

data1 <- data %>% mutate(DEPT = case_when(DEPT %in% "DOA" ~ "Anaesthesia", DEPT %in% "DOG" ~ "Maternity", DEPT %in% "DOM" ~ "Medicine", DEPT %in% "DON" ~ "Nutrition", DEPT %in% "DOP" ~ "Paediatrics", DEPT %in% "DOS" ~ "Surgery", DEPT %in% "RM" ~ "Physiotherapy"))

data1 <- data1 %>% select(AKNO, Month, DOCTOR, DEPT, SNN:MET)
emergency <- emergency %>% select(MR.Number, Month, DOCTOR, DEPT:COMPLETE)
emergency <- emergency %>% filter(DOCUMENTED %in% c("D", "ND"))
emergency <- emergency %>% mutate(MET = case_when(DOCUMENTED %in% "D" & TIMELY %in% "Y" & LEGIBLE %in% "Y" & COMPLETE %in% "Y" ~ "M", TRUE ~ "NM"))
emergency$Month <- substr(emergency$Month, 1,3)

emergency <- emergency %>% rename(AKNO = MR.Number)
emergency <- emergency %>% mutate(DEPT = "A&E")

data1 <- rbind(data1, emergency)
rm(emergency, data)

#data1[data1$DOCTOR == "Shaffin",]$DOCTOR <- "Shaffin Rajan"
#data1[data1$DOCTOR == "KIlalo Mjema",]$DOCTOR <- "Kilalo Mjema"
#data1[data1$DOCTOR == "Biita",]$DOCTOR <- "Muhanuzi Biita"

#data1$DOCTOR[(data1$DOCTOR == "Hussein Manji") & (data1$Month == "Apr")] <- ""

#Institutional data for Graphs & presentations - the Excel way
##Documented
documented_scores <- data1 %>% group_by(Month, DEPT, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(DOCUMENTED == "D") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), DOCUMENTED = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

documented_scores <- documented_scores %>%
  add_row(Month = "Q1", Count = sum(documented_scores[c(1,2,3),2]), DOCUMENTED = apply(documented_scores[c(1,2,3),3], 2, mean)) %>% 
  add_row(Month = "Q2", Count = sum(documented_scores[c(4,5,6),2]), DOCUMENTED = apply(documented_scores[c(4,5,6),3], 2, mean)) %>%
  add_row(Month = "Q3", Count = sum(documented_scores[c(7,8,9),2]), DOCUMENTED = apply(documented_scores[c(7,8,9),3], 2, mean)) %>%
  add_row(Month = "Q4", Count = sum(documented_scores[c(10,11,12),2]), DOCUMENTED = apply(documented_scores[c(10,11,12),3], 2, mean)) %>%
  mutate_if(is.numeric, round, 1)
documented_scores <- documented_scores %>% add_row(Month = "Annual", Count = sum(documented_scores[c(13,14,15,16),2]), DOCUMENTED = apply(documented_scores[c(13,14,15,16),3], 2, mean))

documented_scores <- documented_scores %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Q1", "Apr", "May", "Jun", "Q2", "Jul",
                                                           "Aug", "Sep", "Q3", "Oct", "Nov", "Dec", "Q4", "Annual")), DOCUMENTED = round(DOCUMENTED, 1)) %>% arrange(Month)
documented_scores <- documented_scores %>%select(Month, DOCUMENTED) %>% pivot_wider(names_from = Month, values_from = DOCUMENTED)

##Timeliness
timely_scores <- data1 %>% group_by(Month, DEPT, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(TIMELY == "Y") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), TIMELY = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

timely_scores <- timely_scores %>% add_row(Month = "Q1", Count = sum(timely_scores[c(1,2,3),2]), TIMELY = apply(timely_scores[c(1,2,3),3], 2, mean)) %>% 
  add_row(Month = "Q2", Count = sum(timely_scores[c(4,5,6),2]), TIMELY = apply(timely_scores[c(4,5,6),3], 2, mean)) %>%
  add_row(Month = "Q3", Count = sum(timely_scores[c(7,8,9),2]), TIMELY = apply(timely_scores[c(7,8,9),3], 2, mean)) %>%
  add_row(Month = "Q4", Count = sum(timely_scores[c(10,11,12),2]), TIMELY = apply(timely_scores[c(10,11,12),3], 2, mean)) %>%
  mutate_if(is.numeric, round, 1)
timely_scores <- timely_scores %>% add_row(Month = "Annual", Count = sum(timely_scores[c(13,14,15,16),2]), TIMELY = apply(timely_scores[c(13,14,15,16),3], 2, mean))

timely_scores <- timely_scores %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Q1", "Apr", "May", "Jun", "Q2", "Jul",
                                                                           "Aug", "Sep", "Q3", "Oct", "Nov", "Dec", "Q4", "Annual")), TIMELY = round(TIMELY, 1)) %>% arrange(Month)
timely_scores <- timely_scores %>%select(Month, TIMELY) %>% pivot_wider(names_from = Month, values_from = TIMELY)

##Legibility
legible_scores <- data1 %>% group_by(Month, DEPT, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(LEGIBLE == "Y") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), LEGIBLE = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

legible_scores <- legible_scores %>% add_row(Month = "Q1", Count = sum(legible_scores[c(1,2,3),2]), LEGIBLE = apply(legible_scores[c(1,2,3),3], 2, mean)) %>% 
  add_row(Month = "Q2", Count = sum(legible_scores[c(4,5,6),2]), LEGIBLE = apply(legible_scores[c(4,5,6),3], 2, mean)) %>%
  add_row(Month = "Q3", Count = sum(legible_scores[c(7,8,9),2]), LEGIBLE = apply(legible_scores[c(7,8,9),3], 2, mean)) %>%
  add_row(Month = "Q4", Count = sum(legible_scores[c(10,11,12),2]), LEGIBLE = apply(legible_scores[c(10,11,12),3], 2, mean)) %>%
  mutate_if(is.numeric, round, 1)
legible_scores <- legible_scores %>% add_row(Month = "Annual", Count = sum(legible_scores[c(13,14,15,16),2]), LEGIBLE = apply(legible_scores[c(13,14,15,16),3], 2, mean))

legible_scores <- legible_scores %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Q1", "Apr", "May", "Jun", "Q2", "Jul",
                                                                             "Aug", "Sep", "Q3", "Oct", "Nov", "Dec", "Q4", "Annual")), LEGIBLE = round(LEGIBLE, 1)) %>% arrange(Month)
legible_scores <- legible_scores %>%select(Month, LEGIBLE) %>% pivot_wider(names_from = Month, values_from = LEGIBLE)

##Completeness
complete_scores <- data1 %>% group_by(Month, DEPT, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(COMPLETE == "Y") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), COMPLETE = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

complete_scores <- complete_scores %>% add_row(Month = "Q1", Count = sum(complete_scores[c(1,2,3),2]), COMPLETE = apply(complete_scores[c(1,2,3),3], 2, mean)) %>% 
  add_row(Month = "Q2", Count = sum(complete_scores[c(4,5,6),2]), COMPLETE = apply(complete_scores[c(4,5,6),3], 2, mean)) %>%
  add_row(Month = "Q3", Count = sum(complete_scores[c(7,8,9),2]), COMPLETE = apply(complete_scores[c(7,8,9),3], 2, mean)) %>%
  add_row(Month = "Q4", Count = sum(complete_scores[c(10,11,12),2]), COMPLETE = apply(complete_scores[c(10,11,12),3], 2, mean)) %>%
  mutate_if(is.numeric, round, 1)
complete_scores <- complete_scores %>% add_row(Month = "Annual", Count = sum(complete_scores[c(13,14,15,16),2]), COMPLETE = apply(complete_scores[c(13,14,15,16),3], 2, mean))

complete_scores <- complete_scores %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Q1", "Apr", "May", "Jun", "Q2", "Jul",
                                                                               "Aug", "Sep", "Q3", "Oct", "Nov", "Dec", "Q4", "Annual")), COMPLETE = round(COMPLETE, 1)) %>% arrange(Month)
complete_scores <- complete_scores %>%select(Month, COMPLETE) %>% pivot_wider(names_from = Month, values_from = COMPLETE)


##Overall
overall_scores <- data1 %>% group_by(Month, DEPT, MET) %>% filter(MET %in% c("M", "NM")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(MET == "M") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), MET = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

overall_scores <- overall_scores %>% add_row(Month = "Q1", Count = sum(overall_scores[c(1,2,3),2]), MET = apply(overall_scores[c(1,2,3),3], 2, mean)) %>% 
  add_row(Month = "Q2", Count = sum(overall_scores[c(4,5,6),2]), MET = apply(overall_scores[c(4,5,6),3], 2, mean)) %>%
  add_row(Month = "Q3", Count = sum(overall_scores[c(7,8,9),2]), MET = apply(overall_scores[c(7,8,9),3], 2, mean)) %>%
  add_row(Month = "Q4", Count = sum(overall_scores[c(10,11,12),2]), MET = apply(overall_scores[c(10,11,12),3], 2, mean)) %>%
  mutate_if(is.numeric, round, 1)
overall_scores <- overall_scores %>% add_row(Month = "Annual", Count = sum(overall_scores[c(13,14,15,16),2]), MET = apply(overall_scores[c(13,14,15,16),3], 2, mean))

overall_scores <- overall_scores %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Q1", "Apr", "May", "Jun", "Q2", "Jul",
                                                                             "Aug", "Sep", "Q3", "Oct", "Nov", "Dec", "Q4", "Annual")), MET = round(MET, 1)) %>% arrange(Month)
overall_scores <- overall_scores %>%select(Month, MET) %>% pivot_wider(names_from = Month, values_from = MET)


##Exporting them
# setwd("C:/Users/amri.kyaruzi/Documents/Graphs/Institutional")
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Documented score', location = ph_location_type(type="title")) %>%
  ph_with(value = documented_scores, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Timeliness score', location = ph_location_type(type="title")) %>%
  ph_with(value = timely_scores, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Legibility score', location = ph_location_type(type="title")) %>%
  ph_with(value = legible_scores, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Completeness score', location = ph_location_type(type="title")) %>%
  ph_with(value = complete_scores, location = ph_location_type(type="body")) %>%
  
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Overall score', location = ph_location_type(type="title")) %>%
  ph_with(value = overall_scores, location = ph_location_type(type="body")) %>%
  print("Institutional - scores.pptx")


###Graphs
##Control charts

#Documented scores
documented <- data1 %>% group_by(Month, DEPT, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(DOCUMENTED == "D") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), Percentage = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

documented <- documented %>% mutate(Mean = mean(documented$Percentage)) %>% mutate(UCL = Mean + 2*(sd(documented$Percentage)), LCL = Mean - 2*(sd(documented$Percentage)), Benchmark = 100) %>% mutate_if(is.numeric, round, 1)

#Timeliness scores
timely <- data1 %>% group_by(Month, DEPT, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(TIMELY == "Y") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), Percentage = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

timely <- timely %>% mutate(Mean = mean(timely$Percentage), UCL = Mean + 2*(sd(timely$Percentage)), LCL = Mean - 2*(sd(timely$Percentage)), Benchmark = 90) %>% mutate_if(is.numeric, round, 1)

#Legible
legible <- data1 %>% group_by(Month, DEPT, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(LEGIBLE == "Y") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), Percentage = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

legible <- legible %>% mutate(Mean = mean(legible$Percentage), UCL = Mean + 2*(sd(legible$Percentage)), LCL = Mean - 2*(sd(legible$Percentage)), Benchmark = 90) %>% mutate_if(is.numeric, round, 1)

#Complete
complete <- data1 %>% group_by(Month, DEPT, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(COMPLETE == "Y") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), Percentage = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

complete <- complete %>% mutate(Mean = mean(complete$Percentage), UCL = Mean + 2*(sd(complete$Percentage)), LCL = Mean - 2*(sd(complete$Percentage)), Benchmark = 90) %>% mutate_if(is.numeric, round, 1)

#Overall
overall <- data1 %>% group_by(Month, DEPT, MET) %>% filter(MET %in% c("M", "NM")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(MET == "M") %>% mutate_if(is.numeric, round, 1) %>% group_by(Month) %>%
  summarise(Count = sum(Count), Percentage = mean(Percentage)) %>% mutate_if(is.numeric, round, 1)

overall <- overall %>% mutate(Mean = mean(overall$Percentage), UCL = Mean + 2*(sd(overall$Percentage)), LCL = Mean - 2*(sd(overall$Percentage)), Benchmark = 90)  %>% mutate_if(is.numeric, round, 1)

##Control charts graphs
documented_plot <- ggplot(data = documented, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(80,105,1)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("INSTITUTIONAL - AVAILABILITY OF REQUIRED DOCUMENTS")
timely_plot <- ggplot(data = timely, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(0,105,3)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("INSTITUTIONAL - TIMELINESS")
legible_plot <- ggplot(data = legible, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(0,105,5)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("INSTITUTIONAL - LEGIBILITY")
complete_plot <- ggplot(data = complete, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(0,105,3)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("INSTITUTIONAL - COMPLETENESS")
overall_plot <- ggplot(data = overall, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(0,105,5)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("INSTITUTIONAL - OVERALL")

#Exporting them
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Documented score', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= documented_plot),
          location = ph_location_type(type="body")) %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Timely score', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= timely_plot),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Legible score', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= legible_plot),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Complete score', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= complete_plot),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Overall score', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= overall_plot),
          location = ph_location_type(type="body")) %>% 
  print('Institutional - plots.pptx')

rm(documented, timely, legible, complete, overall, documented_plot, timely_plot, legible_plot,
   complete_plot, overall_plot)


##Graph faceted per department
#Data

#Documented
documented.for.bar.graph <- data1 %>% group_by(Month, DEPT, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(DOCUMENTED == "D") %>%
  mutate_if(is.numeric, round, 1)
  
documented.for.bar.graph <- documented.for.bar.graph %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark", Percentage >= 100 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Timeliness
timely.for.bar.graph <- data1 %>% group_by(Month, DEPT, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(TIMELY == "Y") %>%
  mutate_if(is.numeric, round, 1)

timely.for.bar.graph <- timely.for.bar.graph %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Legibility
legible.for.bar.graph <- data1 %>% group_by(Month, DEPT, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(LEGIBLE == "Y") %>%
  mutate_if(is.numeric, round, 1)

legible.for.bar.graph <- legible.for.bar.graph %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Completeness
complete.for.bar.graph <- data1 %>% group_by(Month, DEPT, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(COMPLETE == "Y") %>%
  mutate_if(is.numeric, round, 1)

complete.for.bar.graph <- complete.for.bar.graph %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Overall
met.for.bar.graph <- data1 %>% group_by(Month, DEPT, MET) %>% filter(MET %in% c("M", "NM")) %>%
 summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(MET == "M") %>%
mutate_if(is.numeric, round, 1)

met.for.bar.graph <- met.for.bar.graph %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

##Graphs
#Rectangular background - faceted by Department
documented.graph.faceted.department <- ggplot(documented.for.bar.graph, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "OVERALL - AVAILABILITY OF REQUIRED DOCUMENTS SCORES", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

timely.graph.faceted.department <- ggplot(timely.for.bar.graph, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "TIMELINESS - COMPARATIVE SCORES", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))


legible.graph.faceted.department <- ggplot(legible.for.bar.graph, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "LEGIBILITY - COMPARATIVE SCORES", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

complete.graph.faceted.department <- ggplot(complete.for.bar.graph, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "COMPLETENESS - COMPARATIVE SCORES", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

met.graph.faceted.department <- ggplot(met.for.bar.graph, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "OVERALL - COMPARATIVE SCORES", x = "Month", y = "Compliance (%)") +
    theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))
  

#Exporting them
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Documented comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= documented.graph.faceted.department),
          location = ph_location_type(type="body")) %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Timeliness comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= timely.graph.faceted.department),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Legibility comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= legible.graph.faceted.department),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Completeness comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= complete.graph.faceted.department),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Overall comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj=met.graph.faceted.department),
          location = ph_location_type(type="body")) %>% 
  print('Institutional - comparative plots.pptx')

rm(documented.for.bar.graph, documented.graph.faceted.department, timely.for.bar.graph, timely.graph.faceted.department
   ,legible.for.bar.graph, legible.graph.faceted.department, complete.for.bar.graph, complete.graph.faceted.department,
   met.for.bar.graph, met.graph.faceted.department)

setwd("C:/Users/amri.kyaruzi/Documents")

##Others - additional graphs
##Graph faceted by Month
#With non flipped coordinates
ggplot(data2, aes(x = DEPT, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~Month, nrow = 2) + labs(title = "COMPARATIVE SCORES", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.line = element_line(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.background = element_rect(fill = "gray96")) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

#With flipped coordinates
ggplot(data2, aes(x = DEPT, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~Month, nrow = 3) + labs(title = "COMPARATIVE SCORES", x = "Department", y = "Compliance (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96")) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10)) + coord_flip()



##Institutional special summaries
setwd("C:/Users/amri.kyaruzi/Documents/Graphs/Special Summaries/For Doctors")

##Physician documents
#Use of abbreviations on Informed consent forms
consent.abbreviations <- data1 %>% filter((DEPT %in% c("A&E", "Anaesthesia", "Maternity", "Medicine", "Paediatrics", "Surgery")) & SNN == 5.5) %>%
  filter(LEGIBLE %in% c("Y", "N")) %>% group_by(Month, DEPT, LEGIBLE) %>% summarize(Count = n()) %>%
  mutate(Percentage = (100*Count/sum(Count))) %>% filter(LEGIBLE == "Y") %>% select(-Count) %>% mutate_if(is.numeric, round, 1)

consent.abbreviations <- consent.abbreviations %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark", Percentage >= 100 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

consent.abbreviations.graph <- ggplot(data = consent.abbreviations, aes(x = Month, y = Percentage, fill = Category)) + geom_col() +
  scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0,100,10)) + theme(panel.background = element_rect(fill = "gray96"), axis.line = element_line(), plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Informed Consent forms WITHOUT abbreviations", x = "Month", y = "Compliance (%)") + facet_wrap(~DEPT)

#Use of abbreviation on Discharge summaries
discharge.abbreviations <- data1 %>% filter((DEPT %in% c("A&E", "Maternity", "Medicine", "Paediatrics", "Surgery"))
                                            & DOCUMENTS == "Check the Discharge summary and DNR forms for any abbreviations") %>%
  filter(LEGIBLE %in% c("Y", "N")) %>% group_by(Month, DEPT, LEGIBLE) %>% summarize(Count = n()) %>%
  mutate(Percentage = (100*Count/sum(Count))) %>% filter(LEGIBLE == "Y") %>% select(-Count) %>% mutate_if(is.numeric, round, 1)

discharge.abbreviations <- discharge.abbreviations %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark", Percentage >= 100 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

discharge.abbreviations.graph <- ggplot(data = discharge.abbreviations, aes(x = Month, y = Percentage, fill = Category)) + geom_col() +
  scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0,100,10)) + theme(panel.background = element_rect(fill = "gray96"), axis.line = element_line(), plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Discharge summaries WITHOUT abbreviations", x = "Month", y = "Compliance (%)") + facet_wrap(~DEPT)

#Exporting them
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Consent forms WITHOUT abbreviations', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= consent.abbreviations.graph),
          location = ph_location_type(type="body")) %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Discharge summaries WITHOUT abbreviations', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= discharge.abbreviations.graph),
          location = ph_location_type(type="body")) %>% 
  print('Summaries for Doctors.pptx')

rm(consent.abbreviations, consent.abbreviations.graph, discharge.abbreviations, discharge.abbreviations.graph)

###Other Physician documents - All reports
#Documented
documented.for.bar.graph.physicians <- data1 %>% filter(TYPE == "P") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(DOCUMENTED == "D") %>%
  mutate_if(is.numeric, round, 1)

documented.for.bar.graph.physicians <- documented.for.bar.graph.physicians %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark", Percentage >= 100 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Timeliness
timely.for.bar.graph.physicians <- data1 %>% filter(TYPE == "P") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(TIMELY == "Y") %>%
  mutate_if(is.numeric, round, 1)

timely.for.bar.graph.physicians <- timely.for.bar.graph.physicians %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Legibility
legible.for.bar.graph.physicians <- data1 %>% filter(TYPE == "P") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(LEGIBLE == "Y") %>%
  mutate_if(is.numeric, round, 1)

legible.for.bar.graph.physicians <- legible.for.bar.graph.physicians %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Completeness
complete.for.bar.graph.physicians <- data1 %>% filter(TYPE == "P") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(COMPLETE == "Y") %>%
  mutate_if(is.numeric, round, 1)

complete.for.bar.graph.physicians <- complete.for.bar.graph.physicians %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Overall
met.for.bar.graph.physicians <- data1 %>% filter(TYPE == "P") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, MET) %>% filter(MET %in% c("M", "NM")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(MET == "M") %>%
  mutate_if(is.numeric, round, 1)

met.for.bar.graph.physicians <- met.for.bar.graph.physicians %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))


##Graphs
#Rectangular background - faceted by Department
documented.graph.faceted.department.physicians <- ggplot(documented.for.bar.graph.physicians, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "DOCTORS - AVAILABILITY OF REQUIRED DOCUMENTS", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

timely.graph.faceted.department.physicians <- ggplot(timely.for.bar.graph.physicians, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "DOCTORS - TIMELINESS", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))


legible.graph.faceted.department.physicians <- ggplot(legible.for.bar.graph.physicians, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "DOCTORS - LEGIBILITY", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

complete.graph.faceted.department.physicians <- ggplot(complete.for.bar.graph.physicians, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "DOCTORS - COMPLETENESS", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

met.graph.faceted.department.physicians <- ggplot(met.for.bar.graph.physicians, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "DOCTORS - OVERALL", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))


#Exporting them
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Doctors - Documented comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= documented.graph.faceted.department.physicians),
          location = ph_location_type(type="body")) %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Doctors - Timeliness comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= timely.graph.faceted.department.physicians),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Doctors - Legibility comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= legible.graph.faceted.department.physicians),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Doctors - Completeness comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= complete.graph.faceted.department.physicians),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Doctors - Overall comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj=met.graph.faceted.department.physicians),
          location = ph_location_type(type="body")) %>% 
  print('Doctors - Institutional Comparative plots.pptx')

rm(documented.for.bar.graph.physicians, documented.for.bar.graph, documented.graph.faceted.department.physicians, timely.for.bar.graph.physicians, timely.graph.faceted.department.physicians,
   legible.for.bar.graph.physicians, legible.graph.faceted.department.physicians, complete.for.bar.graph.physicians, complete.graph.faceted.department.physicians,
   met.for.bar.graph.physicians, met.graph.faceted.department.physicians)

##Nursing documents
#STAT medications
setwd("C:/Users/amri.kyaruzi/Documents/Graphs/Special Summaries/For Nurses")

stat.data <- data1 %>% filter((DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery")) & SNN == 4.2) %>%
  filter(TIMELY %in% c("Y", "N")) %>% group_by(Month, DEPT, TIMELY) %>% summarize(Count = n()) %>%
  mutate(Percentage = (100*Count/sum(Count))) %>% filter(TIMELY == "Y") %>% select(-Count) %>% mutate_if(is.numeric, round, 1)

stat.data <- stat.data %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark", Percentage >= 100 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

stat.data.graph <- ggplot(data = stat.data, aes(x = Month, y = Percentage, fill = Category)) + geom_col() +
  scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0,100,10)) + theme(panel.background = element_rect(fill = "gray96"), axis.line = element_line(), plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Percentage of STAT CPOE orders where medications were administered within 1 hour", x = "Month", y = "Compliance (%)") + facet_wrap(~DEPT)

#Nursing care plans
#Documenting Nursing Care plan when required
nursing.care.data <- data1 %>% filter((DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery")) & SNN == 1.8) %>%
  filter(DOCUMENTED %in% c("D", "ND")) %>% group_by(Month, DEPT, DOCUMENTED) %>% summarize(Count = n()) %>%
  mutate(Percentage = (100*Count/sum(Count))) %>% select(-Count) %>% mutate_if(is.numeric, round, 1)

nursing.care.data <- nursing.care.data %>% mutate(DOCUMENTED = case_when(DOCUMENTED == "D" ~ "Documented", DOCUMENTED == "ND" ~ "Not documented"))

notdocumented <- nursing.care.data %>% filter((DOCUMENTED == "Not documented") & (Percentage == 100))
notdocumented <- notdocumented %>% mutate(DOCUMENTED = "Documented", Percentage = 0)
nursing.care.data <- rbind(nursing.care.data, notdocumented)
rm(notdocumented)

nursing.care.data <- nursing.care.data %>% filter(DOCUMENTED == "Documented")

nursing.care.data <- nursing.care.data %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark", Percentage >= 100 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))


nursing.care.graph <- ggplot(data = nursing.care.data, aes(x = Month, y = Percentage, fill = Category)) +
  geom_col(position = "dodge")  + scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) +
  theme(panel.background = element_rect(fill = "gray96"), axis.line = element_line(), plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Nursing Care Plan - Availability in the file", x = "Month", y = "Compliance (%)") + facet_wrap(~DEPT, scales = "free_x")


#Completeness of the Nursing Care plan when documented
nursing.care.completeness <- data1 %>% filter((DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery")) & SNN == 1.8) %>%
  filter(COMPLETE %in% c("Y", "N")) %>% group_by(Month, DEPT, COMPLETE) %>% summarize(Count = n()) %>%
  mutate(Percentage = (100*Count/sum(Count))) %>% select(-Count) %>% mutate_if(is.numeric, round, 1)

nursing.care.completeness <- nursing.care.completeness %>% mutate(COMPLETE = case_when(COMPLETE == "Y" ~ "Complete", COMPLETE == "N" ~ "Not complete"))

notcomplete <- nursing.care.completeness %>% filter(COMPLETE == "Not complete" & Percentage == 100)
notcomplete <- notcomplete %>% mutate(COMPLETE = "Complete", Percentage = 0)
nursing.care.completeness <- rbind(nursing.care.completeness, notcomplete)

rm(notcomplete)

nursing.care.completeness <- nursing.care.completeness %>% filter(COMPLETE == "Complete")

nursing.care.completeness <- nursing.care.completeness %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

nursing.care.completeness.graph <- ggplot(data = nursing.care.completeness, aes(x = Month, y = Percentage, fill = Category)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) +
  scale_y_continuous(breaks = seq(0,100,10)) + theme(panel.background = element_rect(fill = "gray96"), axis.line = element_line(), plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Nursing Care Plan - Completeness when documented", x = "Month", y = "Compliance (%)") + facet_wrap(~DEPT, scales = "free_y")

###Other Nursing documents - All reports
#Documented
documented.for.bar.graph.nursing <- data1 %>% filter(TYPE == "N") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(DOCUMENTED == "D") %>%
  mutate_if(is.numeric, round, 1)

documented.for.bar.graph.nursing <- documented.for.bar.graph.nursing %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 100) ~ "Above 50 but below benchmark", Percentage >= 100 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Timeliness
timely.for.bar.graph.nursing <- data1 %>% filter(TYPE == "N") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(TIMELY == "Y") %>%
  mutate_if(is.numeric, round, 1)

timely.for.bar.graph.nursing <- timely.for.bar.graph.nursing %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Legibility
legible.for.bar.graph.nursing <- data1 %>% filter(TYPE == "N") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(LEGIBLE == "Y") %>%
  mutate_if(is.numeric, round, 1)

legible.for.bar.graph.nursing <- legible.for.bar.graph.nursing %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Completeness
complete.for.bar.graph.nursing <- data1 %>% filter(TYPE == "N") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(COMPLETE == "Y") %>%
  mutate_if(is.numeric, round, 1)

complete.for.bar.graph.nursing <- complete.for.bar.graph.nursing %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

#Overall
met.for.bar.graph.nursing <- data1 %>% filter(TYPE == "N") %>% filter(DEPT %in% c("Maternity", "Medicine", "Paediatrics", "Surgery", "A&E", "Anaesthesia")) %>% group_by(Month, DEPT, MET) %>% filter(MET %in% c("M", "NM")) %>%
  summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>% filter(MET == "M") %>%
  mutate_if(is.numeric, round, 1)

met.for.bar.graph.nursing <- met.for.bar.graph.nursing %>% mutate(Category = case_when(Percentage < 50 ~ "Below 50", (Percentage >= 50 & Percentage < 90) ~ "Above 50 but below benchmark", Percentage >= 90 ~ "At or above benchmark")) %>%
  mutate(Category = factor(Category, levels = c("Below 50", "Above 50 but below benchmark", "At or above benchmark")))

##Graphs
#Rectangular background - faceted by Department
documented.graph.faceted.department.nursing <- ggplot(documented.for.bar.graph.nursing, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "NURSING - AVAILABILITY OF REQUIRED DOCUMENTS", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

timely.graph.faceted.department.nursing <- ggplot(timely.for.bar.graph.nursing, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "NURSING - TIMELINESS", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))


legible.graph.faceted.department.nursing <- ggplot(legible.for.bar.graph.nursing, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "NURSING - LEGIBILITY", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

complete.graph.faceted.department.nursing <- ggplot(complete.for.bar.graph.nursing, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "NURSING - COMPLETENESS", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))

met.graph.faceted.department.nursing <- ggplot(met.for.bar.graph.nursing, aes(x = Month, y = Percentage, fill = Category)) + geom_bar(stat = "identity") + facet_wrap(~DEPT, nrow = 2) + labs(title = "NURSING - OVERALL", x = "Month", y = "Compliance (%)") +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray96"), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.8)) + scale_fill_manual(values = c("Below 50" = "red", "Above 50 but below benchmark" = "orange", "At or above benchmark" = "#00b159")) + scale_y_continuous(breaks = seq(0, 100, 10))


#Exporting them
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('STAT medications - Administration', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= stat.data.graph),
          location = ph_location_type(type="body")) %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing Care plan - Documentation', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= nursing.care.graph),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing care plan - Completeness', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= nursing.care.completeness.graph),
          location = ph_location_type(type="body")) %>% 
  print('Nursing - special plots.pptx')


#Exporting them
read_pptx() %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing - Documented comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= documented.graph.faceted.department.nursing),
          location = ph_location_type(type="body")) %>%
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing - Timeliness comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= timely.graph.faceted.department.nursing),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing - Legibility comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= legible.graph.faceted.department.nursing),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing - Completeness comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj= complete.graph.faceted.department.nursing),
          location = ph_location_type(type="body")) %>% 
  add_slide(layout='Title and Content',master='Office Theme') %>%
  ph_with('Nursing - Overall comparative graph', location = ph_location_type(type="title")) %>%
  ph_with(dml(ggobj=met.graph.faceted.department.nursing),
          location = ph_location_type(type="body")) %>% 
  print('Nursing Institutional - comparative plots.pptx')

rm(documented_scores, timely_scores, legible_scores, complete_scores, overall_scores, nursing.care.data, nursing.care.graph, stat.data, stat.data.graph,
   nursing.care.completeness, nursing.care.completeness.graph)

setwd("C:/Users/amri.kyaruzi/Documents")

###Special reports
##Overall - quarterly
#Scores per Nurse
#Scores per RMO
#Scores per Consultant

#Control charts - Institutional
#pwd <- getwd()
#setwd("C:/Users/amri.kyaruzi/Documents/Graphs/Institutional")


####Documents - summary - to be used
documented <- data1 %>% select(Month, DOCUMENTS, TYPE, DOCUMENTED) %>%
  group_by(DOCUMENTS, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
  summarize(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(DOCUMENTED == "D") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 100) %>%
  arrange(Percentage)

timely <- data1 %>% select(Month, DOCUMENTS, TYPE, TIMELY) %>%
  group_by(DOCUMENTS, TYPE, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(TIMELY == "Y") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)

legible <- data1 %>% select(Month, DOCUMENTS, TYPE, LEGIBLE) %>%
  group_by(DOCUMENTS, TYPE, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(LEGIBLE == "Y") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)

complete <- data1 %>% select(Month, DOCUMENTS, TYPE, COMPLETE) %>%
  group_by(DOCUMENTS, TYPE, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(COMPLETE == "Y") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)

met <- data1 %>% select(Month, DOCUMENTS, TYPE, MET) %>%
  group_by(DOCUMENTS, TYPE, MET) %>% filter(MET %in% c("M", "NM")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(MET == "M") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)


##Removing other dataframes
rm(complete, complete_scores, consent.abbreviations, consent.abbreviations.graph, discharge.abbreviations, discharge.abbreviations.graph,
   documented, documented_scores, legible, legible_scores, met, nursing.care.completeness, nursing.care.completeness.graph,
   nursing.care.data, nursing.care.graph, overall_scores, stat.data, stat.data.graph, timely, timely_scores)



###Closed Charts per Consultant for each department
summary <- data1 %>% filter(TYPE == "P")

#For Physiotherapy
#summary <- data1 %>% filter(TYPE %in% c("P", "N"))

#Documented
documented.consultant <- summary %>% filter(DOCTOR != "") %>% filter(DOCUMENTED %in% c("D", "ND")) %>% 
  group_by(DEPT, Month, DOCTOR, DOCUMENTED) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(DOCUMENTED == "D") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-DOCUMENTED) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

documented.consultant <- documented.consultant %>%
  mutate(Q1 = apply(documented.consultant[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(documented.consultant[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(documented.consultant[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(documented.consultant[,c(12,13,14)], 1, mean, na.rm = T))

# #Special
# documented.consultant$Apr[documented.consultant$DOCTOR == "Hussein Manji"] <- NA
# documented.consultant$Q2[documented.consultant$DOCTOR == "Hussein Manji"] <- NA


documented.consultant <- documented.consultant %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
documented.consultant <- documented.consultant %>% mutate(Annual = apply(documented.consultant[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Timely
timely.consultant <- summary %>% filter(DOCTOR != "") %>% filter(TIMELY %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, DOCTOR, TIMELY) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(TIMELY == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-TIMELY) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

timely.consultant <- timely.consultant %>%
  mutate(Q1 = apply(timely.consultant[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(timely.consultant[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(timely.consultant[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(timely.consultant[,c(12,13,14)], 1, mean, na.rm = T))

#Special
# timely.consultant$Apr[timely.consultant$DOCTOR == "Hussein Manji"] <- NA
# timely.consultant$Q2[timely.consultant$DOCTOR == "Hussein Manji"] <- NA

timely.consultant <- timely.consultant %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
timely.consultant <- timely.consultant %>% mutate(Annual = apply(timely.consultant[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Legible
legible.consultant <- summary %>% filter(DOCTOR != "") %>% filter(LEGIBLE %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, DOCTOR, LEGIBLE) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(LEGIBLE == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-LEGIBLE) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

legible.consultant <- legible.consultant %>%
  mutate(Q1 = apply(legible.consultant[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(legible.consultant[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(legible.consultant[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(legible.consultant[,c(12,13,14)], 1, mean, na.rm = T))

legible.consultant <- legible.consultant %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
legible.consultant <- legible.consultant %>% mutate(Annual = apply(legible.consultant[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Complete
complete.consultant <- summary %>% filter(DOCTOR != "") %>% filter(COMPLETE %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, DOCTOR, COMPLETE) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(COMPLETE == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-COMPLETE) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

complete.consultant <- complete.consultant %>%
  mutate(Q1 = apply(complete.consultant[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(complete.consultant[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(complete.consultant[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(complete.consultant[,c(12,13,14)], 1, mean, na.rm = T))

# #Special
# complete.consultant$Apr[complete.consultant$DOCTOR == "Hussein Manji"] <- NA
# complete.consultant$Q2[complete.consultant$DOCTOR == "Hussein Manji"] <- NA


complete.consultant <- complete.consultant %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
complete.consultant <- complete.consultant %>% mutate(Annual = apply(complete.consultant[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Overall
met.consultant <- summary %>% filter(DOCTOR != "") %>% filter(MET %in% c("M", "NM")) %>% 
  group_by(DEPT, Month, DOCTOR, MET) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(MET == "M") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-MET) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

met.consultant <- met.consultant %>%
  mutate(Q1 = apply(met.consultant[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(met.consultant[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(met.consultant[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(met.consultant[,c(12,13,14)], 1, mean, na.rm = T))

met.consultant <- met.consultant %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
met.consultant <- met.consultant %>% mutate(Annual = apply(met.consultant[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)

setwd("C:/Users/amri.kyaruzi/Documents/Graphs/Scores")

##Exporting
#A&E
ae.documented <- documented.consultant %>% filter(DEPT == "A&E")
ae.timely <- timely.consultant %>% filter(DEPT == "A&E")
ae.legible <- legible.consultant %>% filter(DEPT == "A&E")
ae.complete <- complete.consultant %>% filter(DEPT == "A&E")
ae.overall <- met.consultant %>% filter(DEPT == "A&E")

dae.report <- createWorkbook()
addWorksheet(dae.report, "Documented")
addWorksheet(dae.report, "Timely")
addWorksheet(dae.report, "Legible")
addWorksheet(dae.report, "Complete")
addWorksheet(dae.report, "Overall")

writeData(dae.report, "Documented", ae.documented, startRow = 1, startCol = 1)
writeData(dae.report, "Timely", ae.timely, startRow = 1, startCol = 1)
writeData(dae.report, "Legible", ae.legible, startRow = 1, startCol = 1)
writeData(dae.report, "Complete", ae.complete, startRow = 1, startCol = 1)
writeData(dae.report, "Overall", ae.overall, startRow = 1, startCol = 1)

saveWorkbook(dae.report, file = "A&E Report.xlsx", overwrite = TRUE)

rm(dae.report, ae.complete, ae.documented, ae.legible, ae.overall, ae.timely)

#Anaesthesia
anaesthesia.documented <- documented.consultant %>% filter(DEPT == "Anaesthesia")
anaesthesia.timely <- timely.consultant %>% filter(DEPT == "Anaesthesia")
anaesthesia.legible <- legible.consultant %>% filter(DEPT == "Anaesthesia")
anaesthesia.complete <- complete.consultant %>% filter(DEPT == "Anaesthesia")
anaesthesia.overall <- met.consultant %>% filter(DEPT == "Anaesthesia")

anaesthesia.report <- createWorkbook()
addWorksheet(anaesthesia.report, "Documented")
addWorksheet(anaesthesia.report, "Timely")
addWorksheet(anaesthesia.report, "Legible")
addWorksheet(anaesthesia.report, "Complete")
addWorksheet(anaesthesia.report, "Overall")

writeData(anaesthesia.report, "Documented", anaesthesia.documented, startRow = 1, startCol = 1)
writeData(anaesthesia.report, "Timely", anaesthesia.timely, startRow = 1, startCol = 1)
writeData(anaesthesia.report, "Legible", anaesthesia.legible, startRow = 1, startCol = 1)
writeData(anaesthesia.report, "Complete", anaesthesia.complete, startRow = 1, startCol = 1)
writeData(anaesthesia.report, "Overall", anaesthesia.overall, startRow = 1, startCol = 1)

saveWorkbook(anaesthesia.report, file = "Anaesthesia Report.xlsx", overwrite = TRUE)

rm(anaesthesia.report, anaesthesia.complete, anaesthesia.documented, anaesthesia.legible, anaesthesia.overall, anaesthesia.timely)

#Maternity
maternity.documented <- documented.consultant %>% filter(DEPT == "Maternity")
maternity.timely <- timely.consultant %>% filter(DEPT == "Maternity")
maternity.legible <- legible.consultant %>% filter(DEPT == "Maternity")
maternity.complete <- complete.consultant %>% filter(DEPT == "Maternity")
maternity.overall <- met.consultant %>% filter(DEPT == "Maternity")

maternity.report <- createWorkbook()
addWorksheet(maternity.report, "Documented")
addWorksheet(maternity.report, "Timely")
addWorksheet(maternity.report, "Legible")
addWorksheet(maternity.report, "Complete")
addWorksheet(maternity.report, "Overall")

writeData(maternity.report, "Documented", maternity.documented, startRow = 1, startCol = 1)
writeData(maternity.report, "Timely", maternity.timely, startRow = 1, startCol = 1)
writeData(maternity.report, "Legible", maternity.legible, startRow = 1, startCol = 1)
writeData(maternity.report, "Complete", maternity.complete, startRow = 1, startCol = 1)
writeData(maternity.report, "Overall", maternity.overall, startRow = 1, startCol = 1)

saveWorkbook(maternity.report, file = "Maternity Report.xlsx", overwrite = TRUE)

rm(maternity.report, maternity.complete, maternity.documented, maternity.legible, maternity.overall, maternity.timely)


#Medicine
medicine.documented <- documented.consultant %>% filter(DEPT == "Medicine")
medicine.timely <- timely.consultant %>% filter(DEPT == "Medicine")
medicine.legible <- legible.consultant %>% filter(DEPT == "Medicine")
medicine.complete <- complete.consultant %>% filter(DEPT == "Medicine")
medicine.overall <- met.consultant %>% filter(DEPT == "Medicine")

medicine.report <- createWorkbook()
addWorksheet(medicine.report, "Documented")
addWorksheet(medicine.report, "Timely")
addWorksheet(medicine.report, "Legible")
addWorksheet(medicine.report, "Complete")
addWorksheet(medicine.report, "Overall")

writeData(medicine.report, "Documented", medicine.documented, startRow = 1, startCol = 1)
writeData(medicine.report, "Timely", medicine.timely, startRow = 1, startCol = 1)
writeData(medicine.report, "Legible", medicine.legible, startRow = 1, startCol = 1)
writeData(medicine.report, "Complete", medicine.complete, startRow = 1, startCol = 1)
writeData(medicine.report, "Overall", medicine.overall, startRow = 1, startCol = 1)

saveWorkbook(medicine.report, file = "Medicine Report.xlsx", overwrite = TRUE)

rm(medicine.report, medicine.complete, medicine.documented, medicine.legible, medicine.overall, medicine.timely)

#Paediatrics
paediatrics.documented <- documented.consultant %>% filter(DEPT == "Paediatrics")
paediatrics.timely <- timely.consultant %>% filter(DEPT == "Paediatrics")
paediatrics.legible <- legible.consultant %>% filter(DEPT == "Paediatrics")
paediatrics.complete <- complete.consultant %>% filter(DEPT == "Paediatrics")
paediatrics.overall <- met.consultant %>% filter(DEPT == "Paediatrics")

paediatrics.report <- createWorkbook()
addWorksheet(paediatrics.report, "Documented")
addWorksheet(paediatrics.report, "Timely")
addWorksheet(paediatrics.report, "Legible")
addWorksheet(paediatrics.report, "Complete")
addWorksheet(paediatrics.report, "Overall")

writeData(paediatrics.report, "Documented", paediatrics.documented, startRow = 1, startCol = 1)
writeData(paediatrics.report, "Timely", paediatrics.timely, startRow = 1, startCol = 1)
writeData(paediatrics.report, "Legible", paediatrics.legible, startRow = 1, startCol = 1)
writeData(paediatrics.report, "Complete", paediatrics.complete, startRow = 1, startCol = 1)
writeData(paediatrics.report, "Overall", paediatrics.overall, startRow = 1, startCol = 1)

saveWorkbook(paediatrics.report, file = "Paediatrics Report.xlsx", overwrite = TRUE)

rm(paediatrics.report, paediatrics.complete, paediatrics.documented, paediatrics.legible, paediatrics.overall, paediatrics.timely)

#Surgery
surgery.documented <- documented.consultant %>% filter(DEPT == "Surgery")
surgery.timely <- timely.consultant %>% filter(DEPT == "Surgery")
surgery.legible <- legible.consultant %>% filter(DEPT == "Surgery")
surgery.complete <- complete.consultant %>% filter(DEPT == "Surgery")
surgery.overall <- met.consultant %>% filter(DEPT == "Surgery")

surgery.report <- createWorkbook()
addWorksheet(surgery.report, "Documented")
addWorksheet(surgery.report, "Timely")
addWorksheet(surgery.report, "Legible")
addWorksheet(surgery.report, "Complete")
addWorksheet(surgery.report, "Overall")

writeData(surgery.report, "Documented", surgery.documented, startRow = 1, startCol = 1)
writeData(surgery.report, "Timely", surgery.timely, startRow = 1, startCol = 1)
writeData(surgery.report, "Legible", surgery.legible, startRow = 1, startCol = 1)
writeData(surgery.report, "Complete", surgery.complete, startRow = 1, startCol = 1)
writeData(surgery.report, "Overall", surgery.overall, startRow = 1, startCol = 1)

saveWorkbook(surgery.report, file = "Surgery Report.xlsx", overwrite = TRUE)

rm(surgery.report, surgery.complete, surgery.documented, surgery.legible, surgery.overall, surgery.timely)

#Physiotherapy
physiotherapy.documented <- documented.consultant %>% filter(DEPT == "Physiotherapy")
physiotherapy.timely <- timely.consultant %>% filter(DEPT == "Physiotherapy")
physiotherapy.legible <- legible.consultant %>% filter(DEPT == "Physiotherapy")
physiotherapy.complete <- complete.consultant %>% filter(DEPT == "Physiotherapy")
physiotherapy.overall <- met.consultant %>% filter(DEPT == "Physiotherapy")

physiotherapy.report <- createWorkbook()
addWorksheet(physiotherapy.report, "Documented")
addWorksheet(physiotherapy.report, "Timely")
addWorksheet(physiotherapy.report, "Legible")
addWorksheet(physiotherapy.report, "Complete")
addWorksheet(physiotherapy.report, "Overall")

writeData(physiotherapy.report, "Documented", physiotherapy.documented, startRow = 1, startCol = 1)
writeData(physiotherapy.report, "Timely", physiotherapy.timely, startRow = 1, startCol = 1)
writeData(physiotherapy.report, "Legible", physiotherapy.legible, startRow = 1, startCol = 1)
writeData(physiotherapy.report, "Complete", physiotherapy.complete, startRow = 1, startCol = 1)
writeData(physiotherapy.report, "Overall", physiotherapy.overall, startRow = 1, startCol = 1)

saveWorkbook(physiotherapy.report, file = "Physiotherapy Report.xlsx", overwrite = TRUE)

rm(physiotherapy.report, physiotherapy.complete, physiotherapy.documented, physiotherapy.legible, physiotherapy.overall, physiotherapy.timely)

#Nutrition
nutrition.documented <- documented.consultant %>% filter(DEPT == "Nutrition")
nutrition.timely <- timely.consultant %>% filter(DEPT == "Nutrition")
nutrition.legible <- legible.consultant %>% filter(DEPT == "Nutrition")
nutrition.complete <- complete.consultant %>% filter(DEPT == "Nutrition")
nutrition.overall <- met.consultant %>% filter(DEPT == "Nutrition")

nutrition.report <- createWorkbook()
addWorksheet(nutrition.report, "Documented")
addWorksheet(nutrition.report, "Timely")
addWorksheet(nutrition.report, "Legible")
addWorksheet(nutrition.report, "Complete")
addWorksheet(nutrition.report, "Overall")

writeData(nutrition.report, "Documented", nutrition.documented, startRow = 1, startCol = 1)
writeData(nutrition.report, "Timely", nutrition.timely, startRow = 1, startCol = 1)
writeData(nutrition.report, "Legible", nutrition.legible, startRow = 1, startCol = 1)
writeData(nutrition.report, "Complete", nutrition.complete, startRow = 1, startCol = 1)
writeData(nutrition.report, "Overall", nutrition.overall, startRow = 1, startCol = 1)

saveWorkbook(nutrition.report, file = "Nutrition Report.xlsx", overwrite = TRUE)

rm(nutrition.report, nutrition.complete, nutrition.documented, nutrition.legible, nutrition.overall, nutrition.timely)

rm(list = ls())

####Special reports
###A&E Reports

emergency <- read.csv("D:/Work/QPS/Closed Charts/Monthly working files/Closed Charts Software Outputs/2021/Q1/A&E Closed Charts - Q1 2021.csv")
emergency <- emergency %>% select(MR.Number, Month, RMO, NURSE, DEPT:COMPLETE)
emergency <- emergency %>% filter(DOCUMENTED %in% c("D", "ND"))
emergency <- emergency %>% mutate(MET = case_when(DOCUMENTED %in% "D" & TIMELY %in% "Y" & LEGIBLE %in% "Y" & COMPLETE %in% "Y" ~ "M", TRUE ~ "NM"))
emergency$Month <- substr(emergency$Month, 1,3)

emergency <- emergency %>% rename(AKNO = MR.Number)
emergency <- emergency %>% mutate(DEPT = "A&E")

emergency <- emergency %>% mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

emergency$RMO <- str_to_title(emergency$RMO)

# emergency$RMO[emergency$RMO == "Iptisam"] <- "Iptisam Khalfan"
# emergency$RMO[emergency$RMO == "Frank"] <- "Frank Swai"
# emergency$RMO[emergency$RMO == "Richard"] <- "Richard Mashauri"
# emergency$RMO[emergency$RMO == "Martin"] <- "Martin Ngonyani"
# emergency$RMO[emergency$RMO == "Benjamin"] <- "Benjamin Rulakuze"
# emergency$RMO[emergency$RMO == "Moiz Adamji"] <- "Moiz Adamjee"

emergency$RMO[emergency$RMO == "Benjamin Rulakuze"] <- "Richard Mashauri"

emergency$NURSE <- str_to_title(emergency$NURSE)

# emergency$NURSE[emergency$NURSE == "Michael"] <- "Michael Mahilu"
# emergency$NURSE[emergency$NURSE == "Mahilu"] <- "Michael Mahilu"
# emergency$NURSE[emergency$NURSE == "Isaack Kikoti"] <- "Isaack"
# emergency$NURSE[emergency$NURSE == "Isack"] <- "Isaack"


##RMOs report
#Documented
documented.rmo <- emergency %>% filter((RMO != "") & (TYPE == "P")) %>% filter(DOCUMENTED %in% c("D", "ND")) %>% 
  group_by(DEPT, Month, RMO, DOCUMENTED) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(DOCUMENTED == "D") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-DOCUMENTED) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

documented.rmo <- documented.rmo %>%
  mutate(Q1 = apply(documented.rmo[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(documented.rmo[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(documented.rmo[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(documented.rmo[,c(12,13,14)], 1, mean, na.rm = T))


documented.rmo <- documented.rmo %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
documented.rmo <- documented.rmo %>% mutate(Annual = apply(documented.rmo[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Timely
timely.rmo <- emergency %>% filter((RMO != "") & (TYPE == "P")) %>% filter(TIMELY %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, RMO, TIMELY) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(TIMELY == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-TIMELY) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

timely.rmo <- timely.rmo %>%
  mutate(Q1 = apply(timely.rmo[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(timely.rmo[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(timely.rmo[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(timely.rmo[,c(12,13,14)], 1, mean, na.rm = T))

timely.rmo <- timely.rmo %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
timely.rmo <- timely.rmo %>% mutate(Annual = apply(timely.rmo[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Legible
legible.rmo <- emergency %>% filter((RMO != "") & (TYPE == "P")) %>% filter(LEGIBLE %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, RMO, LEGIBLE) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(LEGIBLE == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-LEGIBLE) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

legible.rmo <- legible.rmo %>%
  mutate(Q1 = apply(legible.rmo[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(legible.rmo[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(legible.rmo[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(legible.rmo[,c(12,13,14)], 1, mean, na.rm = T))

legible.rmo <- legible.rmo %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
legible.rmo <- legible.rmo %>% mutate(Annual = apply(legible.rmo[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Complete
complete.rmo <- emergency %>% filter((RMO != "") & (TYPE == "P")) %>% filter(COMPLETE %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, RMO, COMPLETE) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(COMPLETE == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-COMPLETE) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

complete.rmo <- complete.rmo %>%
  mutate(Q1 = apply(complete.rmo[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(complete.rmo[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(complete.rmo[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(complete.rmo[,c(12,13,14)], 1, mean, na.rm = T))

complete.rmo <- complete.rmo %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
complete.rmo <- complete.rmo %>% mutate(Annual = apply(complete.rmo[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Overall
met.rmo <- emergency %>% filter((RMO != "") & (TYPE == "P")) %>% filter(MET %in% c("M", "NM")) %>% 
  group_by(DEPT, Month, RMO, MET) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(MET == "M") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-MET) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

met.rmo <- met.rmo %>%
  mutate(Q1 = apply(met.rmo[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(met.rmo[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(met.rmo[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(met.rmo[,c(12,13,14)], 1, mean, na.rm = T))

met.rmo <- met.rmo %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
met.rmo <- met.rmo %>% mutate(Annual = apply(met.rmo[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)

##Nurses report
documented.nurse <- emergency %>% filter((NURSE != "") & (TYPE == "N")) %>% filter(DOCUMENTED %in% c("D", "ND")) %>% 
  group_by(DEPT, Month, NURSE, DOCUMENTED) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(DOCUMENTED == "D") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-DOCUMENTED) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

documented.nurse <- documented.nurse %>%
  mutate(Q1 = apply(documented.nurse[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(documented.nurse[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(documented.nurse[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(documented.nurse[,c(12,13,14)], 1, mean, na.rm = T))


documented.nurse <- documented.nurse %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
documented.nurse <- documented.nurse %>% mutate(Annual = apply(documented.nurse[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Timely
timely.nurse <- emergency %>% filter((NURSE != "") & (TYPE == "N")) %>% filter(TIMELY %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, NURSE, TIMELY) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(TIMELY == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-TIMELY) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

timely.nurse <- timely.nurse %>%
  mutate(Q1 = apply(timely.nurse[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(timely.nurse[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(timely.nurse[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(timely.nurse[,c(12,13,14)], 1, mean, na.rm = T))


timely.nurse <- timely.nurse %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
timely.nurse <- timely.nurse %>% mutate(Annual = apply(timely.nurse[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Legible
legible.nurse <- emergency %>% filter((NURSE != "") & (TYPE == "N")) %>% filter(LEGIBLE %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, NURSE, LEGIBLE) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(LEGIBLE == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-LEGIBLE) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

legible.nurse <- legible.nurse %>%
  mutate(Q1 = apply(legible.nurse[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(legible.nurse[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(legible.nurse[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(legible.nurse[,c(12,13,14)], 1, mean, na.rm = T))

legible.nurse <- legible.nurse %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
legible.nurse <- legible.nurse %>% mutate(Annual = apply(legible.nurse[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Complete
complete.nurse <- emergency %>% filter((NURSE != "") & (TYPE == "N")) %>% filter(COMPLETE %in% c("Y", "N")) %>% 
  group_by(DEPT, Month, NURSE, COMPLETE) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(COMPLETE == "Y") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-COMPLETE) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

complete.nurse <- complete.nurse %>%
  mutate(Q1 = apply(complete.nurse[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(complete.nurse[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(complete.nurse[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(complete.nurse[,c(12,13,14)], 1, mean, na.rm = T))


complete.nurse <- complete.nurse %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
complete.nurse <- complete.nurse %>% mutate(Annual = apply(complete.nurse[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


#Overall
met.nurse <- emergency %>% filter((NURSE != "") & (TYPE == "N")) %>% filter(MET %in% c("M", "NM")) %>% 
  group_by(DEPT, Month, NURSE, MET) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(MET == "M") %>% select(-Count) %>%
  pivot_wider(names_from = "Month", values_from = "Percentage") %>% select(-MET) %>% mutate_if(is.numeric, round, 1) %>% ungroup()

met.nurse <- met.nurse %>%
  mutate(Q1 = apply(met.nurse[,c(3,4,5)], 1, mean, na.rm = T)) %>%
  mutate(Q2 = apply(met.nurse[,c(6,7,8)], 1, mean, na.rm = T)) %>% 
  mutate(Q3 = apply(met.nurse[,c(9,10,11)], 1, mean, na.rm = T)) %>% 
  mutate(Q4 = apply(met.nurse[,c(12,13,14)], 1, mean, na.rm = T))

met.nurse <- met.nurse %>% mutate_all(~replace(.,is.nan(.), NA)) %>% mutate_if(is.numeric, round, 1)
met.nurse <- met.nurse %>% mutate(Annual = apply(met.nurse[,c(15:18)], 1, mean, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 1)


##Exporting
#A&E - RMOs & Nurses
dae.report <- createWorkbook()
addWorksheet(dae.report, "DocumentedRMO")
addWorksheet(dae.report, "TimelyRMO")
addWorksheet(dae.report, "LegibleRMO")
addWorksheet(dae.report, "CompleteRMO")
addWorksheet(dae.report, "OverallRMO")
addWorksheet(dae.report, "DocumentedNURSE")
addWorksheet(dae.report, "TimelyNURSE")
addWorksheet(dae.report, "LegibleNURSE")
addWorksheet(dae.report, "CompleteNURSE")
addWorksheet(dae.report, "OverallNURSE")

writeData(dae.report, "DocumentedRMO", documented.rmo, startRow = 1, startCol = 1)
writeData(dae.report, "TimelyRMO", timely.rmo, startRow = 1, startCol = 1)
writeData(dae.report, "LegibleRMO", legible.rmo, startRow = 1, startCol = 1)
writeData(dae.report, "CompleteRMO", complete.rmo, startRow = 1, startCol = 1)
writeData(dae.report, "OverallRMO", met.rmo, startRow = 1, startCol = 1)
writeData(dae.report, "DocumentedNURSE", documented.nurse, startRow = 1, startCol = 1)
writeData(dae.report, "TimelyNURSE", timely.nurse, startRow = 1, startCol = 1)
writeData(dae.report, "LegibleNURSE", legible.nurse, startRow = 1, startCol = 1)
writeData(dae.report, "CompleteNURSE", complete.nurse, startRow = 1, startCol = 1)
writeData(dae.report, "OverallNURSE", met.nurse, startRow = 1, startCol = 1)

saveWorkbook(dae.report, file = "A&E Report - Nurses & RMOs.xlsx", overwrite = TRUE)

rm(emergency, dae.report, complete.rmo,complete.nurse, documented.rmo, documented.nurse, legible.rmo,legible.nurse,
   met.rmo, met.nurse, timely.rmo, timely.nurse)



###OME Forms
OME <- data1 %>% filter(TYPE == "P") %>% mutate(Half = case_when(Month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep") ~ "First",
                                         Month %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ~ "Second"))

OME %>% filter(DOCTOR != "") %>%
  group_by(DEPT, DOCTOR, Half, MET) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(MET == "M") %>% select(-Count) %>%
  pivot_wider(names_from = "Half", values_from = "Percentage") %>% select(DEPT, DOCTOR, MET, First, Second) %>% 
  mutate(Average = mean(c(First, Second), na.rm = TRUE))

##Alternative - group & ungroup - Final
OME %>% filter(DOCTOR != "") %>%
  group_by(DEPT, DOCTOR, Month, MET) %>% summarise(Count = n()) %>% mutate(Percentage = (100*Count)/sum(Count)) %>%
  filter(MET == "M") %>% select(-Count) %>%
  mutate(Half = case_when(Month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep") ~ "First",
                          Month %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ~ "Second")) %>%
  group_by(DEPT, DOCTOR, Half) %>% summarise(Percentage = mean(Percentage)) %>%
  pivot_wider(names_from = "Half", values_from = "Percentage") %>% select(DEPT, DOCTOR, First, Second) %>% 
  mutate(Average = mean(c(First, Second), na.rm = TRUE))
