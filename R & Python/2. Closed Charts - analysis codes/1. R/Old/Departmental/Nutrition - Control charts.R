setwd("C:/Users/amri.kyaruzi/Documents/Graphs/Departmental")

##Nutrition
#Data
controlchart <- data1 %>% filter(DEPT == "Nutrition")

documented <- controlchart %>% select(Month, DEPT, TYPE, DOCUMENTED) %>% group_by(Month, DEPT, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>% summarize(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>% filter(DOCUMENTED == "D")
documented <- documented %>% mutate(Mean = mean(documented$Percentage)) %>% mutate(UCL = Mean + 2*(sd(documented$Percentage)), LCL = Mean - 2*(sd(documented$Percentage)), Benchmark = 100) %>% mutate_if(is.numeric, round, 1)

timely <- controlchart %>% select(Month, DEPT, TYPE, TIMELY) %>% group_by(Month, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>% filter(TIMELY == "Y")
timely <- timely %>% mutate(Mean = mean(timely$Percentage), UCL = Mean + 2*(sd(timely$Percentage)), LCL = Mean - 2*(sd(timely$Percentage)), Benchmark = 90) %>% mutate_if(is.numeric, round, 1)

legible <- controlchart %>% select(Month, DEPT, TYPE, LEGIBLE) %>% group_by(Month, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>% filter(LEGIBLE == "Y")
legible <- legible %>% mutate(Mean = mean(legible$Percentage), UCL = Mean + 2*(sd(legible$Percentage)), LCL = Mean - 2*(sd(legible$Percentage)), Benchmark = 90) %>% mutate_if(is.numeric, round, 1)

complete <- controlchart %>% select(Month, DEPT, TYPE, COMPLETE) %>% group_by(Month, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>% filter(COMPLETE == "Y")
complete <- complete %>% mutate(Mean = mean(complete$Percentage), UCL = Mean + 2*(sd(complete$Percentage)), LCL = Mean - 2*(sd(complete$Percentage)), Benchmark = 90) %>% mutate_if(is.numeric, round, 1)

met <- controlchart %>% select(Month, DEPT, TYPE, MET) %>% group_by(Month, MET) %>% filter(MET %in% c("M", "NM")) %>%  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>% filter(MET == "M")
met <- met %>% mutate(Mean = mean(met$Percentage), UCL = Mean + 2*(sd(met$Percentage)), LCL = Mean - 2*(sd(met$Percentage)), Benchmark = 90)  %>% mutate_if(is.numeric, round, 1)


#Graphs
documented_plot <- ggplot(data = documented, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,3)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("NUTRITION - AVAILABILITY OF REQUIRED DOCUMENTS")
timely_plot <- ggplot(data = timely, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,3)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("NUTRITION - TIMELINESS")
legible_plot <- ggplot(data = legible, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,5)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("NUTRITION - LEGIBILITY")
complete_plot <- ggplot(data = complete, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,4)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("NUTRITION - COMPLETENESS")
overall_plot <- ggplot(data = met, aes(x = Month, y = Percentage, group = 1)) + geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() + geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) + geom_hline(aes(yintercept = Mean, colour = "Mean")) + geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,5)) + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") + xlab("Month") + ylab("Compliance (%)") + ggtitle("NUTRITION - OVERALL")

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
  print('Nutrition - plots.pptx')

rm(documented, timely, legible, complete, met, documented_plot, timely_plot, legible_plot,
   complete_plot, overall_plot)



#Data for tables
tables <- data1
tables <- tables %>% filter(DEPT == "Nutrition")

##Documented
documented_scores <- tables %>% group_by(Month, DEPT, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
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
timely_scores <- tables %>% group_by(Month, DEPT, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
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
legible_scores <- tables %>% group_by(Month, DEPT, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
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
complete_scores <- tables %>% group_by(Month, DEPT, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
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
overall_scores <- tables %>% group_by(Month, DEPT, MET) %>% filter(MET %in% c("M", "NM")) %>%
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


##Flextables and exporting them
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
  print("Nutrition - scores.pptx")

rm(documented_scores, timely_scores, legible_scores, complete_scores, overall_scores)

##Summaries for each parameter per document
documented <- controlchart %>% select(Month, DOCUMENTS, TYPE, DOCUMENTED) %>%
  group_by(DOCUMENTS, TYPE, DOCUMENTED) %>% filter(DOCUMENTED %in% c("D", "ND")) %>%
  summarize(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(DOCUMENTED == "D") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 100) %>%
  arrange(Percentage)

timely <- controlchart %>% select(Month, DOCUMENTS, TYPE, TIMELY) %>%
  group_by(DOCUMENTS, TYPE, TIMELY) %>% filter(TIMELY %in% c("Y", "N")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(TIMELY == "Y") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)

legible <- controlchart %>% select(Month, DOCUMENTS, TYPE, LEGIBLE) %>%
  group_by(DOCUMENTS, TYPE, LEGIBLE) %>% filter(LEGIBLE %in% c("Y", "N")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(LEGIBLE == "Y") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)

complete <- controlchart %>% select(Month, DOCUMENTS, TYPE, COMPLETE) %>%
  group_by(DOCUMENTS, TYPE, COMPLETE) %>% filter(COMPLETE %in% c("Y", "N")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(COMPLETE == "Y") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)

met <- controlchart %>% select(Month, DOCUMENTS, TYPE, MET) %>%
  group_by(DOCUMENTS, TYPE, MET) %>% filter(MET %in% c("M", "NM")) %>%
  summarise(Frequency = n()) %>% mutate(Percentage = (100*Frequency)/sum(Frequency)) %>%
  filter(MET == "M") %>% mutate_if(is.numeric, round, 1) %>% filter(Percentage < 90) %>%
  arrange(Percentage)

#Regular table, formatted
read_docx() %>%
  body_add_par("NUTRITION DEPARTMENT", style = "centered") %>%
  
  #Availability of Required Documents
  body_add_par("Documented", style = "heading 1") %>%
  body_add_par("The set benchmark for Availability of Required documents is a score of 100 percent. The table below shows the documents that had issues in that area:") %>%
  body_add_par(" ") %>% body_add_par(" ") %>%
  body_add_table(documented[,-c(3, 4)], style = "table_template") %>% body_add_par(" ") %>% body_add_par(" ") %>%
  
  #Timeliness
  body_add_par("Timeliness", style = "heading 1") %>%
  body_add_par("The set benchmark for Timeliness is a score of at least 90 percent. The table below shows the documents where you were least compliant in Timeliness:") %>%
  body_add_par(" ") %>% body_add_par(" ") %>%
  body_add_table(timely[,-c(3, 4)], style = "table_template") %>% body_add_par(" ") %>% body_add_par(" ") %>%
  
  #Legibility
  body_add_par("Legibility", style = "heading 1") %>%
  body_add_par("The set benchmark for Legibility is a score of at least 90 percent. The table below shows the documents where you were least compliant in Legibility:") %>%
  body_add_par(" ") %>% body_add_par(" ") %>%
  body_add_table(legible[,-c(3, 4)], style = "table_template") %>% body_add_par(" ") %>% body_add_par(" ") %>%
  
  #Completeness
  body_add_par("Completeness", style = "heading 1") %>%
  body_add_par("The set benchmark for Completeness is a score of at least 90 percent. The table below shows the documents where you were least compliant in Completeness:") %>%
  body_add_par(" ") %>% body_add_par(" ") %>%
  body_add_table(complete[,-c(3, 4)], style = "table_template") %>% body_add_par(" ") %>% body_add_par(" ") %>%
  
  #Overall
  body_add_par("Overall", style = "heading 1") %>%
  body_add_par("The 'overall score' combines all of the four aforementioned scores (i.e. Availability of Required documents, Timeliness, Legibility and Completeness scores). The set benchmark for the Overall score is a score of at least 90 percent. The table below shows the documents where you were least compliant Overall:") %>%
  body_add_par(" ") %>% body_add_par(" ") %>%
  body_add_table(met[,-c(3, 4)], style = "table_template") %>% body_add_par(" ") %>% body_add_par(" ") %>%
  print("Nutrition.docx")

setwd("C:/Users/amri.kyaruzi/Documents")