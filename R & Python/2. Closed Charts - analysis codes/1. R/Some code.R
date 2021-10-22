library(tidyverse)


departments <- unique(data1$DEPT)

# Start of for loop

for (department in departments){
  

###Department name
# department <- "Medicine"


  ##Function

departmental_results <- function(department){
  
  controlchart <- data1 %>% filter(DEPT == department)
  
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
  

c(list(Documented = documented, Timely = timely, Legible = legible,
                  Complete = complete, Overall = met))
}

##End of function

results <- departmental_results(department)

documented <- results[[1]]
timely <- results[[2]]
legible <- results[[3]]
complete <- results[[4]]
met <- results[[5]]

rm(results)

#Graphs

documented_plot <- ggplot(data = documented, aes(x = Month, y = Percentage, group = 1)) +
  geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() +
  geom_hline(aes(yintercept = UCL, colour = "UCL")) +
  geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) +
  geom_hline(aes(yintercept = Mean, colour = "Mean")) +
  geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,0.5)) +
  theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") +
  xlab("Month") + ylab("Compliance (%)") + ggtitle(str_to_upper(glue("{department} - AVAILABILITY OF REQUIRED DOCUMENTS")))

timely_plot <- ggplot(data = timely, aes(x = Month, y = Percentage, group = 1)) +
  geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() +
  geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) +
  geom_hline(aes(yintercept = Mean, colour = "Mean")) +
  geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,3)) +
  theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") +
  xlab("Month") + ylab("Compliance (%)") + ggtitle(str_to_upper(glue("{department} - TIMELINESS")))

legible_plot <- ggplot(data = legible, aes(x = Month, y = Percentage, group = 1)) +
  geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() +
  geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) +
  geom_hline(aes(yintercept = Mean, colour = "Mean")) +
  geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,5)) +
  theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") +
  xlab("Month") + ylab("Compliance (%)") + ggtitle(str_to_upper(glue("{department} - LEGIBILITY")))

complete_plot <- ggplot(data = complete, aes(x = Month, y = Percentage, group = 1)) +
  geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() +
  geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) +
  geom_hline(aes(yintercept = Mean, colour = "Mean")) +
  geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,3)) +
  theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") +
  xlab("Month") + ylab("Compliance (%)") + ggtitle(str_to_upper(glue("{department} - COMPLETENESS")))

overall_plot <- ggplot(data = met, aes(x = Month, y = Percentage, group = 1)) +
  geom_point(shape = 21, color = "black", fill= "#69b3a2", size = 2) + geom_line() +
  geom_hline(aes(yintercept = UCL, colour = "UCL")) + geom_hline(aes(yintercept = Benchmark, colour = "Benchmark")) +
  geom_hline(aes(yintercept = Mean, colour = "Mean")) +
  geom_hline(aes(yintercept = LCL, colour = "LCL")) + scale_y_continuous(breaks = seq(20,120,5)) +
  theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_rect(fill = "gray98"), legend.background = element_rect(), legend.title = element_blank(), legend.position = "bottom") +
  xlab("Month") + ylab("Compliance (%)") + ggtitle(str_to_upper(glue("{department} - OVERALL")))


##Exporting the graphs to pptx

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
  
  print(glue('{department} - plots.pptx'))

rm(documented, timely, legible, complete, met, documented_plot, timely_plot, legible_plot,
   complete_plot, overall_plot)

}

# End of for loop

rm(list = setdiff(ls(), "data1"))