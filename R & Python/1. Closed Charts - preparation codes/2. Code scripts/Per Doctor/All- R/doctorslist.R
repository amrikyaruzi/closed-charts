dt %>% group_by(DEPT) %>% count() %>% arrange(desc(n))
doctorslist <- dt %>% filter(DEPT == "DAE") %>% group_by(DOCTOR) %>% count() %>% arrange(desc(n))
View(doctorslist)

DOC_CODE AKH-026 for Dr Kaguta, should remember to rename his name in OME software!

#Doctor's scores for the Department of Medicine:
PercentDocumentedDOM
PercentTimelyDOM
PercentLegibleDOM
PercentCompleteDOM
PercentMetDOM
