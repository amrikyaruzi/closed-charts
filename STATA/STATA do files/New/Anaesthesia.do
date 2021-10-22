///Closed Charts Analysis
///Department of Anaesthesia

clear
set more off
cd "D:\Work\QPS\Closed Charts\Programming\STATA\Closed Charts Analysis - Outputs"

capture log close
log using Anaesthesia.log, replace

import excel "D:/Work/QPS/Closed Charts/Monthly working files/Closed Charts Software Outputs/2020/All departments/Q3/OME DETAILS JULY - SEPTEMBER 2020_ORIGINAL.xlsx", firstrow clear
///import delimited "D:/Work/QPS/Closed Charts/Monthly working files/Closed Charts Software Outputs/2020/All departments/Q3/OME DETAILS JULY - SEPTEMBER 2020.csv", case(upper)

drop if MET == "NA"
keep if DEPT == "DOA"

destring SNN, replace
keep if (SNN == 1.3 | SNN == 4.1 | SNN == 4.2 | SNN == 5.1 | SNN == 5.2 | SNN == 5.3 | SNN == 5.5 | SNN == 7.2)

////Departmental Results
//Overall percentages
tab DOCUMENTED
tab TIMELY if DOCUMENTED == "D"
tab LEGIBLE if DOCUMENTED == "D"
tab COMPLETE if DOCUMENTED == "D"
tab MET

//Percentages per document for each Statistic
tab DOCUMENTS DOCUMENTED, row
tab DOCUMENTS TIMELY if DOCUMENTED == "D", row
tab DOCUMENTS LEGIBLE if DOCUMENTED == "D", row
tab DOCUMENTS COMPLETE if DOCUMENTED == "D", row
tab DOCUMENTS MET, row



////Group summaries
///Physicians
//Overall
tab DOCUMENTED if TYPE == "P"
tab TIMELY if TYPE == "P" & DOCUMENTED == "D"
tab LEGIBLE if TYPE == "P" & DOCUMENTED == "D"
tab COMPLETE if TYPE == "P" & DOCUMENTED == "D"
tab MET if TYPE == "P"

//Physician documents
tab DOCUMENTS DOCUMENTED if TYPE == "P", row
tab DOCUMENTS TIMELY if TYPE == "P" & DOCUMENTED == "D", row
tab DOCUMENTS LEGIBLE if TYPE == "P" & DOCUMENTED == "D", row
tab DOCUMENTS COMPLETE if TYPE == "P" & DOCUMENTED == "D", row
tab DOCUMENTS MET if TYPE == "P", row

//Per Physician
tab DOCTOR DOCUMENTED if TYPE == "P", row
tab DOCTOR TIMELY if TYPE == "P" & DOCUMENTED == "D", row
tab DOCTOR LEGIBLE if TYPE == "P" & DOCUMENTED == "D", row
tab DOCTOR COMPLETE if TYPE == "P" & DOCUMENTED == "D", row
tab DOCTOR MET if TYPE == "P", row

///Nurses
//Overall
tab DOCUMENTED if TYPE == "N"
tab TIMELY if TYPE == "N" & DOCUMENTED == "D"
tab LEGIBLE if TYPE == "N" & DOCUMENTED == "D"
tab COMPLETE if TYPE == "N" & DOCUMENTED == "D"
tab MET if TYPE == "N"

//Nursing documents
tab DOCUMENTS DOCUMENTED if TYPE == "N", row
tab DOCUMENTS TIMELY if TYPE == "N" & DOCUMENTED == "D", row
tab DOCUMENTS LEGIBLE if TYPE == "N" & DOCUMENTED == "D", row
tab DOCUMENTS COMPLETE if TYPE == "N" & DOCUMENTED == "D", row
tab DOCUMENTS MET if TYPE == "N", row
