////Closed Charts Analysis
///Accidents & Emergency department

clear
set more off
///import excel "C:/Users/amri.kyaruzi/Desktop/A&E October 2020.xlsx", firstrow clear
import delimited "C:/Users/amri.kyaruzi/Desktop/A&E October 2020.csv", case(upper)

cd "D:/Work/QPS/Closed Charts/Programming/STATA/Closed Charts Analysis - Outputs"

capture log close
log using A&E.log, replace

drop if DOCUMENTED == "NA"

///gen MET = .
///replace MET = 1 if (DOCUMENTED == "D" & TIMELY == "Y" & LEGIBLE == "Y" & COMPLETE == "Y")
///replace MET = 2 if (DOCUMENTED == "ND" | TIMELY == "N" | LEGIBLE == "N" | COMPLETE == "N")
*replace MET = 2 if MET == . ///An alternative, adds an increment of 1, don't use!

///label define met 1 "MET" 2 "NOT MET"
///label val MET met

gen MET = "M" if (DOCUMENTED == "D" & TIMELY == "Y" & LEGIBLE == "Y" & COMPLETE == "Y")
replace MET = "NM" if (DOCUMENTED == "ND" | TIMELY == "N" | LEGIBLE == "N" | COMPLETE == "N")


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

///Per RMO
tab RMO DOCUMENTED if TYPE == "P", row
tab RMO TIMELY if TYPE == "P" & DOCUMENTED == "D", row
tab RMO LEGIBLE if TYPE == "P" & DOCUMENTED == "D", row
tab RMO COMPLETE if TYPE == "P" & DOCUMENTED == "D", row
tab RMO MET, row

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

//Per Nurse
tab NURSE DOCUMENTED if TYPE == "N", row
tab NURSE TIMELY if TYPE == "N" & DOCUMENTED == "D", row
tab NURSE LEGIBLE if TYPE == "N" & DOCUMENTED == "D", row
tab NURSE COMPLETE if TYPE == "N" & DOCUMENTED == "D", row
tab NURSE MET, row
