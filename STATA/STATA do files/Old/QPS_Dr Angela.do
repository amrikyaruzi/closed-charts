clear
set more off
set mem 1g

import excel "D:/Work/QPS/Closed Charts/Monthly working files/Closed Charts Software Outputs/2020/OME DETAILS JULY - SEPTEMBER 2020.xlsx", sheet("SHEET1") firstrow clear
//import excel "D:\IMPACT Mwanza\Dr Angella QPS\OME DETAILS FEBRUARY 2018.xlsx", sheet("SHEET1") firstrow clear
//import excel "D:\IMPACT Mwanza\Dr Angella QPS\Emmanuel\OME DETAILS April 2018.xlsx", sheet("SHEET1") firstrow clear
//import excel "D:\IMPACT Mwanza\Dr Angella QPS\Emmanuel\OME DETAILS March 2018.xlsx", sheet("SHEET1") firstrow clear
//import excel "D:\IMPACT Mwanza\Dr Angella QPS\Emmanuel\OME DETAILS February 2018.xlsx", sheet("SHEET1") firstrow clear
//import excel "D:\IMPACT Mwanza\Dr Angella QPS\Emmanuel\OME DETAILS February 2018.XLSX", sheet("January 2018") firstrow clear
//import excel "D:\IMPACT Mwanza\Dr Angella QPS\OME DETAILS sep 17.xlsx", sheet("SHEET1") firstrow clear

//import excel "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\OME DETAILS JULY 2018.xlsx", sheet("Sheet1") firstrow clear
//save "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\July2018.dta", replace
//import excel "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\OME DETAILS JULY 2018.xlsx", sheet("SHEET1") firstrow clear
//save "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\August2018.dta", replace

//import excel "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\SEPTEMBER 2018.xlsx", sheet("SHEET1") firstrow clear
//save "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\September2018.dta", replace

//append using "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\January2018.dta", force

//append using "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\February2018.dta", force
//save "D:\IMPACT Mwanza\Dr Angella QPS\Combine Datasets\Jan_Feb_Mar_2018.dta", replace

destring, replace

tab DOCUMENTED
generate DOCUMENTED1=.
replace DOCUMENTED1 =1 if DOCUMENTED=="D"
replace DOCUMENTED1 =0 if DOCUMENTED=="ND"
replace DOCUMENTED1 =2 if DOCUMENTED=="NA"
label define documented 0 "ND" 1 "D" 2 "NA" 
label values DOCUMENTED1 documented
drop DOCUMENTED
rename DOCUMENTED1 DOCUMENTED
drop if DOCUMENTED==2   // DROP IF NOT APPLICABLE
tab DOCUMENTED
********************************************************************************
tab TYPE
generate TYPE1=.
replace TYPE1 =1 if TYPE=="P"
replace TYPE1 =0 if TYPE=="N"
label define type1 1 "Physcian" 0 "Nurse"
label values TYPE1 type1

drop TYPE
rename TYPE1 TYPE
//drop if TYPE==0          // DROPPING FOR NURSES

*******************************************************************************

tab DEPT
generate DEPT1=.
replace DEPT1 =1 if DEPT=="DOG"
replace DEPT1 =2 if DEPT=="DOM"
replace DEPT1 =3 if DEPT=="DOP"
replace DEPT1 =4 if DEPT=="DOS"
replace DEPT1 =5 if DEPT=="DOA"
replace DEPT1 =6 if DEPT=="DAE"

label define dept 1 "DOG" 2 "DOM" 3 "DOP" 4 "DOS" 5 "DOA" 6 "DAE"
label values DEPT1 dept
drop DEPT
rename DEPT1 DEPT
tab DEPT

*******************************************************************************

tab TIMELY
generate TIMELY1=.
replace TIMELY1 =1 if TIMELY=="Y"
replace TIMELY1 =0 if TIMELY=="N"
label define timely 0 "No" 1 "Yes"
label values TIMELY1 timely
drop TIMELY
rename TIMELY1 TIMELY

********************************************************************************
tab LEGIBLE
generate LEGIBLE1=.
replace LEGIBLE1 =1 if LEGIBLE=="Y"
replace LEGIBLE1 =0 if LEGIBLE=="N"
label define legible 0 "No" 1 "Yes"
label values LEGIBLE1 legible
drop LEGIBLE
rename LEGIBLE1 LEGIBLE

********************************************************************************

tab COMPLETE
generate COMPLETE1=.
replace COMPLETE1 =1 if COMPLETE=="Y"
replace COMPLETE1 =0 if COMPLETE=="N"
label define complete 0 "No" 1 "Yes"
label values COMPLETE1 complete
drop COMPLETE
rename COMPLETE1 COMPLETE

********************************************************************************
tab MET
generate MET1=.
replace MET1 =1 if MET=="M" 
replace MET1 =0 if MET=="NM"
label define met 0 "Not met" 1 "Met"
label values MET1 met
drop MET
rename MET1 MET

*******************************************************************************
generate double DOCUMENTS1=.
replace DOCUMENTS1=1 if SNN==1.1
replace DOCUMENTS1=2 if SNN==1.3
replace DOCUMENTS1=3 if SNN==1.4
replace DOCUMENTS1=4 if SNN==2.1
replace DOCUMENTS1=5 if SNN==3.2
replace DOCUMENTS1=6 if SNN==4.1
replace DOCUMENTS1=7 if SNN==5.1
replace DOCUMENTS1=8 if SNN==5.2
replace DOCUMENTS1=9 if SNN==5.4
replace DOCUMENTS1=10 if SNN==7.1
replace DOCUMENTS1=11 if SNN==7.2
replace DOCUMENTS1=12 if SNN==7.3
replace DOCUMENTS1=13 if SNN==8.1

label define documents  1 "Physician Initial Assessment" 2 "Pre-Anesthesia/Sedation Assessment" 3 "All forms with approved abbreviations" 4 "Daily Case Notes by Physicians" 5 "Intradiciplinary rounds of ICU" 6 "Daily CPOE orders and Nursing signature for administration" 7 "Pre-Induction Assessment" 8 "Informed Consent Form (all procedures requiring consent i.e. anesthesia, sedation, BT, chemo, dialys" 9 "Consent forms without any abbreviations" 10 "Site marking and time out form" 11 "Intraoperative Notes" 12 "Surgical/operative notes" 13 "Discharge/Transfer/Referral Summary at exit"
label values DOCUMENTS1 documents
drop DOCUMENTS
rename DOCUMENTS1 DOCUMENTS
tab DOCUMENTS


********************************************************************************
tab SN
label define sn 1 "Initial Assessment" 2 "Re-assessment" 3 "Patient Education"  4 "Medication"  5 "Consent" 6 "Blood & Blood Products"  7 "Procedures" 8 "Discharge, Referral & Follow-up of patient"
label values SN sn





********************************************************************************
log using "E:\IMPACT Mwanza\Dr Angella QPS\August_2018.log", replace
********************************************************************************

********SUMMARIES FOR PHYSCIAN********

tab DOCUMENTED if TYPE==1
tab TYPE  if TYPE==1
tab DEPT if TYPE==1
tab TIMELY if TYPE==1
tab LEGIBLE if TYPE==1
tab COMPLETE if TYPE==1
tab MET if TYPE==1
tab DOCUMENTS if TYPE==1

*********************************
tab DOCUMENTED TIMELY if TYPE==1
tab DOCUMENTED LEGIBLE if TYPE==1
tab DOCUMENTED COMPLETE if TYPE==1
tab DOCUMENTED MET if TYPE==1
tab DOCUMENTS DOCUMENTED if TYPE==1 
tab DOCUMENTS MET if TYPE==1, row


********************************
//Summary for Department Versus the Criteria
tab DEPT DOCUMENTED if TYPE==1, row
tab DEPT TIMELY if TYPE==1, row
tab DEPT LEGIBLE if TYPE==1, row
tab DEPT COMPLETE if TYPE==1, row
tab DEPT MET if TYPE==1, row
tab SN if TYPE==1
tab SN MET if TYPE==1, row

********************************
bysort DEPT:tab DOCTOR DOCUMENTED if TYPE==1
bysort DEPT:tab DOCTOR TIMELY if TYPE==1
bysort DEPT:tab DOCTOR LEGIBLE if TYPE==1
bysort DEPT:tab DOCTOR COMPLETE if TYPE==1
bysort DEPT:tab DOCTOR MET if TYPE==1



********************************************************************************
// Row percent
bysort DEPT:tab DOCTOR TIMELY if TYPE==1, row
bysort DEPT:tab DOCTOR LEGIBLE if TYPE==1, row
bysort DEPT:tab DOCTOR COMPLETE if TYPE==1, row
bysort DEPT:tab DOCTOR MET if TYPE==1, row
bysort DEPT:tab SN MET  if TYPE==1, row

********************************************************************************
//Column Percent
bysort DEPT:tab DOCTOR TIMELY if TYPE==1, col
bysort DEPT:tab DOCTOR LEGIBLE if TYPE==1, col
bysort DEPT:tab DOCTOR COMPLETE if TYPE==1, col
bysort DEPT:tab DOCTOR MET if TYPE==1, col
bysort DEPT:tab SN MET if TYPE==1, col


//Average by Doctor and Department addressing all the 3 elements
gen TIMELY_LEGIBLE_COMPLETENESS= ((TIMELY+LEGIBLE+COMPLETE)/3*100)
sort DOCTOR
by DOCTOR: tabstat TIMELY_LEGIBLE_COMPLETENESS if TYPE==1
by DEPT: tabstat TIMELY_LEGIBLE_COMPLETENESS if TYPE==1


****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************

***********SUMMARIES FOR NURSING***


tab DOCUMENTED if TYPE==0
tab TYPE  if TYPE==0
tab DEPT if TYPE==0
tab TIMELY if TYPE==0
tab LEGIBLE if TYPE==0
tab COMPLETE if TYPE==0
tab MET if TYPE==0
tab DOCUMENTS if TYPE==0

*********************************
tab DOCUMENTED TIMELY if TYPE==0
tab DOCUMENTED LEGIBLE if TYPE==0
tab DOCUMENTED COMPLETE if TYPE==0
tab DOCUMENTED MET if TYPE==0
tab DOCUMENTS DOCUMENTED  if TYPE==0
tab DOCUMENTS MET if TYPE==0, row




********************************
//Summary for Department Versus the Criteria
tab DEPT TIMELY if TYPE==0, row
tab DEPT LEGIBLE if TYPE==0, row
tab DEPT COMPLETE if TYPE==0, row
tab DEPT MET if TYPE==0, row
tab SN if TYPE==0
tab SN MET if TYPE==0, row



tab DEPT TIMELY if TYPE==0, col
tab DEPT LEGIBLE if TYPE==0, col
tab DEPT COMPLETE if TYPE==0, col
tab DEPT MET if TYPE==0, col
tab SN if TYPE==0
tab SN MET if TYPE==0, col


********************************
bysort DEPT:tab DOCTOR TIMELY if TYPE==0
bysort DEPT:tab DOCTOR LEGIBLE if TYPE==0
bysort DEPT:tab DOCTOR COMPLETE if TYPE==0
bysort DEPT:tab DOCTOR MET if TYPE==0



********************************************************************************
// Row percent
bysort DEPT:tab DOCTOR TIMELY if TYPE==0, row
bysort DEPT:tab DOCTOR LEGIBLE if TYPE==0, row
bysort DEPT:tab DOCTOR COMPLETE if TYPE==0, row
bysort DEPT:tab DOCTOR MET if TYPE==0, row
bysort DEPT:tab SN MET if TYPE==0, row

********************************************************************************
//Column Percent
bysort DEPT:tab DOCTOR TIMELY if TYPE==0, col
bysort DEPT:tab DOCTOR LEGIBLE if TYPE==0, col
bysort DEPT:tab DOCTOR COMPLETE if TYPE==0, col
bysort DEPT:tab DOCTOR MET if TYPE==0, col
bysort DEPT:tab SN MET if TYPE==0 , col


log close

*******************************************************************************************************************************************************
*******************************************************************************************************************************************************
*******************************************************************************************************************************************************
//Graphs
/*
#delimit ;
graph bar (mean) wage,
over(union) over(married) over(collgrad)
blabel(bar, format(%9.2f)) yscale(off)
title("1988 Mean Hourly Wage of Women Age 40-44")
subtitle("by union status, marital status, and college graduation")
note("Source: Stata 12 NLSW 1988 extract", span);
#delimit cr



