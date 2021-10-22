# -*- coding: utf-8 -*-
"""
Created on Wed Apr  8 10:14:17 2020

@author: amri.kyaruzi
"""


#DON Departmental data:
import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\Q2 2020\Q2 2020.csv", engine='python')
print(data.head())
doctorslist = data[(data['DEPT'] == 'DON')].groupby('DOCTOR')['DOCTOR'].value_counts()

#DONFilters
is_DON = data['DEPT'] == 'DON'
is_SNN = (data['SNN'] == 1.5) | (data['SNN'] == 2.1) | (data['SNN'] == 3.1) | (data['SNN'] == 3.2)
is_DOCTOR = data['DOCTOR'] == 'Kassim'

#DONdataset
mydata = data[is_DON & is_SNN & is_DOCTOR]

#Filters
is_documented = (mydata['DOCUMENTED'] == 'D') | (mydata['DOCUMENTED'] == 'ND')
is_timely = (mydata['TIMELY'] == 'Y') | (mydata['TIMELY'] == 'N')
is_legible = (mydata['LEGIBLE'] == 'Y') | (mydata['LEGIBLE'] == 'N')
is_complete = (mydata['COMPLETE'] == 'Y') | (mydata['COMPLETE'] == 'N')
is_met = (mydata['MET'] == 'M') | (mydata['MET'] == 'NM')

#Filtered Dataframes
documentedDON = mydata[is_documented]
timelyDON = mydata[is_timely]
legibleDON = mydata[is_legible]
completeDON = mydata[is_complete]
metDON = mydata[is_met]

#Scores vs Documents
documented = documentedDON.groupby('DOCUMENTS')['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Documented_Scores = documented.to_frame()

timely = timelyDON.groupby('DOCUMENTS')['TIMELY'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Timeliness_Scores = timely.to_frame()

legible = legibleDON.groupby('DOCUMENTS')['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Legibility_Scores = legible.to_frame()

complete = completeDON.groupby('DOCUMENTS')['COMPLETE'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Completeness_Scores = complete.to_frame()

met = metDON.groupby('DOCUMENTS')['MET'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Met_Scores = met.to_frame()

#Results
#Closed Charts Data for the Nutrition Department:
print('DOCUMENTED SCORES DON')
print(documentedDON['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('TIMELINESS SCORES DON')
print(timelyDON['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('LEGIBILITY SCORES DON')
print(legibleDON['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('COMPLETENESS SCORES DON')
print(completeDON['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('OVERALL COMPLIANCE SCORES DON')
print(metDON['MET'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')

print('Summary per Parameter')

print(Documented_Scores)
print('')

print(Timeliness_Scores)
print('')

print(Legibility_Scores)
print('')

print(Completeness_Scores)
print('')

print(Met_Scores)
print('')
