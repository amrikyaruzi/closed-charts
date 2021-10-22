# -*- coding: utf-8 -*-
"""
Created on Wed Apr  8 09:58:11 2020

@author: amri.kyaruzi
"""


#DOA Departmental data:
import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\Q2 2020\Q2 2020.csv", engine='python')
print(data.head())
doctorslist = data[(data['DEPT'] == 'DOA')].groupby('DOCTOR')['DOCTOR'].value_counts()

#DOAFilters
is_DOA = data['DEPT'] == 'DOA'
is_SNN = (data['SNN'] == 1.3) | (data['SNN'] == 4.1) | (data['SNN'] == 5.1) | (data['SNN'] == 5.2) | (data['SNN'] == 5.3) | (data['SNN'] == 5.5) | (data['SNN'] == 7.2)
is_TYPE = data['TYPE'] == 'P'
is_DOCTOR = data['DOCTOR'] == 'Tembo, Frederick'

#DOAdataset
mydata = data[is_DOA & is_SNN & is_TYPE & is_DOCTOR]

#Filters
is_documented = (mydata['DOCUMENTED'] == 'D') | (mydata['DOCUMENTED'] == 'ND')
is_timely = (mydata['TIMELY'] == 'Y') | (mydata['TIMELY'] == 'N')
is_legible = (mydata['LEGIBLE'] == 'Y') | (mydata['LEGIBLE'] == 'N')
is_complete = (mydata['COMPLETE'] == 'Y') | (mydata['COMPLETE'] == 'N')
is_met = (mydata['MET'] == 'M') | (mydata['MET'] == 'NM')

#Filtered Dataframes
documentedDOA = mydata[is_documented]
timelyDOA = mydata[is_timely]
legibleDOA = mydata[is_legible]
completeDOA = mydata[is_complete]
metDOA = mydata[is_met]

#Scores vs Documents
documented = documentedDOA.groupby('DOCUMENTS')['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Documented_Scores = documented.to_frame()

timely = timelyDOA.groupby('DOCUMENTS')['TIMELY'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Timeliness_Scores = timely.to_frame()

legible = legibleDOA.groupby('DOCUMENTS')['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Legibility_Scores = legible.to_frame()

complete = completeDOA.groupby('DOCUMENTS')['COMPLETE'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Completeness_Scores = complete.to_frame()

met = metDOA.groupby('DOCUMENTS')['MET'].value_counts(normalize=True).mul(100).round(2).sort_values(ascending=False).astype('str') + "%"
Met_Scores = met.to_frame()

#Results
#Closed Charts Data for the Anaesthesia Department:
print('DOCUMENTED SCORES DOA')
print(documentedDOA['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('TIMELINESS SCORES DOA')
print(timelyDOA['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('LEGIBILITY SCORES DOA')
print(legibleDOA['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('COMPLETENESS SCORES DOA')
print(completeDOA['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('OVERALL COMPLIANCE SCORES DOA')
print(metDOA['MET'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
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
