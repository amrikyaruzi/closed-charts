# -*- coding: utf-8 -*-
"""
Created on Wed Sep 23 10:26:57 2020

@author: amri.kyaruzi
"""


#DOG Departmental data:
import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\Q1-Q2.csv", engine='python')

print(data.head())

#DOGFilters
is_DOG = data['DEPT'] == 'DOG'
is_SNN = (data['SNN'] == 1.1) | (data['SNN'] == 1.2) | (data['SNN'] == 1.4) | (data['SNN'] == 2.1) | (data['SNN'] == 2.2) | (data['SNN'] == 2.3) | (data['SNN'] == 3.1) | (data['SNN'] == 3.2) | (data['SNN'] == 3.3) | (data['SNN'] == 4.1) | (data['SNN'] == 5.2) | (data['SNN'] == 5.4) | (data['SNN'] == 5.5) | (data['SNN'] == 6.1) | (data['SNN'] == 7.1) | (data['SNN'] == 7.3) | (data['SNN'] == 8.1)
is_NURSING = data['TYPE'] == 'N'

#DOGdataset
mydata = data[is_DOG & is_SNN & is_NURSING]

#Filters
is_documented = (mydata['DOCUMENTED'] == 'D') | (mydata['DOCUMENTED'] == 'ND')
is_timely = (mydata['TIMELY'] == 'Y') | (mydata['TIMELY'] == 'N')
is_legible = (mydata['LEGIBLE'] == 'Y') | (mydata['LEGIBLE'] == 'N')
is_complete = (mydata['COMPLETE'] == 'Y') | (mydata['COMPLETE'] == 'N')
is_met = (mydata['MET'] == 'M') | (mydata['MET'] == 'NM')

#Filtered Dataframes
documentedDOG = mydata[is_documented]
timelyDOG = mydata[is_timely]
legibleDOG = mydata[is_legible]
completeDOG = mydata[is_complete]
metDOG = mydata[is_met]

#Scores vs Documents
documented = documentedDOG.groupby('DOCUMENTS')['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Documented_Scores = documented.to_frame()

timely = timelyDOG.groupby('DOCUMENTS')['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Timeliness_Scores = timely.to_frame()

legible = legibleDOG.groupby('DOCUMENTS')['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Legibility_Scores = legible.to_frame()

complete = completeDOG.groupby('DOCUMENTS')['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Completeness_Scores = complete.to_frame()

met = metDOG.groupby('DOCUMENTS')['MET'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Met_Scores = met.to_frame()

#Results
#Closed Charts Data for the Obstetrics & Gynaecology Department:
print('DOCUMENTED SCORES DOG')
print(documentedDOG['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('TIMELINESS SCORES DOG')
print(timelyDOG['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('LEGIBILITY SCORES DOG')
print(legibleDOG['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('COMPLETENESS SCORES DOG')
print(completeDOG['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('OVERALL COMPLIANCE SCORES DOG')
print(metDOG['MET'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
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
