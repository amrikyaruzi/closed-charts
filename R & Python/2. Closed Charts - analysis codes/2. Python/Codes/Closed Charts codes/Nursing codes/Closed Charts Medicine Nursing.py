# -*- coding: utf-8 -*-
"""
Created on Wed Sep 23 10:29:58 2020

@author: amri.kyaruzi
"""

import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\Q1-Q2.csv", engine='python')
print(data.head())

#DOMFilters
is_DOM = data['DEPT'] == 'DOM'
is_SNN = (data['SNN'] == 1.1) | (data['SNN'] == 1.2) | (data['SNN'] == 1.3) | (data['SNN'] == 1.4) | (data['SNN'] == 2.1) | (data['SNN'] == 2.2) | (data['SNN'] == 2.3) | (data['SNN'] == 3.1) | (data['SNN'] == 3.2) | (data['SNN'] == 3.3) | (data['SNN'] == 4.1) | (data['SNN'] == 5.2) | (data['SNN'] == 5.4) | (data['SNN'] == 5.5) | (data['SNN'] == 6.1) | (data['SNN'] == 7.1) | (data['SNN'] == 7.3) | (data['SNN'] == 8.1)
is_NURSING = data['TYPE'] == 'N'

#DOMdataset
mydata = data[is_DOM & is_SNN & is_NURSING]

#Filters
is_documented = (mydata['DOCUMENTED'] == 'D') | (mydata['DOCUMENTED'] == 'ND')
is_timely = (mydata['TIMELY'] == 'Y') | (mydata['TIMELY'] == 'N')
is_legible = (mydata['LEGIBLE'] == 'Y') | (mydata['LEGIBLE'] == 'N')
is_complete = (mydata['COMPLETE'] == 'Y') | (mydata['COMPLETE'] == 'N')
is_met = (mydata['MET'] == 'M') | (mydata['MET'] == 'NM')

#Filtered Dataframes
documentedDOM = mydata[is_documented]
timelyDOM = mydata[is_timely]
legibleDOM = mydata[is_legible]
completeDOM = mydata[is_complete]
metDOM = mydata[is_met]

#Scores vs Documents
documented = documentedDOM.groupby('DOCUMENTS')['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Documented_Scores = documented.to_frame()

timely = timelyDOM.groupby('DOCUMENTS')['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Timeliness_Scores = timely.to_frame()

legible = legibleDOM.groupby('DOCUMENTS')['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Legibility_Scores = legible.to_frame()

complete = completeDOM.groupby('DOCUMENTS')['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Completeness_Scores = complete.to_frame()

met = metDOM.groupby('DOCUMENTS')['MET'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Met_Scores = met.to_frame()

#Results
#Closed Charts Data for the Internal Medicine Department:
print('DOCUMENTED SCORES DOM')
print(documentedDOM['DOCUMENTED'].value_counts(normalize=True).mul(100).round(1).astype(str) + '%')
print('')
print('TIMELINESS SCORES DOM')
print(timelyDOM['TIMELY'].value_counts(normalize=True).mul(100).round(1).astype(str) + '%')
print('')
print('LEGIBILITY SCORES DOM')
print(legibleDOM['LEGIBLE'].value_counts(normalize=True).mul(100).round(1).astype(str) + '%')
print('')
print('COMPLETENESS SCORES DOM')
print(completeDOM['COMPLETE'].value_counts(normalize=True).mul(100).round(1).astype(str) + '%')
print('')
print('OVERALL COMPLIANCE SCORES DOM')
print(metDOM['MET'].value_counts(normalize=True).mul(100).round(1).astype(str) + '%')
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
