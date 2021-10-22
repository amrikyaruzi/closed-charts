# -*- coding: utf-8 -*-
"""
Created on Fri Sep  4 11:52:39 2020

@author: amri.kyaruzi
"""

import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Documents\A&E April to June Closed Charts.csv", engine='python')
print(data.head())

mydata = data[['MR.Number', 'SNN', 'NURSE', 'DOCUMENTS', 'TYPE', 'DOCUMENTED', 'TIMELY', 'LEGIBLE', 'COMPLETE', 'MET']]


#Filters
is_NURSE = (mydata['TYPE'] == 'N')
is_documented = (mydata['DOCUMENTED'] == 'D') | (mydata['DOCUMENTED'] == 'ND')
is_timely = (mydata['TIMELY'] == 'Y') | (mydata['TIMELY'] == 'N')
is_legible = (mydata['LEGIBLE'] == 'Y') | (mydata['LEGIBLE'] == 'N')
is_complete = (mydata['COMPLETE'] == 'Y') | (mydata['COMPLETE'] == 'N')
is_met = (mydata['MET'] == 'M') | (mydata['MET'] == 'NM')

mydata = mydata[is_NURSE]

#Filtered Dataframes
documentedDAE = mydata[is_documented]
timelyDAE = mydata[is_timely]
legibleDAE = mydata[is_legible]
completeDAE = mydata[is_complete]
metDAE = mydata[is_met]

#Scores vs RMO
documentedNURSE = documentedDAE.groupby(['NURSE'])['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Documented_ScoresNURSE = documentedNURSE.to_frame()

timelyNURSE = timelyDAE.groupby('NURSE')['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Timeliness_ScoresNURSE = timelyNURSE.to_frame()

legibleNURSE = legibleDAE.groupby('NURSE')['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Legibility_ScoresNURSE = legibleNURSE.to_frame()

completeNURSE = completeDAE.groupby('NURSE')['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Completeness_ScoresNURSE = completeNURSE.to_frame()

metNURSE = metDAE.groupby('NURSE')['MET'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Met_ScoresNURSE = metNURSE.to_frame()

#Scores vs Documents
documented = documentedDAE.groupby('DOCUMENTS')['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Documented_Scores = documented.to_frame()

timely = timelyDAE.groupby('DOCUMENTS')['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Timeliness_Scores = timely.to_frame()

legible = legibleDAE.groupby('DOCUMENTS')['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Legibility_Scores = legible.to_frame()

complete = completeDAE.groupby('DOCUMENTS')['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Completeness_Scores = complete.to_frame()

met = metDAE.groupby('DOCUMENTS')['MET'].value_counts(normalize=True).mul(100).round(2).astype('str') + "%"
Met_Scores = met.to_frame()

#Results
#Closed Charts Data for the Emergency Department:
print('DOCUMENTED SCORES DAE')
print(documentedDAE['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype('str') + '%')
print('')
print('TIMELINESS SCORES DAE')
print(timelyDAE['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype('str') + '%')
print('')
print('LEGIBILITY SCORES DAE')
print(legibleDAE['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype('str') + '%')
print('')
print('COMPLETENESS SCORES DAE')
print(completeDAE['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype('str') + '%')
print('')
print('OVERALL COMPLIANCE SCORES DAE')
print(metDAE['MET'].value_counts(normalize=True).mul(100).round(2).astype('str') + '%')
print('')

print('Summary per Parameter')

print(Documented_ScoresNURSE)
print('')

print(Timeliness_ScoresNURSE)
print('')

print(Legibility_ScoresNURSE)
print('')

print(Completeness_ScoresNURSE)
print('')

print(Met_ScoresNURSE)
print('')
