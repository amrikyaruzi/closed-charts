# -*- coding: utf-8 -*-
"""
Created on Wed Sep 23 10:50:00 2020

@author: amri.kyaruzi
"""


import pandas as pd
mydata = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\A&E January to June Closed Charts.csv", engine='python')
mydata = mydata[mydata['TYPE'] == 'N']
print(mydata.head())

#Filters
is_documented = (mydata['DOCUMENTED'] == 'D') | (mydata['DOCUMENTED'] == 'ND')
is_timely = (mydata['TIMELY'] == 'Y') | (mydata['TIMELY'] == 'N')
is_legible = (mydata['LEGIBLE'] == 'Y') | (mydata['LEGIBLE'] == 'N')
is_complete = (mydata['COMPLETE'] == 'Y') | (mydata['COMPLETE'] == 'N')
is_met = (mydata['MET'] == 'M') | (mydata['MET'] == 'NM')

#Filtered Dataframes
documentedDAE = mydata[is_documented]
timelyDAE = mydata[is_timely]
legibleDAE = mydata[is_legible]
completeDAE = mydata[is_complete]
metDAE = mydata[is_met]

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
