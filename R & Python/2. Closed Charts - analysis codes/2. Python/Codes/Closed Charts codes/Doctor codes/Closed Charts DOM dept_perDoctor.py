
import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\Q2 2020\Q2 2020.csv", engine='python')
print(data.head())
doctorslist = data[(data['DEPT'] == 'DOM')].groupby('DOCTOR')['DOCTOR'].value_counts()

#DOMFilters
is_DOM = data['DEPT'] == 'DOM'
is_SNN = (data['SNN'] == 1.1) | (data['SNN'] == 1.2) | (data['SNN'] == 1.3) | (data['SNN'] == 1.4) | (data['SNN'] == 2.1) | (data['SNN'] == 2.2) | (data['SNN'] == 2.3) | (data['SNN'] == 3.1) | (data['SNN'] == 3.2) | (data['SNN'] == 3.3) | (data['SNN'] == 4.1) | (data['SNN'] == 5.2) | (data['SNN'] == 5.4) | (data['SNN'] == 5.5) | (data['SNN'] == 6.1) | (data['SNN'] == 7.1) | (data['SNN'] == 7.3) | (data['SNN'] == 8.1)
is_TYPE = data['TYPE'] == 'P'
is_DOCTOR = ((data['DOCTOR'] == 'Wambura, Casmir'))

#DOMdataset
mydata = data[is_DOM & is_SNN & is_TYPE & is_DOCTOR]

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

#Results
#Closed Charts Data for the Internal Medicine Department:
print('DOCUMENTED SCORES DOM')
print(documentedDOM['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('TIMELINESS SCORES DOM')
print(timelyDOM['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('LEGIBILITY SCORES DOM')
print(legibleDOM['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('COMPLETENESS SCORES DOM')
print(completeDOM['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('OVERALL COMPLIANCE SCORES DOM')
print(metDOM['MET'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')

