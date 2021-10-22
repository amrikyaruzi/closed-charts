#DOG Departmental data:
import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\Q2 2020\Q2 2020.csv", engine='python')
print(data.head())
doctorslist = data[(data['DEPT'] == 'DOG')].groupby('DOCTOR')['DOCTOR'].value_counts()

#DOGFilters
is_DOG = data['DEPT'] == 'DOG'
is_SNN = (data['SNN'] == 1.1) | (data['SNN'] == 1.2) | (data['SNN'] == 1.4) | (data['SNN'] == 2.1) | (data['SNN'] == 2.2) | (data['SNN'] == 2.3) | (data['SNN'] == 3.1) | (data['SNN'] == 3.2) | (data['SNN'] == 3.3) | (data['SNN'] == 4.1) | (data['SNN'] == 5.2) | (data['SNN'] == 5.4) | (data['SNN'] == 5.5) | (data['SNN'] == 6.1) | (data['SNN'] == 7.1) | (data['SNN'] == 7.3) | (data['SNN'] == 8.1)
is_TYPE = data['TYPE'] == 'P'
is_DOCTOR = (data['DOCTOR'] == 'Muzo, Jane')
#DOGdataset
mydata = data[is_DOG & is_SNN & is_TYPE & is_DOCTOR]

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

