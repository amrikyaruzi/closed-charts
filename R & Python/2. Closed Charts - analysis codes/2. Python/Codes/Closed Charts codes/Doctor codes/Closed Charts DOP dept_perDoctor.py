#DOP Physician data:
import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Desktop\Q2 2020\Q2 2020.csv", engine='python')
print(data.head())
doctorslist = data[(data['DEPT'] == 'DOP')].groupby('DOCTOR')['DOCTOR'].value_counts()

#DOPFilters
is_DOP = data['DEPT'] == 'DOP'
is_SNN = (data['SNN'] == 1.1) | (data['SNN'] == 1.2) | (data['SNN'] == 1.4) | (data['SNN'] == 2.1) | (data['SNN'] == 2.2) | (data['SNN'] == 2.3) | (data['SNN'] == 3.1) | (data['SNN'] == 3.2) | (data['SNN'] == 3.3) | (data['SNN'] == 4.1) | (data['SNN'] == 5.2) | (data['SNN'] == 5.4) | (data['SNN'] == 5.5) | (data['SNN'] == 6.1) | (data['SNN'] == 8.1)
is_TYPE = data['TYPE'] == 'P'
is_DOCTOR = data['DOCTOR'] == 'Walli, Nahida'

#DOPdataset
mydata = data[is_DOP & is_SNN & is_TYPE & is_DOCTOR]

#Filters
is_documented = (mydata['DOCUMENTED'] == 'D') | (mydata['DOCUMENTED'] == 'ND')
is_timely = (mydata['TIMELY'] == 'Y') | (mydata['TIMELY'] == 'N')
is_legible = (mydata['LEGIBLE'] == 'Y') | (mydata['LEGIBLE'] == 'N')
is_complete = (mydata['COMPLETE'] == 'Y') | (mydata['COMPLETE'] == 'N')
is_met = (mydata['MET'] == 'M') | (mydata['MET'] == 'NM')

#Filtered Dataframes
documentedDOP = mydata[is_documented]
timelyDOP = mydata[is_timely]
legibleDOP = mydata[is_legible]
completeDOP = mydata[is_complete]
metDOP = mydata[is_met]

#Results
#Closed Charts Data for the Paediatrics Dept:
print('DOCUMENTED SCORES DOP')
print(documentedDOP['DOCUMENTED'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('TIMELINESS SCORES DOP')
print(timelyDOP['TIMELY'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('LEGIBILITY SCORES DOP')
print(legibleDOP['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('COMPLETENESS SCORES DOP')
print(completeDOP['COMPLETE'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')
print('')
print('OVERALL COMPLIANCE SCORES DOP')
print(metDOP['MET'].value_counts(normalize=True).mul(100).round(2).astype(str) + '%')

