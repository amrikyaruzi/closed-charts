# -*- coding: utf-8 -*-
"""
Created on Wed Jul 29 15:10:13 2020

@author: amri.kyaruzi
"""


import pandas as pd
data = pd.read_csv(r"C:\Users\amri.kyaruzi\Documents\Use of Abbreviations on Discharge Summaries - Paediatrics.csv", engine = "python")
data.groupby("DATE")['LEGIBLE'].value_counts()
data.groupby("DATE")['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype("str") + "%"

data.groupby(["DATE" , "DOCTOR"])['LEGIBLE'].value_counts()
data.groupby(["DATE" , "DOCTOR"])['LEGIBLE'].value_counts(normalize=True).mul(100).round(2).astype("str") + "%"
