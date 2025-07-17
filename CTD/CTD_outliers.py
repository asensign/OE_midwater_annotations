import numpy as np
import pandas as pd
import sys
import os
from glob import iglob
from scipy import stats

pd.set_option('display.max_columns', None)

#exnb =[1806,1811,1902,'1903L2',1904,'1905L2',1907,2103,2107,2206,2205,2104,2201]
print('Check exnb before running!!      #####')
exnb =['1903L2']

for i in range(len(exnb)):
        print('i =',i)
        for file in iglob("CTD/EX%s/*CPCTD.csv"%(exnb[i])):
                print(file)
                df = pd.read_csv('%s'%(file), engine = "python")
                
                df.rename(columns={'sbeox0Mg/L': 'sbeox'}, inplace=True)
                print(len(df))

                oxygen = df['sbeox']
                temp = df['t090C']
                prde = df['prDE']
                sal = df['sal00']
    
                print('temp min/max',temp.min(),temp.max())
                print('prde min/max',prde.min(),prde.max())
                print('sal min/max',sal.min(),sal.max())
                print('sbe ox min/max',oxygen.min(),oxygen.max())
  
                 # remove outliers based on Module error count increases and variable jumps (e.g. prDE)
                differences = df.diff()
                jumps = differences.query('modError>0 or prDE > 7 or sbeox > 2 or sal00 > 1 or t090C > 3') # JUMPS

                # remove duplicates so as not to remove same indexes twice for jumps list and adjacent rows
                a = jumps.index+1
                b = jumps.index
                c = jumps.index-1
                
                for i in b[:]:
                    if i in a or i in c:
                        b.drop(i)
                #print(b)
                
                df.drop(b, inplace=True)
                print(len(df))

                # Remove outliers based on range
                outliers = df.query('prDE > 5000 or prDE < 0 or sal00 > 100 or sbeox < 0') # RANGE
                df.drop(outliers.index, inplace=True)
                print(len(df))
     

                # now rename vars, format, check removals, and save.
                df.rename(columns={'sbeox': 'sbeox0Mg/L'}, inplace=True)
                oxygen = df["sbeox0Mg/L"]
                temp = df['t090C']
                prde = df['prDE']
                sal = df['sal00']
                oxygen = df['sbeox0Mg/L']

                # now rename vars, format, check removals, and save.
                df.rename(columns={'sbeox': 'sbeox0Mg/L'}, inplace=True)
                oxygen = df["sbeox0Mg/L"]
                temp = df['t090C']
                prde = df['prDE']
                sal = df['sal00']
                oxygen = df['sbeox0Mg/L']

                print('-----AFTER REMOVAL OF OUTLIERS')
                print('temp min/max',temp.min(),temp.max())
                print('new prde min/max',prde.min(),prde.max())
                print('sal min/max',sal.min(),sal.max())
                print('sbe ox min/max',oxygen.min(),oxygen.max())

                #print(df.diff())
                #sys.exit()
                
                output_filename = file.replace(".csv", "")
                df.to_csv('%s_cleaned.csv'%(output_filename), index=False)
                #sys.exit()
               
sys.exit()

                
# where is the error occuring relative to the index?
               # print(df.loc[[55587]])
               # print(df.loc[[55588]]) # mod error changes here, and error is here (prDE jumps, preceding and proceding do not agree)
               # print(df.loc[[55589]])
                # rows match yay! so now:
  
