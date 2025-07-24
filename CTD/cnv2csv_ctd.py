import numpy as np
import pandas as pd
import sys
import os
from glob import iglob


# after processing in Sea-Bird,
# read through each .cnv file in the CTD folder as pd df
# set col names and convert to csv in the same directory


pd.set_option('display.max_columns', None)

#exnb =[1806,1811,1902,1903L2,1904,1905L2,1907,2103,2107,2206,2205,2104,2201]
exnb =['1902']

for i in range(len(exnb)):
        print('i =',i)
        for file in iglob("CTD/EX%s/*.cnv"%(exnb[i])):
                print(file)
                df = pd.read_csv('%s'%(file), sep='\s+', engine = "python",skiprows=208, header=None)
                df.columns = ['prDE','t090C','c0S/m','depSM','timeQ','prDM','seaTurbMtr','upoly0','modError','sal00','svCM','sbeox0Mg/L', 'sbeox0ML/L', 'sbox0Mm/Kg', 'flag']
                #print(df.head)
                output_filename = file.replace(".cnv", "")
                df.to_csv('%s.csv'%(output_filename), index=False)

sys.exit()
