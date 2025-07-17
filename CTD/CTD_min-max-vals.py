import pandas as pd
import sys
import os
from glob import iglob
from mpl_toolkits.axes_grid1 import host_subplot
import matplotlib.ticker as ticker

exnb =['1806','1903L2','2107']
ex_mins_list = []
ex_maxs_list = []

for i in range(len(exnb)):
        if exnb[i]=='1903L2':
            #abc = iglob("../../../CTD/EX%s/ROVCTD/*cleaned.csv"%(exnb[i])) # ROV CTD
            abc = iglob("../../../CTD/EX%s/CPCTD/*.csv"%(exnb[i]))
        else:
            abc = iglob("../../../CTD/EX%s/*.csv"%(exnb[i]))
        # abc is all of the dive files for that expedition (i)

        # define dataframes to write out mins/maxs for each EX
        dive_mins_df = pd.DataFrame()
        dive_maxs_df = pd.DataFrame()

        # create empty lists to store values within the loop
        vals1 = []
        vals2 = []
        vals3 = []
        vals4 = []
        vals5 = []
        vals6 = []
        vals7 = []
        vals8 = []

        
        for divefile in abc:
            #print(divefile)
            df = pd.read_csv('%s'%(divefile), engine = "python")
            temp = df['t090C']
            prde = df['prDE']
            sal = df['sal00']
            oxygen = df['sbeox0Mg/L']
                
             # write out mins and maxes for each dive
            vals1.append(temp.min())
            vals2.append(temp.max())
            vals3.append(prde.min())
            vals4.append(prde.max())
            vals5.append(sal.min())
            vals6.append(sal.max())
            vals7.append(oxygen.min())
            vals8.append(oxygen.max())
        print('all mins/maxs calculated for expedition %s'%(exnb[i]))

        # append the new calculated values (currently list) to df
        # each of these contain a min/max for each dive and each var
        dive_mins_df['t090C_min'] = vals1
        dive_maxs_df['t090C_max'] = vals2
        dive_mins_df['prde_min'] = vals3
        dive_maxs_df['prde_max'] = vals4
        dive_mins_df['salinity_min'] = vals5
        dive_maxs_df['salinity_max'] = vals6
        dive_mins_df['sbeox_min'] = vals7
        dive_maxs_df['sbeox_max'] = vals8
        
        ex_mins_list.append((dive_mins_df['t090C_min']).min())
        ex_maxs_list.append((dive_maxs_df['t090C_max']).max())
        ex_mins_list.append((dive_mins_df['prde_min']).min())
        ex_maxs_list.append((dive_maxs_df['prde_max']).max())
        ex_mins_list.append((dive_mins_df['salinity_min']).min())
        ex_maxs_list.append((dive_maxs_df['salinity_max']).max())
        ex_mins_list.append((dive_mins_df['sbeox_min']).min())
        ex_maxs_list.append((dive_maxs_df['sbeox_max']).max())



# for all expeditions (3 numbers per variable)
all_mins_df = pd.DataFrame(ex_mins_list)
all_maxs_df = pd.DataFrame(ex_maxs_list)
print('mins',all_mins_df)
print('maxs',all_maxs_df)
sys.exit()

