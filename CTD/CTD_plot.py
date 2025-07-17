import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys
import os
from glob import iglob
from mpl_toolkits.axes_grid1 import host_subplot
import matplotlib.ticker as ticker


pd.set_option('display.max_columns', None)

#exnb =[1806,1811,1902,'1903L2',1904,'1905L2',1907,2103,2107,2206,2205,2104,2201]
print('Check exnb before running!!      #####')
exnb =['1903L2']

for i in range(len(exnb)):
        print('i =',i)
        if exnb[i]=='1903L2':
                abc = iglob("CTD/EX%s/*cleaned.csv"%(exnb[i]))
                #abc = iglob("CTD/EX%s/ROVCTD/*.csv"%(exnb[i]))
        else:
                abc = iglob("CTD/EX%s/ROVCTD/*.csv"%(exnb[i]))
        for file in abc:
                print(file)
                df = pd.read_csv('%s'%(file), engine = "python")
                temp = df['t090C']
                prde = df['prDE']
                sal = df['sal00']
                oxygen = df['sbeox0Mg/L']

                fig, ax1 = plt.subplots()
                fig.set_size_inches(5,8)
                #plt.ylim(1350,0)

                # ax1: temp and pressure
                ax1.grid(axis='y', which='major')
                ax1.set_xlim(0,30)
                ax1.set_ylabel('pressure, psi', color='black')
                ax1.set_xlabel('temperature, ITS-90 [deg C]', color='blue')
                ax1.tick_params(axis='y', colors='black')
                ax1.tick_params(axis='x', colors='blue')
                ax1.minorticks_on()

                plt.plot(temp,prde,color='blue',linewidth=0.75)

                # ax2: Salinity vs. pressure
                ax2 = ax1.twiny() 
                ax2.set_xlabel('salinity', color='red')
                ax2.tick_params(axis='x', colors='red')
                ax2.minorticks_on()
                #ax2.set_xlim(
                
                plt.plot(sal,prde,color='red',linewidth=0.75)

                # ax3: Oxygen vs. pressure
                ax3 = ax1.twiny()

                ax3.xaxis.set_label_position('bottom')
                ax3.xaxis.set_ticks_position('bottom')
                ax3.spines['bottom'].set_position(('outward', 36))
               
                ax3.set_xlabel('oxygen, SBE 43, Mg/L')
                ax3.tick_params(axis='x', colors='green')
                ax3.minorticks_on()

                plt.plot(oxygen,prde,color='green',linewidth=0.75)

                fig.suptitle('EX%s'%(exnb[i]),y=0.98)
                #ax3.legend(fancybox=True, framealpha=0.5, loc='best')
                #plt.legend()

                #plt.show()
                #sys.exit()
                output_filename = file.replace(".csv", "")
                plt.savefig('%s.jpg'%(output_filename), bbox_inches='tight', pad_inches=0.02, dpi=150)

                plt.close()



sys.exit()

# for paneled plots: fig, axs = plt.subplots(ncols=2, nrows=2, figsize=(5.5, 3.5), layout="constrained")

long_names = {
		'prDE': 'prDE: Pressure, Digiquartz [psi]',
		't090C': 'Temperature [ITS-90, deg C]', 
		'c0S/m': 'Conductivity [S/m]', 
		'depSM': 'Depth [salt water, m]',
		'timeQ': 'Time, NMEA [seconds]', 
		'prDM': 'Pressure, Digiquartz [db]', 
		'seaTurbMtr': 'Turbidity, Seapoint [FTU]', 
		'upoly0': 'Upoly 0, PMEL ORP', 
		'modError': 'Modulo Error Count', 
		'sal00': 'Salinity, Practical [PSU]', 
		'svCM': 'Sound Velocity [Chen-Millero, m/s]', 
		'sbeox0Mg/L': 'Oxygen, SBE 43 [mg/l]', 
		'sbeox0ML/L': 'Oxygen, SBE 43 [ml/l]', 
		'sbox0Mm/Kg': 'Oxygen, SBE 43 [umol/kg]', 
		'flag':  'flag: 0.000e+00'
    }
# for plotting purposes, rename columns to long names
 # df.rename(columns=long_names).to_csv('nicely-formatted-output.csv') # change output file name
