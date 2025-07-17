import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

pd.set_option('display.max_columns', None)
dives = ['01','03','04','05','06','07','08','09','10','11','12','13','14']
mmdd = ['1027','1101','1102','1102','1103','1104','1105','1108','1109','1110','1112','1113','1114']

for abc in range(14):
	i = dives[abc]
	j = mmdd[abc]
	df = pd.read_csv('CTD/EX2107_DIVE%s_2021%s_ROVCTD.cnv'%(i,j), sep='\s+', engine = "python",skiprows=208, header=None) #, encoding="ISO-8859-1")
#	print(df)

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

	df.columns = ['prDE','t090C','c0S/m','depSM','timeQ','prDM','seaTurbMtr','upoly0','modError','sal00','svCM','sbeox0Mg/L', 'sbeox0ML/L', 'sbox0Mm/Kg', 'flag']

	print(i,j)
	print(df)
	

	df.to_csv('CTD/EX2107_DIVE%s_20211114_ROVCTD.csv'%(i), index=False)
	
	# for plotting purposes, rename columns to long names
	# df.rename(columns=long_names).to_csv('nicely-formatted-output.csv') # change output file name

sys.exit()