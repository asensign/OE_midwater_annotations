from glob import glob
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from matplotlib_scalebar.scalebar import ScaleBar
import numpy as np
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.io.shapereader as shpreader
import cartopy
from cartopy.mpl.gridliner import LongitudeFormatter, LatitudeFormatter
from cartopy.feature import ShapelyFeature
import sys
import os
import warnings


# read in acoustic data as csv

files_1806 = glob("../../../echoview_output/EX1806*.csv")
files_1903L2 = glob("../../../echoview_output/EX1903*.csv")
files_2107 = glob("../../../echoview_output/EX2107*.csv")

df_list_1806 = []
df_list_1903L2 = []
df_list_2107 = []
	
for file in files_1806:
	df1 = pd.read_csv(file)
	df_list_1806.append(df1)	
for file2 in files_1903L2:
	df12 = pd.read_csv(file2)
	df_list_1903L2.append(df12)	
for file3 in files_2107:
	df13 = pd.read_csv(file3)
	df_list_2107.append(df13)	

df_1806 = pd.concat(df_list_1806)
df_1903L2 = pd.concat(df_list_1903L2)
df_2107 = pd.concat(df_list_2107)


#print(df_1903L2.head())
#print(df_2107.head())

cruise_latsS_1806 = df_1806["Lat_S"]
cruise_lonsS_1806 = df_1806["Lon_S"]
cruise_latsM_1806 = df_1806["Lat_M"]
cruise_lonsM_1806 = df_1806["Lon_M"]
cruise_latsE_1806 = df_1806["Lat_E"]
cruise_lonsE_1806 = df_1806["Lon_E"]

cruise_lons_1806 = pd.DataFrame(pd.concat([cruise_lonsS_1806, cruise_lonsM_1806,  cruise_lonsE_1806], ignore_index=True))
print(cruise_lons_1806)
cruise_lats_1806 = pd.DataFrame(pd.concat([cruise_latsS_1806, cruise_latsM_1806,  cruise_latsE_1806], ignore_index=True))
print(cruise_latsE_1806)
print(cruise_lats_1806)


cruise_latsS_1903L2 = df_1903L2["Lat_S"]
cruise_lonsS_1903L2 = df_1903L2["Lon_S"]
cruise_latsM_1903L2 = df_1903L2["Lat_M"]
cruise_lonsM_1903L2 = df_1903L2["Lon_M"]
cruise_latsE_1903L2 = df_1903L2["Lat_E"]
cruise_lonsE_1903L2 = df_1903L2["Lon_E"]

cruise_lons_1903L2 = pd.DataFrame(pd.concat([cruise_lonsS_1903L2, cruise_lonsM_1903L2, cruise_lonsE_1903L2], ignore_index=True))
print(cruise_lons_1903L2)
cruise_lats_1903L2 = pd.DataFrame(pd.concat([ cruise_latsS_1903L2, cruise_latsM_1903L2, cruise_latsE_1903L2], ignore_index=True))
print(cruise_lats_1903L2)

cruise_latsS_2107 = df_2107["Lat_S"]
cruise_lonsS_2107 = df_2107["Lon_S"]
cruise_latsM_2107 = df_2107["Lat_M"]
cruise_lonsM_2107 = df_2107["Lon_M"]
cruise_latsE_2107 = df_2107["Lat_E"]
cruise_lonsE_2107 = df_2107["Lon_E"]

cruise_lons_2107 = pd.DataFrame(pd.concat([cruise_lonsS_2107, cruise_lonsM_2107, cruise_lonsE_2107], ignore_index=True))
print(cruise_lons_2107)
cruise_lats_2107 = pd.DataFrame(pd.concat([cruise_latsS_2107, cruise_latsM_2107, cruise_latsE_2107], ignore_index=True))
print(cruise_lats_2107)

#--------------------------------------------------------------

# set coords from midwater dive summary table (google sheets)
dives_1903L2 =  [2, 6, 8, 9, 14]
lats_1903L2 = [29.110683,30.4344,30.9179,31.528833,35.73515]
lons_1903L2 = [-79.445017,-79.581983,-78.087233,-77.15485,-74.81225]
dives_1806 = [2, 4, 15]
lats_1806 = [31.94937883,30.94027766,35.53850111]
lons_1806 = [-75.52429803,-77.32827961,-74.80059902]
dives_2107 = 3, 13
lats_2107 = [30.70846,28.36854]
lons_2107 = [-77.41911,-78.25901]

# set coord. bounds for plot
lat0 = -65
lat1 = -87
lon0 = 20
lon1 = 40

#--------------------------------------------------------------

#Ocean bathymetry
#----------------

def load_bathymetry(zip_file_url):
    #"""Read zip file from Natural Earth containing bathymetry shapefiles"""
    # Download and extract shapefiles
    import io
    import zipfile

    import requests
    r = requests.get(zip_file_url)
    z = zipfile.ZipFile(io.BytesIO(r.content))
    z.extractall("ne_10m_bathymetry_all/")

    # Read shapefiles, sorted by depth
    shp_dict = {}
    files = glob('ne_10m_bathymetry_all/*.shp')
    assert len(files) > 0
    files.sort()
    depths = []
    for f in files:
        depth = '-' + f.split('_')[-1].split('.')[0]  # depth from file name
        depths.append(depth)
        #bbox = (72, 20, 87, 40)  # (x0, y0, x1, y1)//min lon, min lat, max loln, max lat
        nei = shpreader.Reader(f)#, bbox=bbox)
        shp_dict[depth] = nei
    depths = np.array(depths)[::-1]  # sort from surface to bottom
    return depths, shp_dict


if __name__ == "__main__":
    # Load data (14.8 MB file)
    depths_str, shp_dict = load_bathymetry(
        'https://naturalearth.s3.amazonaws.com/' +
        '10m_physical/ne_10m_bathymetry_all.zip')

    # Construct a discrete colormap with colors corresponding to each depth
    depths = depths_str.astype(int)
    N = len(depths)
    nudge = 0.01  # shift bin edge slightly to include data
    boundaries = [min(depths)] + sorted(depths+nudge)  # low to high
    norm = matplotlib.colors.BoundaryNorm(boundaries, N)
    blues_cm = matplotlib.colormaps['Blues_r'].resampled(N)
    colors_depths = blues_cm(norm(depths))

    # Set up plot
    data_crs = ccrs.PlateCarree()

    subplot_kw = {'projection': ccrs.Miller()}
    fig, ax = plt.subplots(subplot_kw=subplot_kw, figsize=(9, 7))
    ax.set_extent([lat0, lat1, lon0, lon1], crs=ccrs.PlateCarree())  # x0, x1, y0, y1

    # Iterate and plot feature for each depth level
    #for i, depth_str in enumerate(depths_str):
     #   ax.add_geometries(shp_dict[depth_str].geometries(),
      #                    crs=ccrs.PlateCarree(),
       #                   color=colors_depths[i])

    # Add features
    #ax.add_feature(cfeature.LAND,transform=data_crs)
    #ax.coastlines(transform=data_crs)
    #islands = cartopy.feature.NaturalEarthFeature('physical', 'minor_islands', '10m',transform=data_crs)
    #ax.add_feature(islands,transform=data_crs)
    ax.add_feature(cfeature.STATES,transform=data_crs)

    # plot
    ax.plot(cruise_lons_1806,cruise_lats_1806,color="#CD34B5", linestyle='dotted',label='EX1806',transform=data_crs)
    ax.plot(cruise_lons_1903L2,cruise_lats_1903L2,color="#0000FF", linestyle='dotted',label='EX1903L2', transform=data_crs)
    ax.plot(cruise_lons_2107,cruise_lats_2107,color="#FFD700", linestyle='dotted',label='EX2107', transform=data_crs)

    # add legend
    plt.legend(loc='upper right')

    plt.title("Cruise tracks for studied expeditions")
  
    # Convert vector bathymetries to raster (saves a lot of disk space) while leaving labels as vectors
    ax.set_rasterized(True)
    
    plt.savefig("../../../Figures/cruise-track_plot.png") 
    plt.show()

sys.exit()


