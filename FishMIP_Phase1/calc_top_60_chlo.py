#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 16 15:21:34 2018

@author: ryanheneghan
"""
import numpy as np
import xarray as xr
import glob
import re
#from dateutil.relativedelta import *
#from datetime import datetime
#from netCDF4 import date2num

# Set curr directory

# Path to files
root = '/work/bb0820/ISIMIP/ISIMIP3b/InputData/climate/ocean/uncorrected/global/monthly/'
save_root = '/pf/b/b380694/FishMIP_CMIP6/chl_60/'
esms = ['CESM2/', 'GFDL-ESM4/', 'IPSL-CM6A-LR/', 'MPI-ESM1-2-HR/', 'UKESM1-0-LL/']
esm_save = ['cesm2_r4i1p1f1','gfdl-esm4_r1i1p1f1', 'ipsl-cm6a-lr_r1i1p1f1', 'mpi-esm1-2-hr_r1i1p1f1', 'ukesm1-0-ll_r1i1p1f2']
depth_60 = [6, 5, 20, 6, 20]
weights_to_60 = {}
weights_to_60[1] = [10,10,10,10,10,10]
weights_to_60[2] =[5,10,10,15,20] 
weights_to_60[3] = [1.011520, 1.088670, 1.134984, 1.242212, 1.325950, 1.479396, 1.619574, 1.840888, 2.060972, 2.373748, 2.694812, 3.114148, 3.541492, 4.059868, 4.572652, 5.158628,5.721012, 6.342728, 6.934772, 2.681978]
weights_to_60[4] = [12,10,10,10,10,8]
weights_to_60[5] = [1.011520, 1.088670, 1.134984, 1.242212, 1.325950, 1.479396, 1.619574, 1.840888, 2.060972, 2.373748, 2.694812, 3.114148, 3.541492, 4.059868, 4.572652, 5.158628,5.721012, 6.342728, 6.934772, 2.681978]
num_esms = len(esms)
scens = ['historical/', 'ssp126/', 'ssp370/', 'ssp585/']
scens_save = ['historical', 'ssp126', 'ssp370', 'ssp585']
num_scens = len(scens)

# Variable names, vars = to find the files, 
# var_names = to manipulate the variables in the files

vars=["*chl_onedeg*"]
var_names=["chl"]
num_vars=len(vars) # How many variables there are

for f in range(0, num_esms): # Loop over esms
    curr_esm=esms[f]
    curr_esm_save=esm_save[f]
    curr_depthlev=depth_60[f]
    curr_weights = weights_to_60[f+1]
    curr_depth60 = depth_60[f]

    for g in range(0,num_scens): # Loop over scens
        curr_scen=scens[g]
        curr_scen_save=scens_save[g]

        for h in range(0,num_vars): # Looping over the variables
            curr_var=vars[h] # Current variable to identify files
            all_files=glob.glob(root+curr_scen+'/'+curr_esm+'/'+curr_var) # Pulls out all zall files with this variable
            num_files=len(all_files) # How many files there are...
            var_name=var_names[h] # Current variable name for file manipulation
            
            ## For loop over all zall files with this variable
            for i in range(0,num_files):
                
                curr_file=all_files[i] # Current file name
                
                print("Now working on " + curr_file)
                
                CURR_NCFILE = xr.open_dataset(curr_file, decode_times=False) # Open current file
                
                # Extract lat, lon, time dimensions
                num_lat = len(CURR_NCFILE['lat'])
                num_lon = len(CURR_NCFILE['lon'])
                num_time = len(CURR_NCFILE['time']) 
                
                # Initialise storage
                VAR_bot = np.zeros((num_time, num_lat, num_lon))
                
                # loop over time
                for time in range(0,num_time):
                    print(time)
                    
                    VAR = CURR_NCFILE[var_name][time,:,:,:].values # pull out values for current time slice

                    for j in range(0,num_lat): # loop over latitude
                        for k in range(0,num_lon): # loop over longitude
                            VAR_SPEC = VAR[:,j,k] # pull out lat, lon slice
                            VAR_UNMASK = VAR_SPEC[~np.isnan(VAR_SPEC)] # pull out only unmasked values
                            if len(VAR_UNMASK) == 0: # if there are no unmasked values
                                VAR_bot[time,j,k] = np.NAN # there are no values in this cell, so temperature is "nan"
                            if len(VAR_UNMASK) > 0: # if there are unmasked values
                                if len(VAR_UNMASK) >= curr_depth60: # if depth is greater than or equal to 60m
                                    VAR_bot[time,j,k] = np.sum((VAR_UNMASK[0:(curr_depthlev)]*curr_weights)/sum(curr_weights)) # ave. temp, weighted by depth intervals
                                if len(VAR_UNMASK) < curr_depth60: # if depth is less than 60m
                                    VAR_bot[time,j,k] = np.sum((VAR_UNMASK[0:len(VAR_UNMASK)]*curr_weights[0:len(VAR_UNMASK)])/sum(curr_weights[0:len(VAR_UNMASK)])) # ave. temp, weighted by depth intervals

                        
                ##### Now, we want to construct the netcdf which will carry the bottom variable        
                # Create temporary dataset "ds", with time, long and lats from original netcdf we're working from
                ds = xr.Dataset({'time': CURR_NCFILE['time'], 'lon': CURR_NCFILE['lon'],'lat':CURR_NCFILE['lat']})
                # Put the bottom values in a dataarray "da" with corresponding dimensions to "ds"
                da = xr.DataArray(VAR_bot, dims = ['time','lat','lon'])
                # Put da in ds, under current var_name
                ds[var_name]=da
            
                # Fill in attributes of the variable, using info from the current netcdf we're using
                ds[var_name].attrs['long_name'] = "chlorophyll average over top 60m"
                ds[var_name].attrs['units'] = CURR_NCFILE[var_name].units
            
                ## Fill in attributes of time, using info from the current netcdf we're using
#                ds['time'].attrs['long_name'] = CURR_NCFILE['time'].long_name
                ds['time'].attrs['units'] = CURR_NCFILE['time'].units
#                ds['time'].attrs['bounds'] = CURR_NCFILE['time'].bounds
                ds['time'].attrs['calendar'] = CURR_NCFILE['time'].calendar

                ## Fill in attributes of lat, using info from the current netcdf we're using
#                ds['lat'].attrs['long_name'] = CURR_NCFILE['lat'].long_name
                ds['lat'].attrs['units'] = CURR_NCFILE['lat'].units
                ds['lat'].attrs['axis'] = CURR_NCFILE['lat'].axis

                ## Fill in attributes of lon, using info from the current netcdf we're using
#                ds['lon'].attrs['long_name'] = CURR_NCFILE['lon'].long_name
                ds['lon'].attrs['units'] = CURR_NCFILE['lon'].units
                ds['lon'].attrs['axis'] = CURR_NCFILE['lon'].axis
                
                # Get name of new netcdf
                DATE=re.split("[_.]", curr_file) # Extract date
                DATE=DATE[len(DATE)-3]+'_'+DATE[len(DATE)-2]
                
                # Bits of the new name
                pre = curr_esm_save + '_' + curr_scen_save + '_'
                post = '.nc'
                depth='_z60'
                timer2="_onedeg_global_monthly_"
                
                # New name
                new_name = save_root + pre + var_name + depth + timer2 + DATE + post
            
                # Save ds to netcdf, with new name
                ds.to_netcdf(new_name)
                
    
