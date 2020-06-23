rm(list = ls())
setwd("~/Desktop/Papers/FishMIP_NPPvSST/MACROECOLOGICAL_runs/Outputs")

library('MASS')
library('data.table')
library(ncdf4)
library(sp)
library(raster)
library(data.table)
library(RNetCDF)

input_dat <- readRDS('row.no.na.RDS')

files2proc <- c('cesm_clim_forcing0_6batch1.csv', 'cesm_temp-control_forcing0_6batch1.csv',
                'cesm_pi_forcing0_6batch1.csv', 'cesm_npp-control_forcing0_6batch1.csv')

nc_names <- list(c('macroecological_cesm1-bgc_nobc_historical_nosoc_co2_tcb_global_annual_1850_2005.nc4',
              'macroecological_cesm1-bgc_nobc_rcp85_nosoc_co2_tcb_global_annual_2006_2100.nc4'),
              c('macroecological_cesm1-bgc_nobc_temperature-control_nosoc_co2_tcb_global_annual_1850_2005.nc4',
                'macroecological_cesm1-bgc_nobc_temperature-control_nosoc_co2_tcb_global_annual_2006_2100.nc4'),
              c('macroecological_cesm1-bgc_nobc_pre-industrial_nosoc_co2_tcb_global_annual_1850_2005.nc4',
                'macroecological_cesm1-bgc_nobc_pre-industrial_nosoc_co2_tcb_global_annual_2006_2100.nc4'),
              c('macroecological_cesm1-bgc_nobc_npp-control_nosoc_co2_tcb_global_annual_1850_2005.nc4',
                'macroecological_cesm1-bgc_nobc_npp-control_nosoc_co2_tcb_global_annual_2006_2100.nc4'))

nc_names1 <- list(c('macroecological_cesm1-bgc_nobc_historical_nosoc_co2_b10cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_rcp85_nosoc_co2_b10cm_global_annual_2006_2100.nc4'),
                 c('macroecological_cesm1-bgc_nobc_temperature-control_nosoc_co2_b10cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_temperature-control_nosoc_co2_b10cm_global_annual_2006_2100.nc4'),
                 c('macroecological_cesm1-bgc_nobc_pre-industrial_nosoc_co2_b10cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_pre-industrial_nosoc_co2_b10cm_global_annual_2006_2100.nc4'),
                 c('macroecological_cesm1-bgc_nobc_npp-control_nosoc_co2_b10cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_npp-control_nosoc_co2_b10cm_global_annual_2006_2100.nc4'))

nc_names2 <- list(c('macroecological_cesm1-bgc_nobc_historical_nosoc_co2_b30cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_rcp85_nosoc_co2_b30cm_global_annual_2006_2100.nc4'),
                 c('macroecological_cesm1-bgc_nobc_temperature-control_nosoc_co2_b30cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_temperature-control_nosoc_co2_b30cm_global_annual_2006_2100.nc4'),
                 c('macroecological_cesm1-bgc_nobc_pre-industrial_nosoc_co2_b30cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_pre-industrial_nosoc_co2_b30cm_global_annual_2006_2100.nc4'),
                 c('macroecological_cesm1-bgc_nobc_npp-control_nosoc_co2_b30cm_global_annual_1850_2005.nc4',
                   'macroecological_cesm1-bgc_nobc_npp-control_nosoc_co2_b30cm_global_annual_2006_2100.nc4'))

nc_names3 <- c('macroecological_cesm1-bgc_nobc_historical_nosoc_co2_exp-prop_global_annual_1850_2100.nc4',
                  'macroecological_cesm1-bgc_nobc_temperature-control_nosoc_co2_exp-prop_global_annual_1850_2100.nc4',
                  'macroecological_cesm1-bgc_nobc_pre-industrial_nosoc_co2_exp-prop_global_annual_1850_2100.nc4',
                  'macroecological_cesm1-bgc_nobc_npp-control_nosoc_co2_exp-prop_global_annual_1850_2100.nc4')

nc_start <- c(1, 157)
nc_end <- c(156, 251)

for(i in 1:length(files2proc)){
  
  ## Import raw output
  curr_file <- fread(files2proc[i], header=TRUE, sep = ',')
  curr_bioms <- curr_file$V12/curr_file$V6 # to get g m^-2
  
  new_bioms <- rep(NA, 16264800)
  new_bioms[input_dat] <- curr_bioms
  
  # Convert to lon, lat, time grid, and convert wet weight to carbon biomass
  array_biom <- array(new_bioms, dim=c(360,180,251))*0.0352 
  
  # Names of netcdfs for current protocol
  curr_names <- nc_names[[i]] # Name for tcb
  curr_names1 <- nc_names1[[i]] # Name for b10cm
  curr_names2 <- nc_names2[[i]] # Name for b30cm
  
  for(j in 1:length(curr_names)){ # Loop over current variable netcdf names (should be two, one for 1850-2005, one for 2006-2100)
    print(i)
    print(j)
    curr_array <- array_biom[,,c(nc_start[j]:nc_end[j])] 
    curr_name <- curr_names[j]
    
    
    #### TCB
    new_nc <- create.nc(curr_name)
    
    dim.def.nc(new_nc, 'lon', dimlength = 360)
    var.def.nc(new_nc, 'lon', 'NC_FLOAT', 'lon')
    var.put.nc(new_nc, 'lon', c(0:359))
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'long_name', value = 'longitude')
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'units', value = 'degrees')
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR',  name = 'axis', value = 'X')
    
    dim.def.nc(new_nc, 'lat', dimlength = 180)
    var.def.nc(new_nc, 'lat', 'NC_FLOAT', 'lat')
    var.put.nc(new_nc, 'lat', c(-89.5:89.5))
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'long_name', value = 'latitude')
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'units', value = 'degrees_north')
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'axis', value = 'Y')
    
    dim.def.nc(new_nc, 'time', unlim = TRUE)
    var.def.nc(new_nc, 'time', 'NC_DOUBLE', 'time')
    var.put.nc(new_nc, 'time', c(nc_start[j]:nc_end[j])-1) # Fill time values
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'long_name', value = 'time')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'units', value = 'years since 1850-01-15 00:00:00')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'calendar', value = 'standard')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'axis', value = 'T')
    
    var.def.nc(new_nc, 'tcb', "NC_FLOAT", c('lon', 'lat', 'time'))
    att.put.nc(new_nc, variable = 'tcb', type = 'NC_CHAR', name = 'long_name', value = 'Total consumer biomass density from 1g - 1 tonne')
    att.put.nc(new_nc, variable = 'tcb', type = 'NC_CHAR', name = 'short_name', value = 'tcb')
    att.put.nc(new_nc, variable = 'tcb', type = 'NC_CHAR', name = 'units', value = 'g C m^-2')
    att.put.nc(new_nc, variable = 'tcb', type = 'NC_FLOAT', name = 'missing_value', value = 1e20)
    var.put.nc(new_nc, 'tcb', curr_array)
    
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Created by Ryan Heneghan, with help from Simon Jennings <ryan.heneghan@gmail.com>")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "institution", type = "NC_CHAR", value = "Universitat Autonoma de Barcelona")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "date_created", type = "NC_CHAR", value = date())
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "comments", type = "NC_CHAR", value = "Impact model output for ISIMIP2b and FishMIP NPPvSST experimental protocol")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "length-weight_conversion", type = "NC_CHAR", value = 'esd = 2*(3/(4*pi)*weight)^(1/3)')
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "ph_input_used", type = "NC_CHAR", value = "no")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "diazotroph_input_used", type = "NC_CHAR", value = "implicity, as part of integrated phytoplankton production")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "wet-weight to carbon conversion", type = "NC_CHAR", value = "0.0352, from Weibe Fish Bull 73(4)")
    
    
    close.nc(new_nc)
    
    ### B10CM
    
    curr_name1 <- curr_names1[j]
    curr_b10 <- curr_array*(1-0.23) # Calculated using biomass size spectrum slope of -0.06, over tcb size range of 1g - 1e6g
    new_nc <- create.nc(curr_name1)
    
    dim.def.nc(new_nc, 'lon', dimlength = 360)
    var.def.nc(new_nc, 'lon', 'NC_FLOAT', 'lon')
    var.put.nc(new_nc, 'lon', c(0:359))
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'long_name', value = 'longitude')
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'units', value = 'degrees')
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR',  name = 'axis', value = 'X')
    
    dim.def.nc(new_nc, 'lat', dimlength = 180)
    var.def.nc(new_nc, 'lat', 'NC_FLOAT', 'lat')
    var.put.nc(new_nc, 'lat', c(-89.5:89.5))
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'long_name', value = 'latitude')
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'units', value = 'degrees_north')
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'axis', value = 'Y')
    
    dim.def.nc(new_nc, 'time', unlim = TRUE)
    var.def.nc(new_nc, 'time', 'NC_DOUBLE', 'time')
    var.put.nc(new_nc, 'time',  c(nc_start[j]:nc_end[j])-1) # Fill time values
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'long_name', value = 'time')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'units', value = 'years since 1850-01-15 00:00:00')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'calendar', value = 'standard')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'axis', value = 'T')
    
    var.def.nc(new_nc, 'b10cm', "NC_FLOAT", c('lon', 'lat', 'time'))
    att.put.nc(new_nc, variable = 'b10cm', type = 'NC_CHAR', name = 'long_name', value = 'Total consumer biomass density > b10cm (520g - 1 tonne)')
    att.put.nc(new_nc, variable = 'b10cm', type = 'NC_CHAR', name = 'short_name', value = 'b10cm')
    att.put.nc(new_nc, variable = 'b10cm', type = 'NC_CHAR', name = 'units', value = 'g C m^-2')
    att.put.nc(new_nc, variable = 'b10cm', type = 'NC_FLOAT', name = 'missing_value', value = 1e20)
    var.put.nc(new_nc, 'b10cm', curr_b10)
    
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Created by Ryan Heneghan, with help from Simon Jennings <ryan.heneghan@gmail.com>")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "institution", type = "NC_CHAR", value = "Universitat Autonoma de Barcelona")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "date_created", type = "NC_CHAR", value = date())
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "comments", type = "NC_CHAR", value = "Impact model output for ISIMIP2b and FishMIP NPPvSST experimental protocol")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "length-weight_conversion", type = "NC_CHAR", value = 'esd = 2*(3/(4*pi)*weight)^(1/3)')
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "ph_input_used", type = "NC_CHAR", value = "no")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "diazotroph_input_used", type = "NC_CHAR", value = "implicity, as part of integrated phytoplankton production")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "wet-weight to carbon conversion", type = "NC_CHAR", value = "0.0352, from Weibe Fish Bull 73(4)")
    
    
    close.nc(new_nc)
    
    
    ### B30CM
    
    curr_name2 <- curr_names2[j]
    curr_b30 <- curr_array*(1-0.51) # Calculated using biomass size spectrum slope of -0.06, over tcb size range of 1g - 1e6g
    new_nc <- create.nc(curr_name2)
    
    dim.def.nc(new_nc, 'lon', dimlength = 360)
    var.def.nc(new_nc, 'lon', 'NC_FLOAT', 'lon')
    var.put.nc(new_nc, 'lon', c(0:359))
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'long_name', value = 'longitude')
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'units', value = 'degrees')
    att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR',  name = 'axis', value = 'X')
    
    dim.def.nc(new_nc, 'lat', dimlength = 180)
    var.def.nc(new_nc, 'lat', 'NC_FLOAT', 'lat')
    var.put.nc(new_nc, 'lat', c(-89.5:89.5))
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'long_name', value = 'latitude')
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'units', value = 'degrees_north')
    att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'axis', value = 'Y')
    
    dim.def.nc(new_nc, 'time', unlim = TRUE)
    var.def.nc(new_nc, 'time', 'NC_DOUBLE', 'time')
    var.put.nc(new_nc, 'time',  c(nc_start[j]:nc_end[j])-1) # Fill time values
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'long_name', value = 'time')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'units', value = 'years since 1850-01-15 00:00:00')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'calendar', value = 'standard')
    att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'axis', value = 'T')
    
    var.def.nc(new_nc, 'b30cm', "NC_FLOAT", c('lon', 'lat', 'time'))
    att.put.nc(new_nc, variable = 'b30cm', type = 'NC_CHAR', name = 'long_name', value = 'Total consumer biomass density > b30cm (14.1kg - 1 tonne')
    att.put.nc(new_nc, variable = 'b30cm', type = 'NC_CHAR', name = 'short_name', value = 'b30cm')
    att.put.nc(new_nc, variable = 'b30cm', type = 'NC_CHAR', name = 'units', value = 'g C m^-2')
    att.put.nc(new_nc, variable = 'b30cm', type = 'NC_FLOAT', name = 'missing_value', value = 1e20)
    var.put.nc(new_nc, 'b30cm', curr_b30)
    
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Created by Ryan Heneghan, with help from Simon Jennings <ryan.heneghan@gmail.com>")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "institution", type = "NC_CHAR", value = "Universitat Autonoma de Barcelona")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "date_created", type = "NC_CHAR", value = date())
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "comments", type = "NC_CHAR", value = "Impact model output for ISIMIP2b and FishMIP NPPvSST experimental protocol")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "length-weight_conversion", type = "NC_CHAR", value = 'esd = 2*(3/(4*pi)*weight)^(1/3)')
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "ph_input_used", type = "NC_CHAR", value = "no")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "diazotroph_input_used", type = "NC_CHAR", value = "implicity, as part of integrated phytoplankton production")
    att.put.nc(new_nc, variable = "NC_GLOBAL", name = "wet-weight to carbon conversion", type = "NC_CHAR", value = "0.0352, from Weibe Fish Bull 73(4)")
    
    
    close.nc(new_nc)
  }
  
  ## EXPORT PROPORTION FOR PRIMARY PRODUCTION
  curr_bioms <- curr_file$V15
  
  new_bioms <- rep(NA, 16264800)
  new_bioms[input_dat] <- curr_bioms
  
  array_biom <- array(new_bioms, dim=c(360,180,251))
  
  curr_name3 <- nc_names3[i]

  new_nc <- create.nc(curr_name3)
  
  dim.def.nc(new_nc, 'lon', dimlength = 360)
  var.def.nc(new_nc, 'lon', 'NC_FLOAT', 'lon')
  var.put.nc(new_nc, 'lon', c(0:359))
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'long_name', value = 'longitude')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'units', value = 'degrees')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR',  name = 'axis', value = 'X')
  
  dim.def.nc(new_nc, 'lat', dimlength = 180)
  var.def.nc(new_nc, 'lat', 'NC_FLOAT', 'lat')
  var.put.nc(new_nc, 'lat', c(-89.5:89.5))
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'long_name', value = 'latitude')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'units', value = 'degrees_north')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'axis', value = 'Y')
  
  dim.def.nc(new_nc, 'time', unlim = TRUE)
  var.def.nc(new_nc, 'time', 'NC_DOUBLE', 'time')
  var.put.nc(new_nc, 'time', 0:(250)) # Fill time values
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'long_name', value = 'time')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'units', value = 'years since 1850-01-15 00:00:00')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'calendar', value = 'standard')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'axis', value = 'T')
  
  var.def.nc(new_nc, 'tau_r', "NC_FLOAT", c('lon', 'lat', 'time'))
  att.put.nc(new_nc, variable = 'tau_r', type = 'NC_CHAR', name = 'long_name', value = 'Proportion of primary production exported from system')
  att.put.nc(new_nc, variable = 'tau_r', type = 'NC_CHAR', name = 'short_name', value = 'tau_r')
  att.put.nc(new_nc, variable = 'tau_r', type = 'NC_CHAR', name = 'units', value = 'NA')
  att.put.nc(new_nc, variable = 'tau_r', type = 'NC_FLOAT', name = 'missing_value', value = 1e20)
  var.put.nc(new_nc, 'tau_r', array_biom)
  
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Created by Ryan Heneghan, with help from Simon Jennings <ryan.heneghan@gmail.com>")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "institution", type = "NC_CHAR", value = "Universitat Autonoma de Barcelona")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "date_created", type = "NC_CHAR", value = date())
  
  
  close.nc(new_nc)
  

}
