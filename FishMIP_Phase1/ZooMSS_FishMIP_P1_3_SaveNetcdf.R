library(tidyverse)
library(ncdf4)
library(lubridate)
# After running this, files are to be uploaded to:
# /work/bb0820/ISIMIP/ISIMIP2b/UploadArea/marine-fishery_global/ZooMSS/_tmp


# For Derek’s paper: /work/bb0820/ISIMIP/ISIMIP3b/UploadArea/marine-fishery_global/ZooMSS/_tmp

out_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "Output",.Platform$file.sep)

# Output data naming conventions for Runs 1,2 and 3 (preindustrial, historical and rcp85)
o_Model <- "ZooMSS"
o_Forcing <- "cesm1-bgc"
o_Bias <- "nobc"
o_Scenario <- c("pre-industrial", "historical", "rcp85", "npp-control", "temperature-control")
o_Fishing <- "nosoc"
o_CO2_Scenario <- "co2"
o_Variable <- c("tsb", "tcb", "b10cm", "b30cm")
o_Region <- "global"
o_TempRes <- "monthly"

#Example: apecosm_cesm1-bgc_nobc_historical_nosoc_co2_b10cm_global_monthly_1860-1869.nc4

files <- c(paste0(out_dir,"CESM_pi_withZooMSS.rds"),
           paste0(out_dir,"CESM_hist_withZooMSS.rds"),
           paste0(out_dir,"CESM_rcp85_withZooMSS.rds"),
           paste0(out_dir,"CESM_nppControl_withZooMSS.rds"),
           paste0(out_dir,"CESM_tempControl_withZooMSS.rds"))

for (f in 1:length(files)){

  dat <- read_rds(files[f])
  dat$days <- as.numeric(dat$time - as_date("1850-01-01"))

  lon <- 0:359
  lat <- -89.5:89.5
  days <- sort(unique(dat$days))

  full_res <- crossing(lon, lat, days) %>%
    add_column(Bio = NA)

  out <- left_join(full_res, dat, by = c("lon", "lat", "days")) %>%
    arrange(days, lat, lon)

  rm(full_res)

  # Convert to lon, lat, time grid
  tsb <- tibble(array = array(out$tsb, dim=c(length(lon),length(lat),length(days))),
                name = "Total system carbon biomass",
                units = "g C m-2")

  tcb <- tibble(array = array(out$tcb, dim=c(length(lon),length(lat),length(days))),
                name = "Total consumer carbon biomass",
                units = "g C m-2")

  b10cm <- tibble(array = array(out$b10cm, dim=c(length(lon),length(lat),length(days))),
                name = "Carbon biomass density of consumers > 10cm",
                units = "g C m-2")

  b30cm <- tibble(array = array(out$b30cm, dim=c(length(lon),length(lat),length(days))),
                  name = "Carbon biomass density of consumers > 30cm",
                  units = "g C m-2")

  rm(out)
  # filled.contour(lon, lat, array_b30cm[,,1])

  # define dimensions
  londim <- ncdim_def("lon", "degrees", as.double(lon))
  latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
  timedim <- ncdim_def("time", "days since 1850-01-01 00:00:00", as.double(days), unlim = TRUE, calendar = "standard")

  # define variables
  fillvalue <- 1e32

  for (v in 1:length(o_Variable)){

    o_file <- paste0(out_dir, o_Model, "_", o_Forcing, "_", o_Bias, "_",
                     o_Scenario[f], "_", o_Fishing, "_", o_CO2_Scenario, "_",
                     o_Variable[v], "_", o_Region, "_", o_TempRes, "_",
                     min(year(dat$time)), "-", max(year(dat$time)),".nc4")

    var <- eval(parse(text = o_Variable[v]))
    def = ncvar_def(o_Variable[v], var$units, list(londim, latdim, timedim), fillvalue, var$name, prec="double")

    # create netCDF file and put arrays
    ncout <- nc_create(o_file, def, force_v4=TRUE)

    # put variables
    ncvar_put(ncout, def, var$array)

    # put additional attributes into dimension and data variables
    ncatt_put(ncout,"lon","axis","X")
    ncatt_put(ncout,"lon","long_name", "longitude")
    ncatt_put(ncout,"lat","axis","Y")
    ncatt_put(ncout,"lat","long_name", "latitude")
    ncatt_put(ncout,"time","axis","T")
    # ncatt_put(ncout,"time","calendar", "standard")

    # add global attributes
    ncatt_put(ncout, 0, "author", "Created by Jason Everett <Jason.Everett@uq.edu.au>")
    ncatt_put(ncout, 0, "institution", "University of Queensland, Australia")
    ncatt_put(ncout, 0, "date_created", now())
    ncatt_put(ncout, 0, "comments", "ZooMSS model output for ISIMIP2b and FishMIP NPPvSST experimental protocol")
    ncatt_put(ncout, 0, "ph_input_used", "no")
    ncatt_put(ncout, 0, "diazotroph_input_used", "only if included with integrated phytoplankton biomass")
    ncatt_put(ncout, 0, "wet-weight to carbon conversion", "as per Heneghan et al. 2020 Ecological Modelling")
    ncatt_put(ncout, 0, "Assumed Mixed Layer Depth", "60 m")
    ## Probably should include this but need to check the conversion
    # ncatt_put(ncout, 0, "length-weight_conversion", "esd = 2*(3/(4*pi)*weight)^(1/3)")

    # close the file, writing data to disk
    nc_close(ncout)
  }
}
