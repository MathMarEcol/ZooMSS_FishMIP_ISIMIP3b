library(tidyverse)
library(ncdf4)
library(lubridate)
# After running this, files are to be uploaded to:
# /work/bb0820/ISIMIP/ISIMIP3b/UploadArea/marine-fishery_global/ZooMSS/_tmp

# <model>_<climate-forcing>_<bias-adjustment>_<climate-scenario>_<soc-scenario>_<sens-scenario>_<variable>_<global>_<timestep>_<start-year>_<end-year>.nc

# macroecological_ipsl-cm6a-lr_nobc_historical_nosoc_default_tcb_global_annual_1950_2014.nc
# macroecological_ipsl-cm6a-lr_nobc_ssp126_nosoc_default_tcb_global_annual_2015-2100.nc

base_dir <- paste0("~",.Platform$file.sep,
                   "Nextcloud",.Platform$file.sep,
                   "MME2Work",.Platform$file.sep,
                   "FishMIP",.Platform$file.sep,
                   "Phase1")

o_Model <- "ZooMSS"
o_Forcing <- c("ipsl-cm6a-lr", "gfdl-esm4")
o_Bias <- "nobc"
o_Scenario <- c("historical", "picontrol", "ssp126", "ssp585")
o_Soc <- "nat"
o_Sens <- "default"
o_Variable <- c("tpb", "tcb", "bp30cm", "bp30to90cm", "bp90cm", "tcblog10")
o_Region <- "global"
o_TempRes <- "annual"


for (m in 1:length(o_Forcing)){

  for (s in 1:length(o_Scenario)){

    file <- list.files(path = paste0(base_dir, .Platform$file.sep, "Output"), pattern = paste0(o_Forcing[m], "_", o_Scenario[s]), full.names = TRUE)

    for (f in 1:length(file)){

      dat <- read_rds(file[f])

      dat$days <- as.numeric(dat$time - as_date("1850-01-01"))

      if (str_detect(o_Forcing[m], "ipsl")){
        lon <- -180:179
        lat <- -89.5:89.5
      } else{ # GFDL
        lon <- -179.5:179.5
        lat <- -89.5:89.5
      }

      days <- sort(unique(dat$days))

      full_res <- crossing(lon, lat, days)

      out <- left_join(full_res, dat, by = c("lon", "lat", "days")) %>%
        arrange(days, lat, lon)

      rm(full_res)

      # Convert to lon, lat, time grid
      tcb <- tibble(array = array(out$tcb, dim=c(length(lon), length(lat), length(days))),
                    name = "TOTAL consumer biomass density",
                    units = "g m-2")

      tpb <- tibble(array = array(out$tpb, dim=c(length(lon), length(lat), length(days))),
                    name = "TOTAL pelagic biomass density",
                    units = "g m-2")

      bp30cm <- tibble(array = array(out$bp30cm, dim=c(length(lon), length(lat), length(days))),
                       name = "Biomass density of small pelagics <30cm",
                       units = "g m-2")

      bp30to90cm <- tibble(array = array(out$bp30to90cm, dim=c(length(lon), length(lat), length(days))),
                           name = "Biomass density of medium pelagics >=30cm and <90cm",
                           units = "g m-2")

      bp90cm <- tibble(array = array(out$bp90cm, dim=c(length(lon), length(lat), length(days))),
                       name = "Biomass density of large pelagics >=90cm",
                       units = "g m-2")

      # # This will need some more coding to get multiple layers in the nc file

      tcblog10 <- tibble(array = array(c(out$tcblog10_0, out$tcblog10_1, out$tcblog10_2, out$tcblog10_3, out$tcblog10_4, out$tcblog10_5),
                                       dim=c(length(lon), length(lat), length(days), 6)),
                         name = "TOTAL consumer biomass density in log10 weight bins",
                         units = "g m-2")

      rm(out)

      # define dimensions
      londim <- ncdim_def("lon", "degrees", as.double(lon))
      latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
      timedim <- ncdim_def("time", "days since 1850-01-01 00:00:00", as.double(days), unlim = TRUE, calendar = "standard")

      vardim <- ncdim_def("size_class", "g", c(1, 10, 100, 1000, 10000, 100000))

      # define variables
      fillvalue <- 1e20

      for (v in 1:length(o_Variable)){

        o_file <- paste0(base_dir, .Platform$file.sep, "Output", .Platform$file.sep,
                         o_Model, "_", o_Forcing[m], "_", o_Bias, "_",
                         o_Scenario[s], "_", o_Soc, "_", o_Sens, "_",
                         o_Variable[v], "_", o_Region, "_", o_TempRes, "_",
                         min(year(dat$time)), "-", max(year(dat$time)),".nc")

        var <- eval(parse(text = o_Variable[v]))

        if (str_detect(o_Variable[v], "tcblog10")){
          def = ncvar_def(name = o_Variable[v], units = var$units, dim = list(londim, latdim, timedim, vardim), missval = fillvalue, longname = var$name, prec="double")
        } else{
          def = ncvar_def(name = o_Variable[v], units = var$units, dim = list(londim, latdim, timedim), missval = fillvalue, longname = var$name, prec="double")
        }

        # create netCDF
        ncout <- nc_create(o_file, def, force_v4=TRUE)

        # put variables
        ncvar_put(ncout, def, var$array)

        # put additional attributes into dimension and data variables
        ncatt_put(ncout,"lon","axis","X")
        ncatt_put(ncout,"lon","long_name", "longitude")
        ncatt_put(ncout,"lat","axis","Y")
        ncatt_put(ncout,"lat","long_name", "latitude")
        ncatt_put(ncout,"time","axis","T")

        # add global attributes
        ncatt_put(ncout, 0, "author", "Created by Jason Everett <Jason.Everett@uq.edu.au>")
        ncatt_put(ncout, 0, "institution", "University of Queensland, Australia")
        ncatt_put(ncout, 0, "date_created", now())
        ncatt_put(ncout, 0, "comments", "ZooMSS model output for ISIMIP2b and FishMIP Phase 1 experimental protocol")
        # ncatt_put(ncout, 0, "ph_input_used", "no")
        # ncatt_put(ncout, 0, "diazotroph_input_used", "only if included with integrated phytoplankton biomass")
        # ncatt_put(ncout, 0, "wet-weight to carbon conversion", "as per Heneghan et al. 2020 Ecological Modelling")
        ncatt_put(ncout, 0, "Assumed Mixed Layer Depth", "60 m")
        ## Probably should include this but need to check the conversion
        # ncatt_put(ncout, 0, "length-weight_conversion", "esd = 2*(3/(4*pi)*weight)^(1/3)")

        # close the file, writing data to disk
        nc_close(ncout)
      }
    }
  }
}
