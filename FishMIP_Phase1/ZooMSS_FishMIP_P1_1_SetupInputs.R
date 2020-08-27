library(tidync)
library(tidyverse)
library(PCICt)
library(lubridate)

in_dir <- paste0("~",.Platform$file.sep,
                 "Nextcloud",.Platform$file.sep,
                 "MME1Data",.Platform$file.sep,
                 "FishMIP_Phase1_Forcings")

out_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "Phase1",.Platform$file.sep,
                  "Input")

model <- c("gfdl", "ipsl")

experiment <- c("pi", "hist", "ssp126", "ssp585")

for (m in 1:length(model)){
  for (x in 1:length(experiment)){

    file_p = sort(list.files(path = in_dir, pattern = glob2rx(paste0("*",model[m],"*",experiment[x],"*chl*", "*annual*")), full.names = FALSE))
    file_s = sort(list.files(path = in_dir, pattern = glob2rx(paste0("*",model[m],"*",experiment[x],"*tos*", "*annual*")), full.names = FALSE))

    # Load files into tibble
    if (str_detect(model[m], "gfdl")){
      nc <- hyper_tibble(paste0(in_dir,.Platform$file.sep, file_p), lev = index ==1) %>% # Load phytoplankton files
        # rename(Depth = lev)
        select(-lev)
      }

    if (str_detect(model[m], "ipsl")){
      nc <- hyper_tibble(paste0(in_dir,.Platform$file.sep, file_p), olevel = index ==1) %>% # Load phytoplankton files
        # rename(Depth = olevel)
        select(-olevel)
    }

    nc2 <- hyper_tibble(paste0(in_dir,.Platform$file.sep, file_s)) %>% # Load temperature files
      dplyr::select("tos")
    nc <- bind_cols(nc, nc2) # Join temp and phyto dataframes

    nc <- nc %>%
      mutate(lon = round(lon,1), # Some lon/lat don't match up for some reason. Rounding them seems to fix it.
             lat = round(lat,1))

    rm(nc2)

    if (str_detect(model[m], "ipsl")){
      nc$lon <- nc$lon + 0.5
    }

    # Process time with PCICt due to 365 day years
    time <- as.PCICt((nc$time/12)*365*86400, cal="365_day", origin = "1601-01-01 00:00:00", tz = "UTC")
    nc$time <- round_date(as_date(as.character(time)), unit = "year") # Round to year

    nc <- nc %>%
      mutate(chl = chl * 1e6,
             Chl_log10 = log10(chl)) %>%
      rename("SST" = tos) %>% # Clean up
      select(-chl)

    file <- str_replace(file_s, "tos_", "") # Remove variable details
    file <- str_replace(file, ".nc", ".rds")
    write_rds(nc, paste0(out_dir,.Platform$file.sep,file)) # Save to RDM

    rm(nc, file_p, file_s, file, time)
  }
}


## Convert Phyto to Chl using ZooMSS calc: from Maranon et al. 2014
# Phyto is in units of "mmol/m^2"
# / 1000 # to get to moles m-2
# / 75 # m MLD to moles m-3
# * 12.0107 # mol:g to g m-3
# * 1000 # to mg m-3

