library(tidync)
library(tidyverse)
library(PCICt)
library(lubridate)

base_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep)

model <- c("pi", "hist", "rcp85")

for (m in 1:length(model)){
  file_p = sort(list.files(path = paste0(base_dir,"InputFiles",
                                         .Platform$file.sep,"raw",
                                         .Platform$file.sep,model[m]),
                           pattern = paste0("cesm_",model[m],"_phy*"), full.names = TRUE))
  file_s = sort(list.files(path = paste0(base_dir,"InputFiles",
                                         .Platform$file.sep, "raw",
                                         .Platform$file.sep, model[m]),
                           pattern = paste0("cesm_",model[m],"_to*"), full.names = TRUE))

  # Load files into tibble
  nc <- bind_rows(map_df(file_p, function(x) hyper_tibble(x))) # Load phytoplankton files
  nc2 <- bind_rows(map_df(file_s, function(x) hyper_tibble(x))) %>% # Load temperature files
    select("to")
  nc <- bind_cols(nc, nc2) # Join temp and phyto dataframes
  rm(nc2)

  # Process time with PCICt due to 365 day years
  if (str_detect(model[m], "pi")){
    time <- as.PCICt((nc$time-31 + 401500)*86400, cal="365_day", origin = "1661-01-01 00:00:00", tz = "UTC") # Something wrong with time in pi
  }else{
    time <- as.PCICt((nc$time-31)*86400, cal="365_day", origin = "1661-01-01 00:00:00", tz = "UTC") # Other models use same origin
  }
  nc$time <- round_date(as_date(as.character(time)), unit = "month") + 14 # Convert to lubridate time and round to 15th of month.

  nc <- nc %>%
    # filter(time >= ymd("1860-01-01")) %>% # For FishMIP we don't need earlier dates
    mutate(Chl_log10 = (log10(phy /1000 / 75 * 12.0107 * 1000) -1.79) / 0.89) %>% # Convert to Chl
    rename("SST" = to) %>% # Clean up
    select(-phy)

  write_rds(nc, paste0(base_dir,"Output/CESM_",model[m],".rds")) # Save to RDM

  rm(nc, file_p, file_s, time)
}


## Convert Phyto to Chl using ZooMSS calc: from Maranon et al. 2014
# Phyto is in units of "mmol/m^2"
# / 1000 # to get to moles m-2
# / 75 # m MLD to moles m-3
# * 12.0107 # mol:g to g m-3
# * 1000 # to mg m-3

