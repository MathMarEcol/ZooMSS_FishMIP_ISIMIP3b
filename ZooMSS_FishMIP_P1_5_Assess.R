library(tidyverse)
library(raster)
library(ncdf4)
library(tidync)
library(lubridate)
library(PCICt)


# Check the raw input files
r <- raster("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_195001-196912.nc", level = 1)
s <- stack("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_195001-196912.nc")
n <- nc_open("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_195001-196912.nc")

r2 <- raster("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc", level = 1)
s2 <- stack("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc")
n2 <- nc_open("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc")



graphics.off()

plot(log10(r*1e6),
     main = "1950 GFDL (mg Chl m-3)",
     sub = "chl_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_195001-196912.nc")

plot(log10(r2*1e6),
     main = "1950 IPSL (mg Chl m-3)",
     sub = "chl_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc",
     xlim = c(0, 360))

 # Outcome: There seems to be problems with IPSL and its Chl. As well as the IPSL gridding.


## Now lets check it with tidync which seems to work pretty well
n <- hyper_tibble("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_GFDL-ESM4_historical_r1i1p1f1_gr_195001-196912.nc", lev = lev < 5, time = time < 36850)
n <- n %>%
  mutate(chl = chl * 1e6) # Convert to mg m-3
min(n$chl)
max(n$chl)
# > min(n$chl)
# [1] 2.375023e-05
# > max(n$chl)
# [1] 13.69247


# ncvar_get(nc_open("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc"), "time")

n2 <- hyper_tibble("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/Test/chl_Omon_IPSL-CM6A-LR_historical_r1i1p1f1_gn_195001-201412.nc", olevel = olevel < 5, time = time < 36874)
n2 <- n2 %>%
  mutate(chl = chl * 1e6) # Convert to mg m-3
min(n2$chl)
max(n2$chl)
# > min(n2$chl)
# [1] 7.176231
# > max(n2$chl)
# [1] 13776.02





# Check the intermediate match up data
dat <- readRDS("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ipsl-cm6a-lr_picontrol_tos_onedeg_global_annual_1950_2014_withZooMSS.rds")
ggplot(data = dat, aes(x = Chl_log10, y = tcb)) +
  geom_point()







# Now check the final data products.
rg <- raster("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_gfdl-esm4_r1i1p1f1_nobc_historical_nat_co2_tcb_global_annual_1850-2014.nc4")
plot(log10(rg)) # Looks ok, apart form scale issues

ri <- raster("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_ipsl-cm6a-lr_r1i1p1f1_nobc_historical_nat_co2_tcb_global_annual_1850-2014.nc4")
plot(ri) # Values are too consistent - Chl is too high.



# Now lets get to the bottom of scale issues by checking against Stock et al 2020

# dat <- read_rds("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Input/gfdl-esm4_r1i1p1f1_historical_onedeg_global_annual_1850_2014.rds")
#
# dat <- dat %>%
#   filter(time>="1995-01-01")

nc <- nc_open("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/gfdl-esm4_r1i1p1f1_historical_chl_onedeg_global_annual_1850_2014.nc")
ncvar_get(nc, "lev")


nc <- hyper_tibble("/Users/jason/Nextcloud/MME1Data/FishMIP_Phase1_Forcings/gfdl-esm4_r1i1p1f1_historical_chl_onedeg_global_annual_1850_2014.nc", lev = lev < 5)

time <- as.PCICt((nc$time/12)*365*86400, cal="365_day", origin = "1601-01-01 00:00:00", tz = "UTC")
nc$time <- round_date(as_date(as.character(time)), unit = "year") # Round to year

nc <- nc %>%
  filter(time>="1995-01-01") %>%
  mutate(lon = round(lon,1), # Some lon/lat don't match up for some reason. Rounding them seems to fix it.
         lat = round(lat,1),
         chl = chl * 1e6,
         Chl_log10 = log10(chl))

nc_mn <- nc %>%
  group_by(lat, lon) %>%
  summarise(Chl_log10 = mean(Chl_log10),
            .groups = "keep")


ggplot(data = nc_mn, aes(x = lon, y = lat, colour = Chl_log10)) + geom_point(size = 0.1) +
  scale_colour_distiller(palette = "YlOrRd",
                         direction = 1,
                         limits = round(c(log10(0.01), log10(3)),2),
                         breaks = round(log10(c(0.01, 0.03, 0.1, 0.3, 1, 3)), 2),
                         labels = c(0.01, 0.03, 0.1, 0.3, 1, 3))


nc_mn2 <- nc_mn %>%
  mutate(chl = chl/1e3,
         Chl_log10 = log10(chl))

nc_mn2 <- nc %>%
  mutate(chl = chl/1e3,
         Chl_log10 = log10(chl)) %>%
  group_by(lat, lon) %>%
  summarise(Chl_log10 = mean(Chl_log10),
            .groups = "keep")

ggplot(data = nc_mn2, aes(x = lon, y = lat, colour = Chl_log10)) + geom_point(size = 0.1) +
  scale_colour_distiller(palette = "YlOrRd",
                         direction = 1,
                         limits = round(c(log10(0.01), log10(3)),2),
                         breaks = round(log10(c(0.01, 0.03, 0.1, 0.3, 1, 3)), 2),
                         labels = c(0.01, 0.03, 0.1, 0.3, 1, 3))







library(tidyverse)

source("fZooMSS_Xtras.R")

out_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "Output",.Platform$file.sep)

model <- c("pi", "hist", "rcp85")

# There are 2 things to do:
# 1. Work out how far away the data is from what we wanted. (EuclideanDist)
# 2. Work out what values were used within the matrix run (cellID)

# for (m in 1:length(model)){

m <- 3

nc <- read_rds(paste0(out_dir,"CESM_",model[m],"_withZooMSS.rds")) # Save to RDM


nc_round <- nc %>%
  mutate(SST = round(SST,1),
         Chl_log10 = round(Chl_log10,2))

ggplot() + geom_histogram(data = nc_round, mapping = aes(x = Chl_log10), breaks = seq(-3,2,0.1))

ggplot(data = nc_round, mapping = aes(x = Chl_log10, y = SST)) + geom_point()


# Summarise Euclidean Dist

nc2 <- nc %>%
  group_by(Chl_log10_ZooMSS, SST_ZooMSS) %>%
  summarise(EuclideanDist = mean(EuclideanDist))


nc3 <- nc2 %>%
  filter(EuclideanDist > 0.01)

ggplot(data = nc2, mapping = aes(x = Chl_log10, y = EuclideanDist)) + geom_point()

hist(nc2$EuclideanDist)

ggplot(data = nc2, mapping = aes(x = Chl_log10_ZooMSS, y = SST_ZooMSS, colour = EuclideanDist, fill = EuclideanDist)) + geom_point()



# }
