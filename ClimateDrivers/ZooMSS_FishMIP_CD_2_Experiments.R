library(tidyverse)
library(yaImpute)

source("fZooMSS_Xtras.R")

out_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "Output",.Platform$file.sep)

#### PROPOSED MODEL RUNS ####
# 1. Pre-industrial: All forcings pre-industrial (1860-2100)
# 2. Historical: All forcings historical (1860-2005)
# 3. RCP85: All forcings RCP8.5 (2006-2100)
# 4. Change npp (temperature control): All forcings pre-industrial (1860-2100) except for
#       NPP. For NPP use historical and rcp85 forcings one after the other (run historical
#       1860-2005, then RCP8.5 2006-2100)
# 5. Change temperature (NPP control): All forcings pre-industrial (1860-2100) except for
#       temperature. For temperature use historical and rcp85 forcings one after the other
#       (run historical 1860-2005, then RCP8.5 2006-2100)


# Total system carbon biomass (tsb): gCm-2 All primary producers and consumers
# Total consumer carbon biomass density (tcb): gCm-2 All consumers (trophic level >1, vertebrates and invertebrates)
# Carbon biomass density of consumers > 10cm (b10cm): gCm-2 If asymptotic length (Linf) is > 10cm, include in > 10cm class
# Carbon biomass density of consumers > 30cm (b30cm): gCm-2 If asymptotic length (Linf) is > 30cm, include in > 30cm class

#### Conversions for Length to Weight ####
#convert FishMIP length thresholds to weight thresholds, using (wet weight)=0.01(length)^3
weight10 <- 10^round(log10(10^(1/3)*100),1) #using Weight (g) = 0.01 * length^3, length is in cm
weight30 <- 10^round(log10(30^(1/3)*100),1)

#### Mixed Layer Depth ####
MLD <- 60

#### Load ZooMSS Matrix Data ####
enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/enviro_Matrix.RDS")
res <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/Output/res_20200526_TheMatrix.RDS" )
temp <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/20200526_TheMatrix_000001.RDS")


w <- temp$model$param$w
carbon <- temp$model$param$Groups$Carbon
Bio <- fZooMSS_CarbonBiomass(res, w, carbon) # Convert to carbon biomass
Bio <- fZooMSS_SumSpecies(Bio) # Sum the species
rm(res)

Bio_df <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T)) %>%
  mutate(cellID = 1:n()) %>% # Create a cellID
  pivot_longer(cols = starts_with("V"), names_to = "SizeClass", values_to = "Biomass") %>%
  mutate(Biomass = Biomass * MLD, #Convert biomass to m-2 by * MLD
         Weight = rep(w, times = length(Bio))) %>% # Make sure weight class is on every row
  # select(-SizeClass) %>%
  filter(Weight <= 100001) %>% # Remove very large stuff (100 kg)
  # mutate(BiomassC = Biomass * 0.1) %>% # convert to carbon biomass
  add_column(tcb = 1, b10cm = 1, b30cm = 1) %>% # Create column of onces
  mutate(b10cm = replace(b10cm, Weight < weight10, 0), # Replace 1 with zero for rows outside weight range
         b30cm = replace(b30cm, Weight < weight30, 0), # Replace 1 with zero for rows outside weight range
         tcb = tcb * Biomass, # All consumers is simply biomass
         b10cm = b10cm * Biomass, # Multiply weight class switch (0,1) by Biomass
         b30cm = b30cm * Biomass)  # Multiply weight class switch (0,1) by Biomass

Bio_sum <- Bio_df %>%
  group_by(cellID) %>%
  summarise(tcb = sum(tcb),
            b10cm = sum(b10cm),
            b30cm = sum(b30cm),
            .groups = "keep") %>%
  ungroup() %>%
  left_join(select(enviro_data, cellID, chlo, sst), by = "cellID") %>%
  rename(SST = sst, Chl = chlo) %>%
  mutate(Chl_log10 = log10(Chl),
         Chl_C_mg_m3 = 10^(0.89 * Chl_log10 + 1.79), #Convert Chl to Carbon using Maranon et al. 2014
         Chl_C_g_m2 = Chl_C_mg_m3/1000/MLD,
         tsb = tcb + Chl_C_g_m2)

rm(Bio, Bio_df, enviro_data)

#### Get each of the 3 models and match to ZooMSS ####
# 1. Pre-industrial: All forcings pre-industrial (1860-2100)
# 2. Historical: All forcings historical (1860-2005)
# 3. RCP85: All forcings RCP8.5 (2006-2100)
# 4. Change npp (temperature control): All forcings pre-industrial (1860-2100) except for
#       NPP. For NPP use historical and rcp85 forcings one after the other (run historical
#       1860-2005, then RCP8.5 2006-2100)
# 5. Change temperature (NPP control): All forcings pre-industrial (1860-2100) except for
#       temperature. For temperature use historical and rcp85 forcings one after the other
#       (run historical 1860-2005, then RCP8.5 2006-2100)
model <- c("pi", "hist", "rcp85")

for (m in 1:length(model)){

  nc <- read_rds(paste0(out_dir,"CESM_",model[m],".rds"))

  out <- ann(as.matrix(Bio_sum[,c("SST", "Chl_log10")]),
             as.matrix(nc[,c("SST", "Chl_log10")]),
             k = 1, verbose = FALSE)

  nc <- nc %>%
    mutate(cellID = out$knnIndexDist[,1],
           EuclideanDist = out$knnIndexDist[,2],
           tsb = Bio_sum$tsb[cellID],
           tcb = Bio_sum$tcb[cellID],
           b10cm = Bio_sum$b10cm[cellID],
           b30cm = Bio_sum$b30cm[cellID],
           Chl_log10_ZooMSS = Bio_sum$Chl_log10[cellID],
           SST_ZooMSS = Bio_sum$SST[cellID])

  write_rds(nc, paste0(out_dir,"CESM_",model[m],"_withZooMSS.rds")) # Save to RDM

  assign(paste(model[m]),nc)

  rm(nc, out)

}

#### Revert to original files and setup the experiments ####
hist <- hist %>%
  select(c(lat, lon, time, SST, Chl_log10)) %>%
  filter(time < as.POSIXct("2006-01-01"))

rcp85 <- rcp85 %>%
  select(c(lat, lon, time, SST, Chl_log10)) %>%
  filter(time >= as.POSIXct("2006-01-01"))

#### # 4. Change npp (temperature control) ####
# All forcings pre-industrial (1860-2100) except for
# NPP. For NPP use historical and rcp85 forcings one
# after the other (run historical 1860-2005, then
# RCP8.5 2006-2100)

tempControl <- pi %>%
  select(c(lat, lon, time, SST)) %>%
  mutate(Chl_log10 = c(hist$Chl_log10, rcp85$Chl_log10))

out <- ann(as.matrix(Bio_sum[,c("SST", "Chl_log10")]),
           as.matrix(tempControl[,c("SST", "Chl_log10")]),
           k = 1, verbose = FALSE)

tempControl <- tempControl %>%
  mutate(cellID = out$knnIndexDist[,1],
         EuclideanDist = out$knnIndexDist[,2],
         tsb = Bio_sum$tsb[cellID],
         tcb = Bio_sum$tcb[cellID],
         b10cm = Bio_sum$b10cm[cellID],
         b30cm = Bio_sum$b30cm[cellID],
         Chl_log10_ZooMSS = Bio_sum$Chl_log10[cellID],
         SST_ZooMSS = Bio_sum$SST[cellID])

write_rds(tempControl, paste0(out_dir,"CESM_tempControl_withZooMSS.rds")) # Save to RDM

rm(tempControl, out)

#### 5. Change temperature (NPP control) ####
# All forcings pre-industrial (1860-2100) except for
# temperature. For temperature use historical and
# rcp85 forcings one after the other (run historical
# 1860-2005, then RCP8.5 2006-2100)

nppControl <- pi %>%
  select(c(lat, lon, time, Chl_log10)) %>%
  mutate(SST = c(hist$SST, rcp85$SST))

out <- ann(as.matrix(Bio_sum[,c("SST", "Chl_log10")]),
           as.matrix(nppControl[,c("SST", "Chl_log10")]),
           k = 1, verbose = FALSE)

nppControl <- nppControl %>%
  mutate(cellID = out$knnIndexDist[,1],
         EuclideanDist = out$knnIndexDist[,2],
         tsb = Bio_sum$tsb[cellID],
         tcb = Bio_sum$tcb[cellID],
         b10cm = Bio_sum$b10cm[cellID],
         b30cm = Bio_sum$b30cm[cellID],
         Chl_log10_ZooMSS = Bio_sum$Chl_log10[cellID],
         SST_ZooMSS = Bio_sum$SST[cellID])

write_rds(nppControl, paste0(out_dir,"CESM_nppControl_withZooMSS.rds")) # Save to RDM

rm(nppControl, out)

