
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
