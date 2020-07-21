
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

for (m in 1:length(model)){

  nc <- read_rds(paste0(out_dir,"CESM_",model[m],"_withZooMSS.rds")) # Save to RDM

}