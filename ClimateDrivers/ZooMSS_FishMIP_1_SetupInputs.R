library(tidync)
library(lubridate)
library(tidyverse)
library(PCICt)

raw_data <- "../Input/raw/"
proc_data <- "../Input/processed/"

var = {}

models <- {}

# phy
# Size:       360x180x120
# Dimensions: lon,lat,time
# Datatype:   double
# Attributes:
#   units         = 'mmol/m^2'
# _FillValue    = NaN
# missing_value = NaN

# time
# Size:       120x1
# Dimensions: time
# Datatype:   double
# Attributes:
#   standard_name = 'time'
# long_name     = 'time'
# units         = 'days since 1661-1-1 00:00:00'
# calendar      = '365_day'
# axis          = 'T'


temp = sort(list.files(path = "/Users/jason/GitHub/ZooMSS_FishMIP/Input/raw/hist",
                  pattern = "cesm_hist_phy_zint_monthly_185001*",
                  full.names = TRUE))
nc <- bind_rows(map_df(temp, function(x) hyper_tibble(x)))

nc2 <- data.frame(time = unique(nc$time))


q <- as.PCICt(as.POSIXlt(nc2$time*86400), cal="365_day", origin = "1661-01-01 00:00:00", tz = "UTC")

yrs <- as.numeric(unlist(str_extract_all(f,"\\d+"))) # retrieve start and end year from filename

start_date <- yrs[1]


nc <- hyper_tibble(f)

t <- unique(nc$time)
