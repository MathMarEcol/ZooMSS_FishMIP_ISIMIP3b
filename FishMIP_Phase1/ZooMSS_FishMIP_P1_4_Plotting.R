library(tidyverse)
library(tidync)
library(lubridate)
library(patchwork)
library(sf)
library(raster)
library(rnaturalearth)
library(gganimate) #devtools::install_github("thomasp85/gganimate")
library(transformr) #devtools::install_github("thomasp85/transformr")

out_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "Output",.Platform$file.sep)

files <- c(paste0(out_dir,"CESM_pi_withZooMSS.rds"),
           paste0(out_dir,"CESM_hist_withZooMSS.rds"),
           paste0(out_dir,"CESM_rcp85_withZooMSS.rds"),
           paste0(out_dir,"CESM_nppControl_withZooMSS.rds"),
           paste0(out_dir,"CESM_tempControl_withZooMSS.rds"))

##
mollCRS <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
mollCRS_no <- 54009

robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
robCRS_no <- 54030

lonlatCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
lonlatCRS_no <- 4326

# Download and process world outline
world <- ne_countries(scale = "medium", returnclass = "sf")
world_sf <- st_transform(world, crs = st_crs(mollCRS)) # Convert to Mollweide

##

nc1 <- hyper_tibble(paste0(out_dir, "ZooMSS_cesm1-bgc_nobc_historical_nosoc_co2_tcb_global_monthly_1850-2005.nc4"))
nc2 <- hyper_tibble(paste0(out_dir, "ZooMSS_cesm1-bgc_nobc_rcp85_nosoc_co2_tcb_global_monthly_2006-2100.nc4"))
nc <- bind_rows(nc1, nc2) %>%
  rename(tcb_all = tcb)
rm(nc1, nc2)

nc_no <- hyper_tibble(paste0(out_dir, "ZooMSS_cesm1-bgc_nobc_pre-industrial_nosoc_co2_tcb_global_monthly_1850-2100.nc4")) %>%
  dplyr::select(tcb) %>%
  rename(tcb_no = tcb)

nc_tempC <- hyper_tibble(paste0(out_dir, "ZooMSS_cesm1-bgc_nobc_temperature-control_nosoc_co2_tcb_global_monthly_1850-2100.nc4")) %>%
  dplyr::select(tcb) %>%
  rename(tcb_tempC = tcb)

nc_nppC <- hyper_tibble(paste0(out_dir, "ZooMSS_cesm1-bgc_nobc_npp-control_nosoc_co2_tcb_global_monthly_1850-2100.nc4")) %>%
  dplyr::select(tcb) %>%
  rename(tcb_nppC = tcb)

nc <- bind_cols(nc, nc_no, nc_tempC, nc_nppC) %>%
  mutate(year = year(as_date(time, origin = "1850-1-1"))) %>%
  filter(year >= 1960)

rm(nc_no, nc_nppC, nc_tempC)

## Calculate Annual Averages
nc_yr <- nc %>%
  group_by(year) %>%
  summarise(
    tcb_all = mean(tcb_all),
    tcb_no = mean(tcb_no),
    tcb_tempC = mean(tcb_tempC),
    tcb_nppC = mean(tcb_nppC),
    .groups = "keep") %>%
  ungroup() %>%
  # mutate(tcb_no = ((tcb_no-mean(tcb_no[1:10]))/mean(tcb_no[1:10])) + 1,
  #        tcb_all = ((tcb_all-mean(tcb_all[1:10]))/mean(tcb_all[1:10])) + 1,
  #        tcb_nppC = ((tcb_nppC-mean(tcb_nppC[1:10]))/mean(tcb_nppC[1:10])) + 1,
  #        tcb_tempC = ((tcb_tempC-mean(tcb_tempC[1:10]))/mean(tcb_tempC[1:10])) + 1) %>%
  mutate(tcb_no = (tcb_no/mean(tcb_no[1:10])),
         tcb_all = (tcb_all/mean(tcb_all[1:10])),
         tcb_nppC = (tcb_nppC/mean(tcb_nppC[1:10])),
         tcb_tempC = (tcb_tempC/mean(tcb_tempC[1:10]))) %>%
  filter(year >= 1970)

## Do difference plotting
gg_no <- ggplot(data = nc_yr, aes(x = year, y = tcb_no)) +
  geom_line(colour = "blue") +
  geom_hline(yintercept = 1) +
  ylab("Biomass Relative to 1960-1970") +
  ggtitle("No Change") +
  theme_bw() +
  ylim(0.8, 1.05)

gg_nppC <- ggplot(data = nc_yr, aes(x = year, y = tcb_nppC)) +
  geom_line(colour = "blue") +
  geom_hline(yintercept = 1) +
  ylab("Biomass Relative to 1960-1970") +
  ggtitle("Temp Change") +
  theme_bw() +
  ylim(0.8, 1.05)

gg_tempC <- ggplot(data = nc_yr, aes(x = year, y = tcb_tempC)) +
  geom_line(colour = "blue") +
  geom_hline(yintercept = 1) +
  ylab("Biomass Relative to 1960-1970") +
  ggtitle("NPP Change") +
  theme_bw() +
  ylim(0.8, 1.05)

gg_all <- ggplot(data = nc_yr, aes(x = year, y = tcb_all)) +
  geom_line(colour = "blue") +
  geom_hline(yintercept = 1) +
  ylab("Biomass Relative to 1960-1970") +
  ggtitle("All Change") +
  theme_bw() +
  ylim(0.8, 1.05)


graphics.off()
x11(width = 12, height = 8)
gg_no + gg_nppC + gg_tempC + gg_all
ggsave("Figures/ZooMSS_tcb_Change.pdf")


## Do spatial map

# Filter and summarise 60s
nc1 <- nc %>%
  mutate(year = year(as_date(time, origin = "1850-1-1"))) %>%
  filter(year <= 1970 & year >= 1960) %>%
  group_by(lat, lon) %>%
  summarise(tcb_no = mean(tcb_no),
            tcb_all = mean(tcb_all),
            tcb_tempC = mean(tcb_tempC),
            tcb_nppC = mean(tcb_nppC),
            .groups = "keep") %>%
  ungroup()

# Filter and summarise 2100
nc2 <- nc %>%
  mutate(year = year(as_date(time, origin = "1850-1-1"))) %>%
  filter(year >= 2090) %>%
  group_by(lat, lon) %>%
  summarise(tcb_no = mean(tcb_no),
            tcb_all = mean(tcb_all),
            tcb_tempC = mean(tcb_tempC),
            tcb_nppC = mean(tcb_nppC),
            .groups = "keep") %>%
  ungroup()

## Calculate the difference
nc_map <- nc1 %>%
  dplyr::select(lon, lat)

# nc_map$lon <- ifelse(nc_map$lon > 180, -360 + nc_map$lon, nc_map$lon) # Convert to -180:180

nc_map$tcb_no_diff <- ((nc2$tcb_no - nc1$tcb_no) / nc1$tcb_no) * 100
nc_map$tcb_all_diff <- ((nc2$tcb_all - nc1$tcb_all) / nc1$tcb_all) * 100
nc_map$tcb_tempC_diff <- ((nc2$tcb_tempC - nc1$tcb_tempC) / nc1$tcb_tempC) * 100
nc_map$tcb_nppC_diff <- ((nc2$tcb_nppC - nc1$tcb_nppC) / nc1$tcb_nppC) * 100

nc_map_raster <- rasterFromXYZ(nc_map, crs = lonlatCRS)  #Convert first two columns as lon-lat and third as value
nc_map_poly <- rasterToPolygons(nc_map_raster, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE) # Convert to polygon which is better for plotting
nc_map_sf <- st_as_sf(nc_map_poly)
# nc_map_sf <- st_as_sf(nc_map, coords = c("lon", "lat"), crs = lonlatCRS)
nc_map_rob <- st_transform(nc_map_sf, crs = st_crs(robCRS_no))
nc_map_moll <- st_transform(nc_map_sf, crs = st_crs(mollCRS_no))

var <- list("tcb_no_diff", "tcb_nppC_diff", "tcb_tempC_diff", "tcb_all_diff")
tit <- list("No Change", "Temperature Change", "NPP Change", "All Change")
gg_map <- list()
for (v in 1:4){
  gg_map[[v]] <- ggplot() +
    geom_sf(data = nc_map_sf, aes_string(fill = var[[v]]), colour = NA) +
    # geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient2(name = "Biomass Change (%)",
                         # direction = 1,
                         # limits = c(quantile(r0$layer, .05), quantile(data_moll_df$layer, .95)),
                         limits = c(-60, 60),
                         midpoint = 0,
                         low = "red",
                         mid = "white",
                         high = "blue",
                         # breaks = ticks,
                         # labels = clabel,
                         position = "right",
                         na.value = "grey80",
                         guide = "colourbar",
                         oob = scales::squish) +
    ggtitle(tit[[v]]) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) # +
    theme(legend.title = element_text(angle = -90)) +
    guides(fill = guide_colourbar(title.position = "right"))
}

graphics.off()
x11(width = 26, height = 4)
wrap_plots(gg_map, ncol = 4, guides = "collect")
ggsave("Figures/ZooMSS_tcb_MappedChange.pdf")




## Now do animated map

nc_wide <- nc %>%
  dplyr::select(lon, lat, year, tcb_all) %>%
  group_by(lon, lat, year) %>%
  summarise(
    tcb = mean(tcb_all)) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = tcb, names_prefix = "X")

nc_ani_raster <- rasterFromXYZ(nc_wide, crs = lonlatCRS)  #Convert first two columns as lon-lat and third as value
names(nc_ani_raster) <- unique(nc$year) # Names aren't preserved above

nc_ani_poly <- rasterToPolygons(nc_ani_raster, fun=NULL, n=4, na.rm=FALSE, digits=6, dissolve=TRUE) # Convert to polygon which is better for plotting
nc_ani_sf <- st_as_sf(nc_ani_poly)


nc_ani_sf_long <- nc_ani_sf %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "tcb") %>%
  mutate(year = str_replace(year, "X", ""),
         year = as.numeric(year)) %>%
  # filter(year < 1965) %>%
  st_as_sf()

nc_ani_sf_long_yr <- nc_ani_sf_long %>%
  filter(year == 1965)



## Do a test plot
gg_test <- ggplot() +
  geom_sf(data = nc_ani_sf_long_yr, aes(fill = log10(tcb), colour = log10(tcb))) +
  # geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  scale_fill_distiller(name = expression(paste("log"[10], " Biomass (g C m"^-2,")")),
                       palette = "OrRd",
                       direction = 1,
                       limits = c(0.5, 2),
                       position = "right",
                       na.value = "grey80",
                       guide = "colourbar",
                       oob = scales::squish) +
  scale_colour_distiller(name = expression(paste("log"[10], " Biomass (g C m"^-2,")")),
                       palette = "OrRd",
                       direction = 1,
                       limits = c(0.5, 2),
                       position = "right",
                       na.value = "grey80",
                       guide = "colourbar",
                       oob = scales::squish) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.title = element_text(angle = -90),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_colourbar(title.position = "right"))

graphics.off()
x11(width = 12, height = 6)
gg_test
ggsave("Figures/ZooMSS_tcb_test.pdf")

gg_ani <- ggplot() +
  geom_sf(data = nc_ani_sf_long, aes(fill = log10(tcb), colour = log10(tcb))) +
  # geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  scale_fill_distiller(name = expression(paste("log10 Biomass (g C m"^-2,")")),
                      palette = "OrRd",
                       direction = 1,
                       limits = c(0.5, 2),
                       position = "right",
                       na.value = "grey80",
                       guide = "colourbar",
                       oob = scales::squish) +
  scale_colour_distiller(name = expression(paste("log"[10], " Biomass (g C m"^-2,")")),
                         palette = "OrRd",
                         direction = 1,
                         limits = c(0.5, 2),
                         position = "right",
                         na.value = "grey80",
                         guide = "colourbar",
                         oob = scales::squish) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.title = element_text(angle = -90),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_colourbar(title.position = "right")) +
  transition_manual(year) +
  ggtitle("Year: {as.integer(current_frame)}")

graphics.off()
# x11(width = 12, height = 6)
system.time(
  animate(gg_ani, nframes = length(unique(nc_ani_sf_long$year)), fps = 4, width = 1000, height = 400)
)
anim_save("Figures/Annual_ZooMSS_Biomass.gif")




animate(gg_ani, duration = 2, width = 1600, height = 600)
        # , fps = 0.5, nframe = 5, width = 800, height = 450)
anim_save("Figures/Annual_ZooMSS_Biomass.gif")


animate(nations_plot, renderer = ffmpeg_renderer(), width = 800, height = 450)

gganimate::animate(gg_ani, nframes = 100)
# animate(gg_ani, renderer = ffmpeg_renderer(), fps = 0.5)





gg_fac <- ggplot() +
  geom_sf(data = nc_ani_sf_long, aes(fill = tcb), colour = NA) +
  # geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  scale_fill_distiller(name = expression(paste("Biomass (g C m"^-2,")")),
                       palette = "OrRd",
                       direction = 1,
                       # limits = c(quantile(r0$layer, .05), quantile(data_moll_df$layer, .95)),
                       limits = c(0, 60),
                       position = "right",
                       na.value = "grey80",
                       guide = "colourbar",
                       oob = scales::squish) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.title = element_text(angle = -90),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(vars(year))








