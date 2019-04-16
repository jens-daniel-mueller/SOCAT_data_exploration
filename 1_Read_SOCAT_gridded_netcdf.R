# Packages ----------------------------------------------------------------

library(tidyverse)
library(ncdf4)


# Open and inspect decadel gridded data file ------------------------------

nc <- nc_open(here::here("Data/Gridded", "SOCATv6_tracks_gridded_monthly.nc"))
print(nc)

lat <- ncvar_get(nc, "ylat")        #units: degrees_north
lon <- ncvar_get(nc, "xlon")        #units: degrees_east
month <- ncvar_get(nc, "tmnth")  #units: days since 1970-01-01 00:00:00
#decade <- ncvar_get(nc, "tdecade")  #units: days since 1970-01-01 00:00:00

month <- lubridate::as_date(month, origin = lubridate::ymd("1970-01-01"))
#decade <- lubridate::as_date(decade, origin = lubridate::ymd("1900-01-01"))

#nc_close(nc) 
#rm(nc) 


attributes(nc$var)

var <- "fco2_ave_unwtd"
#var <- "fco2_ave_unwtd_decade"

#for (var in c("SurfaceAge", "salt", "temp" )) {

array <- ncvar_get(nc, var) # store the data in a 3-dimensional array
dim(array) # should have 3 dimensions: 1575 coordinate, 51 depth levels, 153 time steps

fillvalue <- ncatt_get(nc, var, "_FillValue")
array[array == fillvalue$value] <- NA
rm(fillvalue)

# slice individual sections
# subset profiles in BloomSail Area
# bind profiles to one data frame

#i <- 500
for (i in seq(1,length(month),1)){

slice <- array[, , i]
slice <- as.data.frame(slice)

names(slice) <- lat
slice$lon <- lon

slice_long <- slice %>%
  gather("lat", "value", 1:length(lat)) %>%
  mutate(lat = as.numeric(lat))

# slice_long %>%
#   filter(!is.na(value)) %>% 
#   ggplot(aes(lon, lat, fill=value))+
#   geom_raster()+
#   scale_fill_viridis_c(limits=c(300, 500))+
#   coord_quickmap()

date <- month[i]

temp <- slice_long %>%
  filter(lat > 54, lat<61,
         lon > 10, lon<30) %>%
  mutate(date = date)

if (exists("ts", inherits = FALSE)){
  ts <- bind_rows(ts, temp)
} else{ts <- temp}

rm(slice, slice_long, temp, date)

}

profiles <- profiles %>%
  mutate(var = var)

if (exists("profiles_all", inherits = FALSE)){
  profiles_all <- bind_rows(profiles_all, profiles)
} else{profiles_all <- profiles}

rm(profiles, array)
}



# Plot monthly Baltic Sea data --------------------------------------------

ts %>%
  filter(lubridate::year(date) >= 2003) %>% 
  ggplot(aes(date, value, col=as.factor(lat)))+
  geom_point()+
  scale_color_viridis_d(name="Lat")+
  labs(x="", y="pCO2 / fCO2", title = "Comparison of Finnmaid data sets",
       subtitle = "Monthly gridded data from SOCAT")+
  theme_bw()
  
ggsave(here::here("/Plots", "SOCATv6_monthly_gridded_Baltic.jpg"),
       width = 15, height = 5)

