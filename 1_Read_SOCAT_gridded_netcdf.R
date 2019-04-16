# Packages ----------------------------------------------------------------

library(tidyverse)
library(ncdf4)



# Open and inspect decadel gridded data file ------------------------------

nc <- nc_open(here::here("Data/Gridded", "SOCATv6_tracks_gridded_coast_monthly.nc"))
print(nc)

lat <- ncvar_get(nc, "ylat")        #units: degrees_north
lon <- ncvar_get(nc, "xlon")        #units: degrees_east
decade <- ncvar_get(nc, "tdecade")  #units: days since 1900-01-01 00:00:00


nc_close(nc) 
rm(nc) 


#var <- "temp"
attributes(nc$var)
# 
# for (var in c("SurfaceAge", "salt", "temp" )) {
#   
# array <- ncvar_get(nc, var) # store the data in a 3-dimensional array
# dim(array) # should have 3 dimensions: 1575 coordinate, 51 depth levels, 153 time steps
# 
# fillvalue <- ncatt_get(nc, var, "_FillValue")
# array[array == fillvalue$value] <- NA
# rm(fillvalue)
# 
# #nc_close(nc)
# 
# 
# # slice individual sections
# # subset profiles in BloomSail Area
# # bind profiles to one data frame
# 
# #i <- 3
# for (i in seq(1,length(t),1)){
# 
# slice <- array[, , i]
# slice <- as.data.frame(t(slice))
# names(slice) <- lat
# slice$dep <- zax
# 
# slice_long <- slice %>% 
#   gather("lat", "value", 1:length(lat)) %>% 
#   mutate(lat = as.numeric(lat),
#          dep = -dep)
# 
# # slice_long %>%
# #   ggplot(aes(lat, dep, col=value))+
# #   geom_point()+
# #   scale_y_reverse()+
# #   scale_color_viridis_c(option = "A")
# 
# date <- ymd_hms("2018-4-1 00:00:00")+t[i]
# 
# temp <- slice_long %>% 
#   filter(lat > 57.33, lat<57.5) %>% 
#   group_by(dep) %>% 
#   summarise_all("mean") %>% 
#   ungroup() %>% 
#   mutate(date = date)
#   
# if (exists("profiles", inherits = FALSE)){
#   profiles <- bind_rows(profiles, temp)
# } else{profiles <- temp}
# 
# rm(slice, slice_long, temp, date)
# 
# }
# 
# profiles <- profiles %>% 
#   mutate(var = var)
# 
# if (exists("profiles_all", inherits = FALSE)){
#   profiles_all <- bind_rows(profiles_all, profiles)
# } else{profiles_all <- profiles}
# 
# rm(profiles, array)
# 
# }
# 
# # Plot profiles for spring and summer period
# 
# profiles_all %>% 
#   filter(dep <= 35) %>% 
#   filter(date >= ymd("2018-06-01")) %>% 
#   ggplot(aes(value, dep, group=as.factor(date),  col=date)) +
#   geom_path()+
#   scale_color_viridis_c(trans = "time", name="")+
#   scale_y_reverse()+
#   labs(x="Value", y="Depth (m)", 
#        title = "2018 | Finnmaid | Route E | 57.33-57.5 °N")+
#   theme_bw()+
#   facet_grid(~var, scales = "free_x")
# 
# ggsave(here::here("Plots/GETM/Profiles",
#                   "Profiles_E_BloomSail_2018_summer.jpg"),
#        width = 10, height = 5, dpi = 300)
# 
# profiles_all %>% 
#   filter(dep <= 35) %>% 
#   filter(date <= ymd("2018-05-31")) %>% 
#   ggplot(aes(value, dep, group=as.factor(date),  col=date)) +
#   geom_path()+
#   scale_color_viridis_c(trans = "time", name="")+
#   scale_y_reverse()+
#   labs(x="Value", y="Depth (m)", 
#        title = "2018 | Finnmaid | Route E | 57.33-57.5 °N")+
#   theme_bw()+
#   facet_grid(~var, scales = "free_x")
# 
# ggsave(here::here("Plots/GETM/Profiles",
#                   "Profiles_E_BloomSail_2018_spring.jpg"),
#        width = 10, height = 5, dpi = 300)


 