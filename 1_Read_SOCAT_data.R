# Load required libraries -------------------------------------------------

library(tidyverse)

# Read full SOCATv6 data set ----------------------------------------------

df <- read_tsv(here::here("/Data", "SOCATv6.tsv"), skip = 5399)


floor_decade    = function(value){ return(value - value %% 10) }

df <- df %>%
  mutate(lat.int = cut(`latitude [dec.deg.N]`, seq(-90,90,10), 
                       labels = seq(-85,85,10)),
         lon.int = cut(`longitude [dec.deg.E]`, seq(-180,180,20), 
                       labels = seq(-170,170,20)),
         d_Tem = cut(`Tequ [deg.C]`-`SST [deg.C]`, seq(-10,20,0.25),
                     labels = seq(-9.88,20,0.25)),
         floor_decade = floor_decade(yr))

df_int <- df %>% 
  group_by(floor_decade, lat.int, d_Tem) %>% 
  summarise(nr=n(),
            SST_mean = mean(`SST [deg.C]`),
            fCO2_mean = mean(`fCO2rec [uatm]`)) %>% 
  ungroup()

df_int <- df_int %>%
  mutate(lat.int = as.numeric(as.character(lat.int)),
         d_Tem = as.numeric(as.character(d_Tem)))

df_int %>% 
  filter(d_Tem > -5, d_Tem < 5) %>% 
  ggplot(aes(lat.int, d_Tem, fill=log10(nr)))+
  geom_raster()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  scale_fill_viridis_c(name = "log10(nr observations)")+
  theme_bw()+
  labs(x="Latitude (10 deg N intervals)", y="Tequi - SST (K)",
       title = "Difference between insitu and measurement temperature",
       subtitle = "SOCATv6 data displayed for decades and latitude intervals")+
  facet_wrap(~floor_decade)+
  theme(legend.position = "bottom")

ggsave(here::here("Plots/deltaT", "deltaT_SOCAT.jpg"), width = 7, height = 7)


# df_int %>% 
#   ggplot(aes(d_Tem, lat.int, fill=fCO2_mean))+
#   geom_vline(xintercept = 0)+
#   geom_raster()+
#   scale_fill_viridis_c(limits=c(200,500))+
#   theme_bw()+
#   facet_wrap(~floor_decade)

df_int %>% 
  ggplot(aes(d_Tem, fCO2_mean, col=as.factor(floor_decade)))+
  geom_vline(xintercept = 0)+
  geom_point()+
  scale_color_viridis_d(name="Decade", direction = -1)+
  theme_bw()+
  facet_wrap(~lat.int)+
  ylim(200,500)+
  labs(x="Tequi - SST (K)", y="fCO2 (uatm)",
       title = "fCO2 relation to temperature difference",
       subtitle = "SOCATv6 data displayed for decades and latitude (10 deg N intervals)")

ggsave(here::here("Plots/deltaT", "fCO2_vs_deltaT_SOCAT.jpg"), width = 8, height = 6)

# 
# df %>% 
# write_rds(here::here("/Data", "SOCATv6.rds"), compress = "none")
# 
# df <- read_rds(here::here("/Data", "SOCATv6.rds"))
# 
# df_2010 <- df %>% 
#   filter(yr >= 2010)
# 
# df_2010 %>%
# write_rds(here::here("/Data", "SOCATv6_2010.rds"), compress = "none")
# 
# df_2010 <- read_rds(here::here("/Data", "SOCATv6_2010.rds"))
# df <- read_rds(here::here("/Data", "SOCATv6.rds"))
# rm(df)
# 
# 
# df_2010
# 
# 
# 
# 
# df_2010 %>% 
#   ggplot(aes(`Tequ [deg.C]`-`SST [deg.C]`))+
#   geom_histogram()
# 
# df_2010 %>% 
#   ggplot(aes(`latitude [dec.deg.N]`,`Tequ [deg.C]`-`SST [deg.C]`))+
#   geom_point()
# 
# 
# for (param in c("SST","sal", "fCO2")) {
#   for (i in na.omit(unique(df$dist_Trave))) {
#     
#     df %>% 
#       filter(dist_Trave == i) %>% 
#       ggplot(aes_string("date_time", param, col="dataset"))+
#       geom_point(size = 0.5)+
#       scale_color_brewer(palette = "Set1", name="")+
#       labs(x="", title = "Comparison of Finnmaid data sets",
#            subtitle = paste("Distance from Travemünde (+/- 10km):",i,"km"))+
#       theme_bw()+
#       scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
#       theme(legend.position = "bottom")
#     
#     ggsave(here::here("/Plots/local_vs_SOCAT", paste(param,"_SOCATv6_vs_local_",i,"km.jpg", sep="")),
#            width = 15, height = 4, dpi = 300)
#     
#     df %>% 
#       filter(dist_Trave == i) %>% 
#       ggplot(aes_string("date_time", as.name(param)))+
#       geom_point(size = 0.5)+
#       labs(x="", title = "Comparison of Finnmaid data sets",
#            subtitle = paste("Distance from Travemünde (+/- 10km):",i,"km"))+
#       theme_bw()+
#       scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
#       theme(legend.position = "bottom")+
#       facet_wrap(~dataset, ncol=1)
#     
#     ggsave(here::here("/Plots/local_vs_SOCAT", paste(param,"_SOCATv6_vs_local_",i,"km_facet.jpg", sep="")),
#            width = 7, height = 4, dpi = 300)
#     
#   }
# }
# 
# 
# 
# 
# 
# f <- function(x, pos) subset(x, SOCAT_DOI  %in% Finnmaid_DOI)
# df_DOI <- read_tsv_chunked(here::here("/Data", "SOCATv6.tsv"), skip = 5399,
#                        DataFrameCallback$new(f), chunk_size = 100000)
# 
# 
# 
# 

