# Load required libraries -------------------------------------------------

library(tidyverse)
library(geosphere)


# Finnmaid DOI subset -----------------------------------------------------

meta <- read_delim("Data/SOCATv6.tsv", 
                      "\t", escape_double = FALSE, trim_ws = TRUE, 
                      skip = 4, n_max = 5500)

meta <- meta %>% 
  filter(`Platform Name` %in% c("Finnmaid", "VOS Finnpartner"))

Finnmaid_DOI <- meta$`SOCAT DOI`

f <- function(x, pos) subset(x, SOCAT_DOI  %in% Finnmaid_DOI)
df_DOI <- read_tsv_chunked(here::here("/Data", "SOCATv6.tsv"), skip = 5399,
                       DataFrameCallback$new(f), chunk_size = 100000)

rm(f, meta, Finnmaid_DOI)


# Finnmaid Flag E DOI subset ----------------------------------------------

meta <- read_delim("Data/SOCATv6_FlagE.tsv", 
                   "\t", escape_double = FALSE, trim_ws = TRUE, 
                   skip = 4, n_max = 107)

meta <- meta %>% 
  filter(`Platform Name` %in% c("Finnmaid", "VOS Finnpartner"))

Finnmaid_DOI <- meta$`SOCAT DOI`

f <- function(x, pos) subset(x, SOCAT_DOI  %in% Finnmaid_DOI)
df_DOI_E <- read_tsv_chunked(here::here("/Data", "SOCATv6_FlagE.tsv"),
                             skip = 154,
                       DataFrameCallback$new(f), chunk_size = 100000)

df_DOI_E <- df_DOI_E %>% 
  mutate(Expocode = as.character(Expocode))

rm(f, meta, Finnmaid_DOI)

# Read local Finnmaid data file -------------------------------------------

FM_local <- read_csv(here::here("Data", "Finnmaid_all_2019.csv"))

FM_local <- FM_local %>% 
  filter(date <= lubridate::ymd("2017-12-31")) %>% 
  select(date_time = date,
         lon = Lon,
         lat = Lat,
         sal = Sal,
         SST = Tem,
         fCO2 = pCO2) %>% 
  mutate(dataset = "Bernds Finnmaid data set, locally stored")

# Merge Central Baltic Sea and DOI data set -------------------------------

df <- bind_rows(df_DOI, df_DOI_E)

df <- df %>% 
  rename("lon" = "longitude [dec.deg.E]",
         "lat" = "latitude [dec.deg.N]",
         "fCO2" = "fCO2rec [uatm]",
         "SST" = "SST [deg.C]") %>% 
  mutate(date_time = lubridate::ymd_hms(paste(yr, mon, day, hh, mm, ss)),
         dataset = "SOCAT incl Flag E") %>% 
  select(date_time, lon, lat, sal, SST, fCO2, dataset)


# Include also locally stored Finnmaid data -------------------------------

df <- bind_rows(df, FM_local)
rm(df_DOI, df_DOI_E, FM_local)

# Define distance intervals from Travemuende ------------------------------

df <- df %>% 
  mutate(dist_Trave = distGeo(cbind(lon, lat), c(10.8605315, 53.9414096))/1e3,
         dist_Trave = as.numeric(as.character(
           cut(dist_Trave, seq(0,1500,20),labels = seq(10,1490,20)))))

# Plot Timeseries ---------------------------------------------------------

#i <- unique(df$dist_Trave)[1]

for (i in unique(df$dist_Trave)) {

  df %>% 
    filter(dist_Trave == i) %>% 
    ggplot(aes(date_time, SST, col=dataset))+
    geom_point(size = 0.5)+
    scale_color_brewer(palette = "Set1", name="")+
    labs(x="", y="SST / deg C", title = "Comparison of Finnmaid data sets",
         subtitle = paste("Distance from Travemünde (+/- 10km):",i,"km"))+
    theme_bw()+
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
    theme(legend.position = "bottom")
  
  ggsave(here::here("/Plots/local_vs_SOCAT", paste("SST_SOCATv6_vs_local_",i,"km.jpg", sep="")),
         width = 15, height = 4, dpi = 300)
 
  df %>% 
    filter(dist_Trave == i) %>% 
    ggplot(aes(date_time, SST))+
    geom_point(size = 0.5)+
    labs(x="", y="SST / deg C", title = "Comparison of Finnmaid data sets",
         subtitle = paste("Distance from Travemünde (+/- 10km):",i,"km"))+
    theme_bw()+
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
    theme(legend.position = "bottom")+
    facet_wrap(~dataset, ncol=1)
  
  ggsave(here::here("/Plots/local_vs_SOCAT", paste("SST_SOCATv6_vs_local_",i,"km_facet.jpg", sep="")),
         width = 7, height = 4, dpi = 300)
  
}


df %>% 
  filter(lat > 59, lat < 59.05) %>% 
  ggplot(aes(date_time, fCO2, col=dataset))+
  geom_point()+
  scale_color_brewer(palette = "Set1", name="")+
  labs(x="", y="pCO2 / fCO2", title = "Comparison of Finnmaid data sets",
      subtitle = "Own data and two subsets from SOCATv6.tsv | Lat range: 59-59.05N")+
  theme_bw()+
  theme(legend.position = "bottom")


ggsave(here::here("/Plots", "SOCATv6_incl_E.jpg"),
       width = 15, height = 4)




# Safe merged data file ---------------------------------------------------

df %>%
write_csv(here::here("/Data", "SOCATv6_Finnmaid.csv"))





