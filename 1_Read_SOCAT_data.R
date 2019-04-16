# Load required libraries -------------------------------------------------

library(tidyverse)


# Read header from complete SOCATv6 tsv file  -----------------------------

meta <- read_delim("Data/SOCATv6.tsv", 
                      "\t", escape_double = FALSE, trim_ws = TRUE, 
                      skip = 4, n_max = 5500)



# Finnmaid DOI subset -----------------------------------------------------

unique( meta$`Platform Name` )

meta <- meta %>% 
  filter(`Platform Name` %in% c("Finnmaid", "VOS Finnpartner"))

Finnmaid_DOI <- meta$`SOCAT DOI`


f <- function(x, pos) subset(x, SOCAT_DOI  %in% Finnmaid_DOI)
df_DOI <- read_tsv_chunked(here::here("/Data", "SOCATv6.tsv"), skip = 5399,
                       DataFrameCallback$new(f), chunk_size = 100000)

rm(f)

# f <- function(x, pos) subset(x, gear == 3)
# read_csv_chunked(readr_example("mtcars.csv"), DataFrameCallback$new(f), chunk_size = 5)


# Central Baltic Sea subset -----------------------------------------------

f_coord <- function(x, pos) subset(x, `longitude [dec.deg.E]` > 11 & `longitude [dec.deg.E]` < 30 &
                               `latitude [dec.deg.N]` > 54 & `latitude [dec.deg.N]` < 61)

df_coord <- read_tsv_chunked(here::here("/Data", "SOCATv6.tsv"), skip = 5399,
                       DataFrameCallback$new(f_coord), chunk_size = 100000)

df_coord <- df_coord %>% 
  filter(yr >= 2003)


# SOCATv6 <- read_delim("Data/SOCATv6.tsv", 
#                       "\t", escape_double = FALSE, col_types = cols(day = col_number(), 
#                                                                     hh = col_number(), mm = col_number(), 
#                                                                     mon = col_number(), ss = col_number(), 
#                                                                     yr = col_number()), trim_ws = TRUE, 
#                       skip = 5399, n_max = 1000)

names(df_DOI)
unique(df_DOI$yr)
unique(df_coord$yr)
unique(df_DOI$SOCAT_DOI)
unique(df_coord$SOCAT_DOI)



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
  mutate(subset = "Bernds Finnmaid data set, locally stored")

# Merge Central Baltic Sea and DOI data set -------------------------------

df_DOI$subset <- "SOCAT: Finnmaid/Finnpartner DOI"
df_coord$subset <- "SOCAT: Central Baltic coordinates"

df <- bind_rows(df_DOI, df_coord)

df <- df %>% 
  rename("lon" = "longitude [dec.deg.E]",
         "lat" = "latitude [dec.deg.N]",
         "fCO2" = "fCO2rec [uatm]",
         "SST" = "SST [deg.C]") %>% 
  mutate(date_time = lubridate::ymd_hms(paste(yr, mon, day, hh, mm, ss))) %>% 
  select(date_time, lon, lat, sal, SST, fCO2, subset)

df <- bind_rows(df, FM_local)


df %>% 
  filter(lat > 59, lat < 59.05) %>% 
  ggplot(aes(date_time, fCO2, col=SST))+
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~subset, ncol = 1)+
  labs(x="", y="pCO2 / fCO2", title = "Comparison of Finnmaid data sets",
      subtitle = "Own data and two subsets from SOCATv6.tsv | Lat range: 59-59.05N")+
  theme_bw()


ggsave(here::here("/Plots", "SOCATv6_Central_Baltic_vs_Finnmaid.jpg"),
       width = 15, height = 8)





# df %>% 
# write_csv(here::here("/Data", "SOCATv6_Finnmaid.csv"))






