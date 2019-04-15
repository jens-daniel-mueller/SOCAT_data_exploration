library(tidyverse)

meta <- read_delim("Data/SOCATv6.tsv", 
                      "\t", escape_double = FALSE, trim_ws = TRUE, 
                      skip = 4, n_max = 5500)


meta <- meta %>% 
  filter(`Platform Name` == "Finnmaid")

Finnmaid_DOI <- meta$`SOCAT DOI`



# f <- function(x, pos) subset(x, gear == 3)
# read_csv_chunked(readr_example("mtcars.csv"), DataFrameCallback$new(f), chunk_size = 5)

f <- function(x, pos) subset(x, SOCAT_DOI  %in% Finnmaid_DOI)
df <- read_tsv_chunked(here::here("/Data", "SOCATv6.tsv"), skip = 5399,
                       DataFrameCallback$new(f), chunk_size = 5000)



# SOCATv6 <- read_delim("Data/SOCATv6.tsv", 
#                       "\t", escape_double = FALSE, col_types = cols(day = col_number(), 
#                                                                     hh = col_number(), mm = col_number(), 
#                                                                     mon = col_number(), ss = col_number(), 
#                                                                     yr = col_number()), trim_ws = TRUE, 
#                       skip = 5399, n_max = 1000)

names(df)
unique(df$yr)

df <- df %>% 
  rename("lon" = "longitude [dec.deg.E]",
         "lat" = "latitude [dec.deg.N]",
         "fCO2" = "fCO2rec [uatm]")

df <- df %>% 
  mutate(date_time = lubridate::ymd_hms(paste(yr, mon, day, hh, mm, ss)))


df %>%
  filter(yr == 2017) %>% 
  ggplot(aes(lat, fCO2, col=date_time))+
  geom_point()


df %>% 
write_csv(here::here("/Data", "SOCATv6_Finnmaid.csv"))






