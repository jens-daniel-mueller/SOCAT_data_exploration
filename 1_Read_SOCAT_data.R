# Load required libraries -------------------------------------------------

library(tidyverse)

# Read full SOCATv6 data set ----------------------------------------------

df <- read_tsv(here::here("/Data", "SOCATv6.tsv"), skip = 5399)

test <- sample_n(df, 1e6)
write_csv(test, here::here("/Data/_summarized_data", "SOCATv6_random_1e6.csv"))