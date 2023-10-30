# Loading data and packages
library(tidyverse)
library(naniar)

spotify_data_top <- 
  read_csv("data/ATop 1990-2022.csv")

spotify_streams <- 
  read_csv("data/spotify_streams.csv")

glimpse(spotify_data_top)
