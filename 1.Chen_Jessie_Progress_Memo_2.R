# Loading packages:
library(tidyverse)
library(naniar)

spotify_data_top <- 
  read_csv("data/raw/ATop 1990-2022.csv")

spotify_streams <- 
  read_csv("data/raw/spotify_streams.csv")

# cleaning before joining:

spotify_data_top_cleaned <- 
  spotify_data_top |> 
  janitor::clean_names() |> 
  rename(song_name = song,
         artist_name = artist)

spotify_streams_cleaned <- 
  spotify_streams |> 
  janitor::clean_names() 

# joining data:

spotify_merged <- 
  spotify_data_top_cleaned |> 
  left_join(spotify_streams_cleaned |> 
              select(song_name, total_streams), 
            by = "song_name", 
            relationship = "many-to-many") |> 
  group_by(song_name, artist_name) |> 
  mutate(final_streams = sum(total_streams)) |> 
  drop_na() |> 
  ungroup() |> 
  select(-total_streams, -spotify_track_img, -spotify_track_id, -song_preview) |> 
  distinct(song_name, artist_name, .keep_all = TRUE)

# checking for missingness again

gg_miss_var(spotify_merged)

# EDA:

spotify_merged |> 
  ggplot(aes(x = final_streams)) +
  geom_histogram()

spotify_merged |> 
  ggplot(aes(x = dance)) +
  geom_histogram()
  
  