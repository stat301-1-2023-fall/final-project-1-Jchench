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

# corrplot:

spotify_merged_numeric <- 
  spotify_merged |> 
  select(where(is.numeric))

spotify_merged_corr <- 
  spotify_merged_numeric |> 
  cor(use = "pairwise.complete.obs") |> 
  ggcorrplot::ggcorrplot()

spotify_merged_corr

spotify_merged_streams_corr <- 
  spotify_merged_numeric |> 
  cor(use = "pairwise.complete.obs") |> 
  as_tibble() |> 
  select(final_streams) |> 
  mutate(var = colnames(spotify_merged_numeric), .before = final_streams)

# categorical data analysis
time_signature <- 
  spotify_merged |> 
  summarize(num_distinct_time_signature = n_distinct(time_signature))

# clearer genres
spotify_pivot_genre <- 
  spotify_merged |> 
  mutate(primary_genre = str_extract(parent_genres, "[A-Z][a-z]+"))

spotify_pivot_genre_count <- 
  spotify_pivot_genre |> 
  summarize(num_distinct = n_distinct(primary_genre))
