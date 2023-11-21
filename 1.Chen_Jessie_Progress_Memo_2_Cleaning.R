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

# cleaner genres
spotify_pivot_genre <- 
  spotify_merged |> 
  mutate(primary_genre = str_extract(parent_genres, "[A-Z][a-z]+"))

spotify_pivot_genre_count <- 
  spotify_pivot_genre |> 
  summarize(num_distinct = n_distinct(primary_genre))

# cleaner years
spotify_merged_years <- 
  spotify_merged |> 
  mutate(album_date = as.Date(album_date),
         year = year(album_date))

spotify_merged_years_1 <-  
  spotify_merged_years |> 
  summarize(mean_dance = mean(dance), .by = year)

spotify_merged_years_1 |> 
  ggplot(aes(x = year, y = mean_dance)) +
  geom_point()
  
# convert song length to numeric variable (so it is easier to work with)

spotify_merged_seconds <- 
  spotify_merged |> 
  mutate(song_seconds = period_to_seconds(hms(time))/60)

# cleaning up the cleaning into one file

spotify_data_top_cleaned <- 
  spotify_data_top |> 
  janitor::clean_names() |> 
  rename(song_name = song,
         artist_name = artist)

spotify_streams_cleaned <- 
  spotify_streams |> 
  janitor::clean_names() 

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
  select(-total_streams, -spotify_track_img, -spotify_track_id, -song_preview, 
         -popularity, -number) |> 
  distinct(song_name, artist_name, .keep_all = TRUE)

spotify_merged <- 
  spotify_merged |> 
  mutate(
    primary_genre = str_extract(parent_genres, "[A-Z][a-z]+"),
    album_date = as.Date(album_date),
    year = year(album_date),
    song_seconds = period_to_seconds(hms(time))/60,
    time_signature = as.character(time_signature)
  )

write_rds(spotify_merged, "data/spotify_data_cleaned")
