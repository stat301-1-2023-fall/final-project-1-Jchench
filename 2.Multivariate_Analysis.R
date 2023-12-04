library(tidyverse)
spotify_data <- read_rds("data/spotify_data_cleaned")

spotify_data <- spotify_data |> 
  mutate(
    stream_breaks = cut(final_streams, breaks = 3),
    stream_breaks = fct_collapse(stream_breaks,
                                 "<1 billion streams" = c("(4.95e+08,7.44e+08]"),
                                 "<500 million streams" = c("(2.48e+08,4.95e+08]"),
                                 "<250 million streams" = c("(-6.9e+05,2.48e+08]")
    ))

spotify_data <- spotify_data |> 
  mutate(major_minor = if_else(str_detect(key, "Major"), "Major", "Minor"))

write_rds(spotify_data, "data/spotify_data_cleaned")

# final streams and dance:

spotify_data |> 
  filter(!is.na(primary_genre)) |> 
  ggplot(aes(x = dance, y = final_streams)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ primary_genre, scales = "free")

spotify_data |> 
  ggplot(aes(x = dance, y = final_streams)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ time_signature, scales = "free")

# final streams and loud:

spotify_data |> 
  filter(!is.na(primary_genre)) |> 
  ggplot(aes(x = loud, y = final_streams)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ primary_genre, scales = "free")

spotify_data |> 
  ggplot(aes(x = loud, y = final_streams)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ time_signature, scales = "free")

# final streams and song length:

spotify_data |> 
  filter(!is.na(primary_genre)) |> 
  ggplot(aes(x = song_seconds, y = final_streams)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ primary_genre, scales = "free")

spotify_data |> 
  ggplot(aes(x = song_seconds, y = final_streams)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ time_signature, scales = "free")

# Key and genres:
spotify_data |> 
  filter(!is.na(primary_genre)) |> 
  ggplot(aes(x = stream_breaks)) +
  geom_bar(aes(fill = major_minor)) +
  facet_wrap(~ primary_genre) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

spotify_data |> 
  filter(!is.na(primary_genre)) |> 
  ggplot(aes(x = major_minor)) +
  geom_bar(aes(fill = primary_genre)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ stream_breaks, scales = "free")

spotify_data |> 
  filter(!is.na(primary_genre)) |> 
  ggplot(aes(x = primary_genre)) +
  geom_bar(aes(fill = major_minor)) +
  facet_wrap(~ stream_breaks, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# things across time

spotify_data |> 
  ggplot(aes(x = year, y = song_seconds, color = dance)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm")


