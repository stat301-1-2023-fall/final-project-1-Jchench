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