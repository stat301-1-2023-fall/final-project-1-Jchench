# EDA:

spotify_data <- 
  read_rds("data/spotify_data_cleaned")

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

spotify_data|> 
  ggplot(aes(x = time_signature)) +
  geom_bar()

# changes throughout years

spotify_data_numeric <- 
  spotify_data |> 
  select(where(is.numeric))

spotify_data_numeric |> 
  cor(use = "pairwise.complete.obs") |> 
  as_tibble() |> 
  select(year) |> 
  mutate(variable = colnames(spotify_data_numeric), .before = year) |> 
  arrange(desc(variable)) |> 
  DT::datatable() |> 
  DT::formatRound(2:2, digits = 3)

# univariate

spotify_data_select <- 
  spotify_data |> 
  select(c(final_streams, year, dance, loud, primary_genre, key, time_signature, song_seconds))

for(var in colnames(spotify_data_select)){
  
  if (is.numeric(spotify_data_select[[var]])){
    plot <- spotify_data_select |> 
      ggplot(aes(x = !!sym(var))) +
      geom_histogram() +
      xlab(var)
  } else {
    plot <- spotify_data_select |> 
      ggplot(aes(x = !!sym(var))) +
      geom_bar() +
      xlab(var) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  ggsave(str_c("plots/", var, ".png"))
}
