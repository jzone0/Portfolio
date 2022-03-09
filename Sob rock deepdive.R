library(dplyr)
library(spotifyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(knitr)
library(compmus)

Sys.setenv(SPOTIFY_CLIENT_ID = '1d0f7f8e56764a23ac0d6e6e19854863')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4b54a9da7d0448eaa1d69f2d67284d90')

access_token <- get_spotify_access_token()

sob_rock <- get_album_tracks(
  "2JmfwvRDitJlTUoLCkp61z",
  limit = 20,
  offset = 0,
  market = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)

track_info_sbr <- get_track_audio_features(sob_rock$id)
track_info_sbr

shot_in_the_dark <-
  get_tidy_audio_analysis("239yM7BAQ2CkNc61ogPGXo") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

shot_in_the_dark %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()
