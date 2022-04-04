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

ggplot(track_info_sbr, aes(x = tempo, y = energy, color = instrumentalness)) +
  geom_point() 

eighties_hits <- get_playlist_tracks(
  "37i9dQZF1DXb57FjYWz00c",
  limit = 100,
  offset = 0,
  market = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)

eighties_hits_analyzed <- get_playlist_audio_features("11149714474", "37i9dQZF1DXb57FjYWz00c", 
                                                      authorization = get_spotify_access_token()) %>%
  select(danceability, energy, key, loudness, mode, speechiness, 
         acousticness, instrumentalness, liveness, valence, tempo, 
         track.id, time_signature, track.duration_ms, track.name, 
         track.popularity, track.artists)

eighties_hits_analyzed

divorcecore <- get_playlist_tracks(
  "16K3PWAPbSxG539FntLQ0e",
  limit = 100,
  offset = 0,
  market = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)

divorcecore_analyzed <- get_playlist_audio_features("11149714474", "16K3PWAPbSxG539FntLQ0e", 
                                                    authorization = get_spotify_access_token()) %>%
  select(danceability, energy, key, loudness, mode, speechiness, 
         acousticness, instrumentalness, liveness, valence, tempo, 
         track.id, time_signature, track.duration_ms, track.name, 
         track.popularity, track.artists)

divorcecore_analyzed

ggplot(eighties_hits_analyzed, aes(x = tempo, y = energy, color = instrumentalness)) +
  geom_point() 

ggplot(divorcecore_analyzed, aes(x = tempo, y = energy, color = instrumentalness)) +
  geom_point() 

ggplot(eighties_hits_analyzed, aes(x = danceability)) +
  geom_histogram(bins = 30) +
  labs(title = "Danceability of 80's Hits",
       x = "Danceability",
       y = "Number of Songs")

ggplot(divorcecore_analyzed, aes(x = danceability)) +
  geom_histogram(bins = 30) +
  labs(title = "Danceability of DivorceCore",
       x = "Danceability",
       y = "Number of Songs")

ggplot(track_info_sbr, aes(x = danceability)) +
  geom_histogram(bins = 30) +
  labs(title = "Danceability of Sob Rock Tracks",
       x = "Danceability",
       y = "Number of Songs")

shot_in_the_dark_chromagram <-
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

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

shot_in_the_dark_chordogram <-
  get_tidy_audio_analysis("239yM7BAQ2CkNc61ogPGXo") %>%
  compmus_align(sections, segments) %>%
  select(sections) %>%
  unnest(sections) %>%
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  )
shot_in_the_dark_chordogram %>% 
  compmus_match_pitch_template(
    key_templates,        
    method = "euclidean",
    norm = "manhattan"    
  ) %>%
  ggplot(aes(x = start + duration / 2, width = duration, y = name, fill = d)) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")

