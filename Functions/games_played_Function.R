
# AFL Experience

#Use games played as a cumulative sum from the past year starting at 2010.


GamesPlayed <- function(data) {
  

age_added <-
  data %>% 
  mutate(played_game = 1) %>%
  group_by(player_id) %>%
  arrange(date) %>%
  mutate(games_played = cumsum(played_game)) %>%
  ungroup() %>%
  relocate(games_played, .after = position_new)
 
}


