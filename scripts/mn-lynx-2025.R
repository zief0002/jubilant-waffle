#devtools::install_github("sportsdataverse/wehoop")


# load libraries
library(tidyverse)
library(wehoop)


espn_wnba_scoreboard(2025) |>
  filter(home_team_abb == "MIN" | away_team_abb == "MIN") |>
  print(width = Inf)

espn_wbb_scoreboard(2026) |>
  filter(home_team_abb == "MINN" | away_team_abb == "MINN") |>
  print(width = Inf)


# Find a game ID for a specific date (example game ID is for illustration)
# You might first use espn_wnba_scoreboard() to find the game_id
game_id <- 401736123 # Replace with an actual WNBA game ID

# Get player box scores
player_box <- espn_wnba_player_box(game_id = game_id)

espn_wbb_player_box(game_id = 401721512) |>
  filter(team_name == "Golden Gophers") |>
  select(game_date, athlete_display_name, minutes)
  print(n = Inf)

game_1 = player_box |>
  filter(team_name == "Lynx") |>
  select(game_date, athlete_display_name, minutes)

game_1$minutes

# The result is a list containing several data frames (e.g., home team, away team)
# You can view the data for a specific team
home_team_stats <- player_box$home_player_box
away_team_stats <- player_box$away_player_box

# Combine them into a single data frame if needed
full_game_box_score <- bind_rows(home_team_stats, away_team_stats)
View(full_game_box_score)


## LYNX PLAYING TIME

lynx_games = espn_wnba_scoreboard(2025) |>
  filter(home_team_abb == "MIN" | away_team_abb == "MIN")

lynx_id = unique(lynx_games$game_id)
n = length(lynx_id)

lynx_data = vector(mode = "list", length = n)


for(i in 1:n){
  lynx_data[[i]] = espn_wnba_player_box(game_id = lynx_id[i]) |>
    filter(team_name == "Lynx") |>
    select(game_date, athlete_display_name, minutes)
}



lynx_2025 = do.call(rbind, lynx_data)


reg_season = lynx_games |>
  filter(season_slug == "regular-season") |>
  select(game_id, game_date)


ordered_lynx = lynx_2025 |>
  filter(game_date %in% reg_season$game_date) |>
  group_by(athlete_display_name) |>
  summarize(
    M = mean(minutes, na.rm = TRUE),
    N = n()
    ) |>
  arrange(desc(N), desc(M)) |>
  print(n = Inf)

game_num = tibble(
  game_date = as.Date(unique(lynx_2025$game_date)),
  game_num = as.factor(1:length(unique(lynx_2025$game_date))),
  game_num2 = 1:length(unique(lynx_2025$game_date))
)


lynx_2025 |>
  left_join(game_num, by = "game_date") |>
  filter(game_date %in% reg_season$game_date) |>
  mutate(
    minutes2 = if_else(is.na(minutes), 0, minutes),
    player = ordered(athlete_display_name, levels = ordered_lynx$athlete_display_name)
    ) |>
  ggplot(aes(x = game_num2, y = player, fill = minutes2)) +
   geom_tile(na.rm = TRUE, color = "white") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), # Removes major grid lines
      panel.grid.minor = element_blank(), # Removes minor grid lines
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom",
      legend.justification = c("left", "bottom"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      text = element_text(family = "Georgia"),
      plot.title = element_text(family = "Roboto", face = "bold", size = 20),
      plot.subtitle = element_text(family = "Roboto", size = 14),
      plot.caption = element_text(family = "Roboto")
    ) +
    scale_fill_viridis_c(
      name = "Playing time\n(in min.)",
      direction = -1
      ) +
    scale_x_continuous(
      name = "",
      breaks = c(6, 14, 25.5, 38, 46),
      labels = c("May", "June", "July", "August", "Sept."),
    ) +
    scale_y_discrete(
      name = "",
      limits = rev
      ) +
    geom_vline(xintercept = c(2.5, 9.5, 19.5, 32.5, 43.5)) +
    labs(
      title = "2025 MINNESOTA LYNX PLAYING TIME",
      subtitle = "The number of minutes played in each of the 53 regular season games of 2025.\n",
      caption = "Data Source: ESPN via wehoops R package"
    )





