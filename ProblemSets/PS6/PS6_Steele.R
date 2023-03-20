library(elo)
library(tidyverse)
library(softballR)
library(devtools)
library(png)
library(tidyr)
library(ggimage)

df<-softballR::load_espn_scoreboard(2023)

# Get unique team names
teams <- unique(c(df$home_team_display_name, df$away_team_display_name))

# Create an empty data frame with columns for team names, win counts, and loss counts
df2 <- data.frame(team_name = teams, win_count = rep(0, length(teams)), loss_count = rep(0, length(teams)), runs_for = rep(0, length(teams)), runs_against = rep(0, length(teams)), run_differential = rep(0, length(teams)), stringsAsFactors = FALSE)

# Loop through each game in the df data frame and update the win and loss counts in the df2 data frame
for (i in seq_len(nrow(df))) {
  home_team <- df[i, "home_team_display_name"]
  away_team <- df[i, "away_team_display_name"]
  home_score <- df[i, "home_team_runs"]
  away_score <- df[i, "away_team_runs"]
  
  df2[df2$team_name == home_team, "runs_for"] <- df2[df2$team_name == home_team, "runs_for"] + as.numeric(home_score)
  df2[df2$team_name == home_team, "runs_against"] <- df2[df2$team_name == home_team, "runs_against"] + as.numeric(away_score)
  df2[df2$team_name == home_team, "run_differential"] <- df2[df2$team_name == home_team, "run_differential"] + (as.numeric(home_score) - as.numeric(away_score))
  df2[df2$team_name == away_team, "runs_for"] <- df2[df2$team_name == away_team, "runs_for"] + as.numeric(away_score)
  df2[df2$team_name == away_team, "runs_against"] <- df2[df2$team_name == away_team, "runs_against"] + as.numeric(home_score)
  df2[df2$team_name == away_team, "run_differential"] <- df2[df2$team_name == away_team, "run_differential"] + (as.numeric(away_score) - as.numeric(home_score))
  # Update the win and loss counts for each team
  if (as.numeric(home_score) > as.numeric(away_score)) {
    df2[df2$team_name == home_team, "win_count"] <- df2[df2$team_name == home_team, "win_count"] + 1
    df2[df2$team_name == away_team, "loss_count"] <- df2[df2$team_name == away_team, "loss_count"] + 1
  } else if (as.numeric(away_score) > as.numeric(home_score)) {
    df2[df2$team_name == away_team, "win_count"] <- df2[df2$team_name == away_team, "win_count"] + 1
    df2[df2$team_name == home_team, "loss_count"] <- df2[df2$team_name == home_team, "loss_count"] + 1
  }
}

df2$win_pct <- df2$win_count / (df2$win_count + df2$loss_count)


for (i in seq_len(nrow(df2))) {
  team_name <- df2[i, "team_name"]
  match_idx <- match(team_name, df$home_team_display_name)
  if (is.na(match_idx)) {
    match_idx <- match(team_name, df$away_team_display_name)
    logo_path <- df[match_idx, "away_team_logo"]
  } else {
    logo_path <- df[match_idx, "home_team_logo"]
  }
  df2[i, "logo_path"] <- logo_path
}


# create a new column for opponent win percentage in df2
df2$opponent_win_pct <- 0

# loop over each row in df2
for (i in seq_len(nrow(df2))) {
  team_name <- df2[i, "team_name"]
  
  # get all games played by the team
  team_games <- df[df$home_team_display_name == team_name | df$away_team_display_name == team_name, ]
  
  # create a vector of all opponent team names
  opponent_teams <- unique(c(team_games$home_team_display_name, team_games$away_team_display_name))
  
  # get win count and loss count for the team
  team_wins <- df2[df2$team_name == team_name, "win_count"]
  team_losses <- df2[df2$team_name == team_name, "loss_count"]
  
  # loop over opponent teams
  opponent_wins <- 0
  opponent_losses <- 0
  for (j in opponent_teams) {
    # get win count and loss count for opponent team
    opponent_team_wins <- df2[df2$team_name == j, "win_count"]
    opponent_team_losses <- df2[df2$team_name == j, "loss_count"]
    
    # sum opponent team's wins
    opponent_wins <- opponent_wins + opponent_team_wins
    
    # sum opponent team's losses
    opponent_losses <- opponent_losses + opponent_team_losses
  }
  
  # calculate opponent win percentage
  if (opponent_losses == 0) {
    opponent_win_pct <- 1
  } else {
    opponent_win_pct <- opponent_wins / (opponent_wins + opponent_losses)
  }
  
  # store opponent win percentage in df2
  df2[i, "opponent_win_pct"] <- opponent_win_pct
}

top_25 <- df2[order(df2$run_differential, decreasing = TRUE), ][1:25, ]

ggplot(top_25, aes(x = runs_for, y = runs_against)) +
  geom_point(color = "black", size = 2) +
  geom_image(aes(image = logo_path), size = 0.05) +
  scale_x_continuous(name = "Runs For", limits = c(0, max(top_25$runs_for) + 10)) +
  scale_y_continuous(name = "Runs Against", limits = c(0, max(top_25$runs_against) + 10)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))


ggplot(top_25, aes(x = opponent_win_pct, y = win_pct, image = logo_path)) +
  geom_image(size = 0.05) +
  labs(x = "Opponent Win Percentage", y = "Winning Percentage") +
  theme_bw()

ggplot(top_25, aes(x = opponent_win_pct, y = run_differential, image = logo_path)) +
  geom_image(size = 0.05) +
  labs(x = "Opponent Win Percentage", y = "Run Differential") +
  theme_bw()
