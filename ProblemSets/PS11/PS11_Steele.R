library(elo)
library(tidyverse)
library(softballR)
library(devtools)
library(png)
library(tidyr)
library(ggimage)

year<-2019
K<-10

for (year in c(2019,2020,2021)){
  df<-softballR::load_ncaa_scoreboard(year)
  df$home_team_runs <- ifelse(is.na(df$home_team_runs), 0, df$home_team_runs)
  df$away_team_runs <- ifelse(is.na(df$away_team_runs), 0, df$away_team_runs)
  df<-df %>% 
    filter(!is.na(home_team))
  
  # Get unique team names
  teams <- unique(c(df$home_team, df$away_team))
  
  # Create an empty data frame with columns for team names, win counts, and loss counts
  df2 <- data.frame(team_name = teams, wins = rep(0, length(teams)), losses = rep(0, length(teams)), runs_for = rep(0, length(teams)), runs_against = rep(0, length(teams)), run_differential = rep(0, length(teams)),rating = rep(1500, length(teams)), stringsAsFactors = FALSE)
  
# Loop through each game in the df data frame and update the win and loss counts in the df2 data frame
for (i in seq_len(nrow(df))) {
  home_team <- df[i, "home_team"]
  away_team <- df[i, "away_team"]
  home_score <- df[i, "home_team_runs"]
  away_score <- df[i, "away_team_runs"]
  
  # Find the current ratings and win-loss records for each team
  home_rating <- df2[df2$team_name == home_team, "rating"]
  away_rating <- df2[df2$team_name == away_team, "rating"]
  home_wins <- df2[df2$team_name == home_team, "wins"]
  away_wins <- df2[df2$team_name == away_team, "wins"]
  home_losses <- df2[df2$team_name == home_team, "losses"]
  away_losses <- df2[df2$team_name == away_team, "losses"]
  
  # Calculate the expected win probability for the home team based on the Elo ratings and strength of schedule
  home_sos <- mean(df2[df2$team_name == away_team, "rating"])
  home_prob <- 1 / (1 + 10^((home_sos - home_rating) / 400))
  
  # Set the game score to 0 if it is missing or does not exist
  if (is.na(home_score)) home_score <- 0
  if (is.na(away_score)) away_score <- 0
  
  if (!is.na(home_score)) {
    df2[df2$team_name == home_team, "runs_for"] <- df2[df2$team_name == home_team, "runs_for"] + as.numeric(home_score)
    df2[df2$team_name == home_team, "run_differential"] <- df2[df2$team_name == home_team, "run_differential"] + (as.numeric(home_score) - as.numeric(away_score))
    df2[df2$team_name == away_team, "runs_against"] <- df2[df2$team_name == away_team, "runs_against"] + as.numeric(home_score)
  }
  if (!is.na(away_score)) {
    df2[df2$team_name == away_team, "runs_for"] <- df2[df2$team_name == away_team, "runs_for"] + as.numeric(away_score)
    df2[df2$team_name == away_team, "run_differential"] <- df2[df2$team_name == away_team, "run_differential"] + (as.numeric(away_score) - as.numeric(home_score))
    df2[df2$team_name == home_team, "runs_against"] <- df2[df2$team_name == home_team, "runs_against"] + as.numeric(away_score)
  }
  
  # Update the win and loss counts for each team
  if (as.numeric(home_score) > as.numeric(away_score)) {
    df2[df2$team_name == home_team, "wins"] <- df2[df2$team_name == home_team, "wins"] + 1
    df2[df2$team_name == away_team, "losses"] <- df2[df2$team_name == away_team, "losses"] + 1
    actual_prob <- 1
  } else if (as.numeric(away_score) > as.numeric(home_score)) {
    df2[df2$team_name == away_team, "wins"] <- df2[df2$team_name == away_team, "wins"] + 1
    df2[df2$team_name == home_team, "losses"] <- df2[df2$team_name == home_team, "losses"] + 1
    actual_prob <- 0
  }
  # Calculate the rating updates for each team based on strength of schedule and win-loss records
  home_update <- K * ((actual_prob - home_prob) + (home_wins - home_losses) / 20)
  away_update <- -home_update
  
  # Update the ratings
  df2[df2$team_name == home_team, "rating"] <- home_rating + home_update
  df2[df2$team_name == away_team, "rating"] <- away_rating + away_update
  
  if(year==2019){
    df_2019<-df2
  }
  else if(year==2020)
  {
    df_2020<-df2
  }
  else if (year==2021)
  {
    df_2021<-df2
  }
}
}

