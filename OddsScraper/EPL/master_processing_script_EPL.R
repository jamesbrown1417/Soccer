#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
    tryCatch({
        source(script_name, echo = FALSE)
    }, error = function(e) {
        cat("Odds not released yet for:", script_name, "\n")
    })
}

# Run all odds scraping scripts
run_scraping("OddsScraper/EPL/scrape_TAB_EPL.R")
run_scraping("OddsScraper/EPL/scrape_Sportsbet_EPL.R")
run_scraping("OddsScraper/EPL/scrape_pointsbet_EPL.R")
run_scraping("OddsScraper/EPL/Neds/scrape_neds_EPL.R")
run_scraping("OddsScraper/EPL/scrape_dabble.R")

#===============================================================================
# Read in all H2H
#===============================================================================

# Read in all H2H data
list_of_h2h_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "h2h")

# Read in all H2H data
list_of_h2h_data <-
    map(list_of_h2h_files, read_csv)

# Combine
h2h_data <-
    list_of_h2h_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match) |> 
    select(-start_time) |> 
    select(match:agency)

# Write out
write_rds(h2h_data, "Data/processed_odds/h2h_data.rds")

#===============================================================================
# Both Teams To Score
#===============================================================================

# Read in both teams to score data
list_of_btts_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "both_teams_to_score|btts")

# Read in all both teams to score data
list_of_btts_data <-
    map(list_of_btts_files, read_csv)

# Combine
btts_data <-
    list_of_btts_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, desc(yes_price)) |> 
    select(-start_time) |> 
    select(match:agency)

# Write out
write_rds(btts_data, "Data/processed_odds/btts_data.rds")

#===============================================================================
# Player Goals
#===============================================================================

# Read in player goals data
list_of_player_goals_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "player_goals")

# Read in all player goals data
list_of_player_goals_data <-
    map(list_of_player_goals_files, read_csv)

# Combine
player_goals_data <-
    list_of_player_goals_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, player_name, line, desc(over_price)) |> 
    select(-start_time) |> 
    select(match, home_team, away_team, market_name, player_name, line, over_price, under_price, agency)
  

# Write out
write_rds(player_goals_data, "Data/processed_odds/player_goals_data.rds")

#===============================================================================
# Total Goals
#===============================================================================

# Read in total goals data
list_of_total_goals_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "total_goals.csv$")

# Get rid of team goals from list
list_of_total_goals_files <- list_of_total_goals_files |> 
    keep(~!grepl("team", .x))

# Read in all total goals data
list_of_total_goals_data <-
    map(list_of_total_goals_files, read_csv)

# Combine
total_goals_data <-
    list_of_total_goals_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, line, desc(over_price)) |> 
    select(-start_time) |> 
    select(match:agency)

# Write out
write_rds(total_goals_data, "Data/processed_odds/total_goals_data.rds")

#===============================================================================
# Team Goals
#===============================================================================

# Read in team goals data
list_of_team_goals_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "team_goals|team_total_goals")

# Read in all team goals data
list_of_team_goals_data <-
    map(list_of_team_goals_files, read_csv)

# Combine
team_goals_data <-
    list_of_team_goals_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, team, line, desc(over_price)) |> 
    select(-start_time) |> 
    select(match:agency)

# Write out
write_rds(team_goals_data, "Data/processed_odds/team_goals_data.rds")

#===============================================================================
# Player Shots
#===============================================================================

# Read in player shots data
list_of_player_shots_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "player_shots\\.csv")

# Read in all player shots data
list_of_player_shots_data <-
    map(list_of_player_shots_files, read_csv)

# Combine
player_shots_data <-
    list_of_player_shots_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, player_name, line, desc(over_price)) |> 
    select(match, player_name, line, over_price, under_price, agency) |>
    mutate(market_name = "Player Shots")

# Write out
write_rds(player_shots_data, "Data/processed_odds/player_shots_data.rds")

#===============================================================================
# Player Shots On Target
#===============================================================================

# Read in player shots on target data
list_of_player_shots_on_target_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "player_shots_on_target")

# Read in all player shots on target data
list_of_player_shots_on_target_data <-
    map(list_of_player_shots_on_target_files, read_csv)

# Combine
player_shots_on_target_data <-
    list_of_player_shots_on_target_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, player_name, line, desc(over_price)) |> 
    select(match, player_name, line, over_price, under_price, agency) |>
    mutate(market_name = "Player Shots On Target")

# Write out
write_rds(player_shots_on_target_data, "Data/processed_odds/player_shots_on_target_data.rds")

#===============================================================================
# Player Tackles
#===============================================================================

# Read in player tackles data
list_of_player_tackles_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "player_tackles")

# Read in all player tackles data
list_of_player_tackles_data <-
    map(list_of_player_tackles_files, read_csv)

# Combine
player_tackles_data <-
    list_of_player_tackles_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, player_name, line, desc(over_price)) |> 
    select(match, home_team, away_team, player_name, line, over_price, under_price, agency) |> 
    mutate(market_name = "Player Tackles")

# Write out
write_rds(player_tackles_data, "Data/processed_odds/player_tackles_data.rds")

#===============================================================================
# Player Assists
#===============================================================================

# Read in player assists data
list_of_player_assists_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "player_assists")

# Read in all player assists data
list_of_player_assists_data <-
    map(list_of_player_assists_files, read_csv)

# Combine
player_assists_data <-
    list_of_player_assists_data |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    arrange(match, player_name, line, desc(over_price)) |> 
    select(match, home_team, away_team, player_name, line, over_price, under_price, agency) |>
    mutate(market_name = "Player Assists")

# Write out
write_rds(player_assists_data, "Data/processed_odds/player_assists_data.rds")
