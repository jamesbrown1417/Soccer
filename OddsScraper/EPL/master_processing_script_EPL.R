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
    select(-start_time)

# Best Home Win
best_home_win <-
    h2h_data |> 
    group_by(match) |> 
    arrange(match, desc(home_win)) |> 
    slice_head(n = 1) |> 
    select(-away_win, -draw, -margin) |> 
    rename(home_agency = agency)

# Best Draw
best_draw <-
    h2h_data |> 
    group_by(match) |> 
    arrange(match, desc(draw)) |> 
    slice_head(n = 1) |> 
    select(-home_win, -away_win, -margin) |> 
    rename(draw_agency = agency)

# Best Away Win
best_away_win <-
    h2h_data |> 
    group_by(match) |>
    arrange(match, desc(away_win)) |>
    slice_head(n = 1) |>
    select(-home_win, -margin, -draw) |>
    rename(away_agency = agency)

# Combine
best_h2h <-
    best_home_win |> 
    inner_join(best_draw) |>
    inner_join(best_away_win) |> 
    mutate(margin = 100*(round(1/home_win + 1/draw + 1/away_win, 3) - 1)) |> 
    arrange(margin)

#===============================================================================
# Both Teams To Score
#===============================================================================

# Read in both teams to score data
list_of_btts_files <- list.files("Data/scraped_odds/EPL", full.names = TRUE, pattern = "both_teams_to_score")

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

# Get Best Yes Price
best_yes_price <-
    btts_data |> 
    group_by(match) |> 
    arrange(match, desc(yes_price)) |> 
    slice_head(n = 1) |> 
    select(-no_price) |> 
    rename(yes_agency = agency) |> 
    select(match:yes_agency)

# Get Best No Price
best_no_price <-
    btts_data |> 
    group_by(match) |> 
    arrange(match, desc(no_price)) |> 
    slice_head(n = 1) |> 
    select(-yes_price) |> 
    rename(no_agency = agency) |> 
    select(match:no_agency)

# Combine
best_btts <-
    best_yes_price |> 
    inner_join(best_no_price) |> 
    mutate(margin = 100*(round(1/yes_price + 1/no_price, 3) - 1)) |> 
    arrange(margin)

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
    select(-start_time)

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
    select(-start_time)

# Get best overs
best_overs <-
    total_goals_data |> 
    group_by(match, line) |> 
    arrange(match, desc(over_price)) |> 
    slice_head(n = 1) |> 
    select(-under_price) |> 
    rename(over_agency = agency) |> 
    select(match:over_agency)

# Get best unders
best_unders <-
    total_goals_data |> 
    group_by(match, line) |> 
    arrange(match, line, desc(under_price)) |> 
    slice_head(n = 1) |> 
    select(-over_price) |> 
    rename(under_agency = agency) |> 
    select(match:under_agency)

# Combine
best_total_goals <-
    best_overs |> 
    inner_join(best_unders) |> 
    mutate(margin = 100*(round(1/over_price + 1/under_price, 3) - 1)) |> 
    arrange(margin)

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
    select(-start_time)

# Get best overs
best_team_overs <-
    team_goals_data |> 
    group_by(match, team, line) |> 
    arrange(match, team, desc(over_price)) |> 
    slice_head(n = 1) |> 
    select(-under_price) |> 
    rename(over_agency = agency) |> 
    select(match:over_agency)

# Get best unders
best_team_unders <-
    team_goals_data |> 
    group_by(match, team, line) |> 
    arrange(match, team, line, desc(under_price)) |> 
    slice_head(n = 1) |> 
    select(-over_price) |> 
    rename(under_agency = agency) |> 
    select(match:under_agency)

# Combine
best_team_goals <-
    best_team_overs |> 
    inner_join(best_team_unders) |> 
    mutate(margin = 100*(round(1/over_price + 1/under_price, 3) - 1)) |> 
    arrange(margin)
