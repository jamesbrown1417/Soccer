#===============================================================================
# Set up
#===============================================================================

# Libraries
library(worldfootballR)
library(tidyverse)

# Set up parallel processing
library(furrr)
plan(multisession)

# Read in fix team names function
source("Scripts/fix_team_names.R")

#===============================================================================
# EPL Squads
#===============================================================================

# Get all current EPL team urls
epl_team_urls <- tm_league_team_urls("England", start_year = 2024)

# Create a function that takes the URL and gets player name and player team
get_team_players <- function(url) {
    
    # Get players
    squad_players <- 
        tm_squad_stats(team_url = url)
    
    # Return only needed columns
    squad_players |> 
        select(player_name, player_team = team_name)
}

# Get all EPL squads by mapping over URL list in parallel
epl_squads <-
    epl_team_urls |>
    future_map_dfr(get_team_players, .progress = TRUE) |> 
    mutate(player_team = fix_team_names(player_team))

# Write out
write_rds(epl_squads, "Data/epl_squads.rds")