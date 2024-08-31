# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("OddsScraper/EPL/Bet365/HTML", full.names = TRUE, pattern = "player")

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

# Main Function
get_player_tackles <- function(scraped_file) {
    # Get Markets
    bet365_player_markets <-
        read_html(scraped_file) |>
        html_nodes(".gl-MarketGroupPod")
    
    # Market Names
    market_names <-
        bet365_player_markets |>
        html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
        html_text()
    
    #=============================================================================
    # Player Tackles Over / Under
    #=============================================================================
    
    # Get index for node with text "Player Tackles Over/Under"
    tackles_over_under_index <- which(market_names == "Player Tackles")
    
    # Get Player Names from node
    tackles_players <-
        bet365_player_markets[[tackles_over_under_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()
    
    # Get Player Teams from node
    tackles_teams <-
        bet365_player_markets[[tackles_over_under_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Team") |>
        html_text()
    
    # Get Over Node Index
    tackles_cols <-
        bet365_player_markets[[tackles_over_under_index]] |>
        html_elements(".gl-Market_General")
    
    tackles_over_index <- which(str_detect(tackles_cols |> html_text(), "Over"))
    
    # Get Over Lines
    tackles_over_lines <-
        tackles_cols[[tackles_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
        html_text()
    
    # Get Over Odds
    tackles_over_odds <-
        tackles_cols[[tackles_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()
    
    # Get Under Node Index
    tackles_under_index <- which(str_detect(tackles_cols |> html_text(), "Under"))
    
    # Get Under Odds
    tackles_under_odds <-
        tackles_cols[[tackles_under_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()
    
    # Create Player Tackles Table
    player_tackles <-
        tibble(player = tackles_players,
               team = tackles_teams,
               line = as.numeric(tackles_over_lines),
               over_price = as.numeric(tackles_over_odds),
               under_price = as.numeric(tackles_under_odds)) |>
        mutate(market_name = "Player Tackles Over/Under") |>
        mutate(agency = "Bet365")
    
    #=============================================================================
    # Combine Lines and milestones
    #=============================================================================
    
    # Get teams
    team_names <-
        scraped_file |> 
        read_html() |> 
        html_nodes(".sph-FixturePodHeader_TeamName ") |> 
        html_text()
    
    team_names <- fix_team_names(team_names)
    
    # Get Match Name
    match_name <- paste(team_names, collapse = " v ")
    
    # Combine all tables
    player_tackles_all <-
        bind_rows(player_tackles) |> 
        arrange(player, line, over_price) |> 
        mutate(market_name = "Player Tackles") |> 
        mutate(match = match_name) |> 
        relocate(match, .before = player)
    
    # Return
    return(player_tackles_all)
}

# Create safe version of function
get_player_tackles_safe <- safely(get_player_tackles)

# Map Over all html files
list_of_player_tackles <- map(scraped_files_player, get_player_tackles_safe)

# Keep only successful results
list_of_player_tackles <-
    list_of_player_tackles |>
    # Keep if the error is null
    keep(~is.null(.x$error)) |>
    # Extract the result
    map_dfr("result")

# Combine into a df
all_player_tackles <-
    list_of_player_tackles |> 
    mutate(player = fix_player_names(player)) |> 
    rename(player_name = player, player_team = team)

# Output as a csv
write_csv(all_player_tackles, "Data/scraped_odds/EPL/bet365_player_tackles.csv")
