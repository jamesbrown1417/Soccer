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
get_player_assists <- function(scraped_file) {
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
    # Player Assists Over / Under
    #=============================================================================
    
    # Get index for node with text "Player Assists Over/Under"
    assists_over_under_index <- which(market_names == "Player Assists")
    
    # Get Player Names from node
    assists_players <-
        bet365_player_markets[[assists_over_under_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()
    
    # Get Player Teams from node
    assists_teams <-
        bet365_player_markets[[assists_over_under_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Team") |>
        html_text()
    
    # Get Over Node Index
    assists_cols <-
        bet365_player_markets[[assists_over_under_index]] |>
        html_elements(".gl-Market_General")
    
    assists_over_index <- which(str_detect(assists_cols |> html_text(), "Over"))
    
    # Get Over Lines
    assists_over_lines <-
        assists_cols[[assists_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
        html_text()
    
    # Get Over Odds
    assists_over_odds <-
        assists_cols[[assists_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()
    
    # Get Under Node Index
    assists_under_index <- which(str_detect(assists_cols |> html_text(), "Under"))
    
    # Get Under Odds
    assists_under_odds <-
        assists_cols[[assists_under_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()
    
    # Create Player Assists Table
    player_assists <-
        tibble(player = assists_players,
               team = assists_teams,
               line = as.numeric(assists_over_lines),
               over_price = as.numeric(assists_over_odds),
               under_price = as.numeric(assists_under_odds)) |>
        mutate(market_name = "Player Assists Over/Under") |>
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
    player_assists_all <-
        bind_rows(player_assists) |> 
        arrange(player, line, over_price) |> 
        mutate(market_name = "Player Assists") |> 
        mutate(match = match_name) |> 
        relocate(match, .before = player)
    
    # Return
    return(player_assists_all)
}

# Create safe version of function
get_player_assists_safe <- safely(get_player_assists)

# Map Over all html files
list_of_player_assists <- map(scraped_files_player, get_player_assists_safe)

# Keep only successful results
list_of_player_assists <-
    list_of_player_assists |>
    # Keep if the error is null
    keep(~is.null(.x$error)) |>
    # Extract the result
    map_dfr("result")

# Combine into a df
all_player_assists <-
    list_of_player_assists |> 
    mutate(player = fix_player_names(player)) |> 
    rename(player_name = player, player_team = team) |> 
    mutate(player_team = fix_team_names(player_team))

# Output as a csv
write_csv(all_player_assists, "Data/scraped_odds/EPL/bet365_player_assists.csv")