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

# Get Squads
epl_squads <- read_rds("Data/epl_squads.rds")

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
    # Alternate Player Tackles
    #=============================================================================
    
    # Get index for node with text "Player Tackles"
    alternate_tackles_index <- which(market_names == "Player Tackles")
    
    # Get Player Names from node
    alternate_tackles_players <-
        bet365_player_markets[[alternate_tackles_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()
    
    # # Get Player Teams from node
    # alternate_tackles_teams <-
    #     bet365_player_markets[[alternate_tackles_index]] |>
    #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
    #     html_text()
    # 
    # Get Tackles Node Indexes for 0.5 to 3.5
    alternate_tackles_cols <-
        bet365_player_markets[[alternate_tackles_index]] |>
        html_elements(".gl-Market_General")
    
    alternate_tackles_05_index <- which(str_detect(alternate_tackles_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "0\\.5"))
    alternate_tackles_15_index <- which(str_detect(alternate_tackles_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "1\\.5"))
    alternate_tackles_25_index <- which(str_detect(alternate_tackles_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "2\\.5"))
    alternate_tackles_35_index <- which(str_detect(alternate_tackles_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "3\\.5"))
    
    # Get Odds for each tackles range
    alternate_tackles_05_odds <-
        alternate_tackles_cols[[alternate_tackles_05_index]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
    
    alternate_tackles_15_odds <-
        alternate_tackles_cols[[alternate_tackles_15_index]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
    
    alternate_tackles_25_odds <-
        alternate_tackles_cols[[alternate_tackles_25_index]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
    
    alternate_tackles_35_odds <-
        alternate_tackles_cols[[alternate_tackles_35_index]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
    
    # Create Alternate Player Tackles Tables
    alternate_tackles_05 <-
        tibble(player = alternate_tackles_players,
               # team = alternate_tackles_teams,
               line = 0.5,
               over_price = as.numeric(alternate_tackles_05_odds)) |>
        mutate(market_name = "Alternate Player Tackles") |>
        mutate(agency = "Bet365")
    
    alternate_tackles_15 <-
        tibble(player = alternate_tackles_players,
               # team = alternate_tackles_teams,
               line = 1.5,
               over_price = as.numeric(alternate_tackles_15_odds)) |>
        mutate(market_name = "Alternate Player Tackles") |>
        mutate(agency = "Bet365")
    
    alternate_tackles_25 <-
        tibble(player = alternate_tackles_players,
               # team = alternate_tackles_teams,
               line = 2.5,
               over_price = as.numeric(alternate_tackles_25_odds)) |>
        mutate(market_name = "Alternate Player Tackles") |>
        mutate(agency = "Bet365")
    
    alternate_tackles_35 <-
        tibble(player = alternate_tackles_players,
               # team = alternate_tackles_teams,
               line = 3.5,
               over_price = as.numeric(alternate_tackles_35_odds)) |>
        mutate(market_name = "Alternate Player Tackles") |>
        mutate(agency = "Bet365")
    
    # Combine
    alternate_player_tackles <-
        bind_rows(alternate_tackles_05, alternate_tackles_15, alternate_tackles_25, alternate_tackles_35)
    
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
        bind_rows(alternate_player_tackles) |> 
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
    left_join(epl_squads, by = c("player" = "player_name")) |>
    rename(player_name = player) |> 
    mutate(player_team = fix_team_names(player_team))

# Output as a csv
write_csv(all_player_tackles, "Data/scraped_odds/EPL/bet365_player_tackles.csv")