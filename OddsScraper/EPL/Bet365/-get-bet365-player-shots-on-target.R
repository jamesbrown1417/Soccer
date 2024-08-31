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
get_player_shots_on_target <- function(scraped_file) {
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
  # Player Shots On Target Over / Under
  #=============================================================================
  
  # Get index for node with text "Player Shots On Target Over/Under"
  shots_on_target_over_under_index <- which(market_names == "Player Shots On Target Over/Under")
  
  # Get Player Names from node
  shots_on_target_players <-
      bet365_player_markets[[shots_on_target_over_under_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
  
  # Get Player Teams from node
  shots_on_target_teams <-
      bet365_player_markets[[shots_on_target_over_under_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Team") |>
      html_text()
  
  # Get Over Node Index
  shots_on_target_cols <-
      bet365_player_markets[[shots_on_target_over_under_index]] |>
      html_elements(".gl-Market_General")
  
  shots_on_target_over_index <- which(str_detect(shots_on_target_cols |> html_text(), "Over"))
  
  # Get Over Lines
  shots_on_target_over_lines <-
      shots_on_target_cols[[shots_on_target_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
      html_text()
  
  # Get Over Odds
  shots_on_target_over_odds <-
      shots_on_target_cols[[shots_on_target_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
  
  # Get Under Node Index
  shots_on_target_under_index <- which(str_detect(shots_on_target_cols |> html_text(), "Under"))
  
  # Get Under Odds
  shots_on_target_under_odds <-
      shots_on_target_cols[[shots_on_target_under_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
  
  # Create Player Shots On Target Table
  player_shots_on_target <-
      tibble(player = shots_on_target_players,
             team = shots_on_target_teams,
             line = as.numeric(shots_on_target_over_lines),
             over_price = as.numeric(shots_on_target_over_odds),
             under_price = as.numeric(shots_on_target_under_odds)) |>
      mutate(market_name = "Player Shots On Target Over/Under") |>
      mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Player Shots On Target
  #=============================================================================
  
  # Get index for node with text "Alternate Player Shots On Target"
  alternate_shots_on_target_index <- which(market_names == "Player Shots On Target")
  
  # Get Player Names from node
  alternate_shots_on_target_players <-
      bet365_player_markets[[alternate_shots_on_target_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
  
  # Get Player Teams from node
  alternate_shots_on_target_teams <-
      bet365_player_markets[[alternate_shots_on_target_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Team") |>
      html_text()
  
  # Get Shots On Target Node Indexes for 0.5 to 3.5
  alternate_shots_on_target_cols <-
      bet365_player_markets[[alternate_shots_on_target_index]] |>
      html_elements(".gl-Market_General")
  
  alternate_shots_on_target_05_index <- which(str_detect(alternate_shots_on_target_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "0\\.5"))
  alternate_shots_on_target_15_index <- which(str_detect(alternate_shots_on_target_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "1\\.5"))
  alternate_shots_on_target_25_index <- which(str_detect(alternate_shots_on_target_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "2\\.5"))
  alternate_shots_on_target_35_index <- which(str_detect(alternate_shots_on_target_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "3\\.5"))
  
  # Get Odds for each shots on target range
  alternate_shots_on_target_05_odds <-
      alternate_shots_on_target_cols[[alternate_shots_on_target_05_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
  
  alternate_shots_on_target_15_odds <-
      alternate_shots_on_target_cols[[alternate_shots_on_target_15_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
  
  alternate_shots_on_target_25_odds <-
      alternate_shots_on_target_cols[[alternate_shots_on_target_25_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
  
  alternate_shots_on_target_35_odds <-
      alternate_shots_on_target_cols[[alternate_shots_on_target_35_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
  
  # Create Alternate Player Shots On Target Tables
  alternate_shots_on_target_05 <-
      tibble(player = alternate_shots_on_target_players,
             team = alternate_shots_on_target_teams,
             line = 0.5,
             over_price = as.numeric(alternate_shots_on_target_05_odds)) |>
      mutate(market_name = "Alternate Player Shots On Target") |>
      mutate(agency = "Bet365")
  
  alternate_shots_on_target_15 <-
      tibble(player = alternate_shots_on_target_players,
             team = alternate_shots_on_target_teams,
             line = 1.5,
             over_price = as.numeric(alternate_shots_on_target_15_odds)) |>
      mutate(market_name = "Alternate Player Shots On Target") |>
      mutate(agency = "Bet365")
  
  alternate_shots_on_target_25 <-
      tibble(player = alternate_shots_on_target_players,
             team = alternate_shots_on_target_teams,
             line = 2.5,
             over_price = as.numeric(alternate_shots_on_target_25_odds)) |>
      mutate(market_name = "Alternate Player Shots On Target") |>
      mutate(agency = "Bet365")
  
  alternate_shots_on_target_35 <-
      tibble(player = alternate_shots_on_target_players,
             team = alternate_shots_on_target_teams,
             line = 3.5,
             over_price = as.numeric(alternate_shots_on_target_35_odds)) |>
      mutate(market_name = "Alternate Player Shots On Target") |>
      mutate(agency = "Bet365")
  
  # Combine
  alternate_player_shots_on_target <-
      bind_rows(alternate_shots_on_target_05, alternate_shots_on_target_15, alternate_shots_on_target_25, alternate_shots_on_target_35)
  
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
  player_shots_on_target_all <-
    bind_rows(player_shots_on_target, alternate_player_shots_on_target) |> 
    arrange(player, line, over_price) |> 
    mutate(market_name = "Player Shots On Target") |> 
    mutate(match = match_name) |> 
    relocate(match, .before = player)
  
  # Return
  return(player_shots_on_target_all)
}

# Create safe version of function
get_player_shots_on_target_safe <- safely(get_player_shots_on_target)

# Map Over all html files
list_of_player_shots_on_target <- map(scraped_files_player, get_player_shots_on_target_safe)

# Keep only successful results
list_of_player_shots_on_target <-
    list_of_player_shots_on_target |>
    # Keep if the error is null
    keep(~is.null(.x$error)) |>
    # Extract the result
    map_dfr("result")

# Combine into a df
all_player_shots_on_target <-
  list_of_player_shots_on_target |> 
  mutate(player = fix_player_names(player)) |> 
  rename(player_name = player, player_team = team)

# Output as a CSV
write_csv(all_player_shots_on_target, "Data/scraped_odds/EPL/bet365_player_shots_on_target.csv")
