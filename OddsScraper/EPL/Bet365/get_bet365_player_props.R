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

#===============================================================================
# Main Function to Process All Markets
#===============================================================================

get_all_player_props <- function(scraped_file) {
  # Get Markets
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")

  # Market Names
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()

  # Get teams
  team_names <-
    scraped_file |>
    read_html() |>
    html_nodes(".sph-FixturePodHeader_TeamName ") |>
    html_text()

  team_names <- fix_team_names(team_names)

  # Get Match Name
  match_name <- paste(team_names, collapse = " v ")

  # Initialize list to store all markets
  all_markets <- list()

  #=============================================================================
  # Player Assists
  #=============================================================================

  if ("Player Assists" %in% market_names) {
    tryCatch({
      assists_over_under_index <- which(market_names == "Player Assists")

      assists_players <-
        bet365_player_markets[[assists_over_under_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()

      assists_cols <-
        bet365_player_markets[[assists_over_under_index]] |>
        html_elements(".gl-Market_General")

      assists_over_index <- which(str_detect(assists_cols |> html_text(), "Over"))

      assists_over_lines <-
        assists_cols[[assists_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
        html_text()

      assists_over_odds <-
        assists_cols[[assists_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()

      assists_under_index <- which(str_detect(assists_cols |> html_text(), "Under"))

      assists_under_odds <-
        assists_cols[[assists_under_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()

      player_assists <-
        tibble(player = assists_players,
               line = as.numeric(assists_over_lines),
               over_price = as.numeric(assists_over_odds),
               under_price = as.numeric(assists_under_odds)) |>
        mutate(market_name = "Player Assists",
               agency = "Bet365",
               match = match_name)

      all_markets$assists <- player_assists
    }, error = function(e) {
      message("Error processing Player Assists: ", e$message)
    })
  }

  #=============================================================================
  # Player Shots Over / Under
  #=============================================================================

  if ("Player Shots Over/Under" %in% market_names) {
    tryCatch({
      shots_over_under_index <- which(market_names == "Player Shots Over/Under")

      shots_players <-
        bet365_player_markets[[shots_over_under_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()

      shots_cols <-
        bet365_player_markets[[shots_over_under_index]] |>
        html_elements(".gl-Market_General")

      shots_over_index <- which(str_detect(shots_cols |> html_text(), "Over"))

      shots_over_lines <-
        shots_cols[[shots_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
        html_text()

      shots_over_odds <-
        shots_cols[[shots_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()

      shots_under_index <- which(str_detect(shots_cols |> html_text(), "Under"))

      shots_under_odds <-
        shots_cols[[shots_under_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()

      player_shots <-
        tibble(player = shots_players,
               line = as.numeric(shots_over_lines),
               over_price = as.numeric(shots_over_odds),
               under_price = as.numeric(shots_under_odds)) |>
        mutate(market_name = "Player Shots",
               agency = "Bet365",
               match = match_name)

      all_markets$shots <- player_shots
    }, error = function(e) {
      message("Error processing Player Shots Over/Under: ", e$message)
    })
  }

  #=============================================================================
  # Alternate Player Shots
  #=============================================================================

  if ("Player Shots" %in% market_names) {
    tryCatch({
      alternate_shots_index <- which(market_names == "Player Shots")

      alternate_shots_players <-
        bet365_player_markets[[alternate_shots_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()

      alternate_shots_cols <-
        bet365_player_markets[[alternate_shots_index]] |>
        html_elements(".gl-Market_General")

      # Try to find all available lines
      all_line_headers <- alternate_shots_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text()

      # Extract unique line values
      unique_lines <- unique(na.omit(as.numeric(str_extract(all_line_headers, "\\d+\\.\\d+"))))

      # Process each line
      for (line_val in unique_lines) {
        line_index <- which(str_detect(all_line_headers, paste0(line_val)))

        if (length(line_index) > 0) {
          line_odds <-
            alternate_shots_cols[[line_index[1]]] |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()

          temp_df <- tibble(
            player = alternate_shots_players,
            line = line_val,
            over_price = as.numeric(line_odds)
          ) |>
            mutate(market_name = "Player Shots",
                   agency = "Bet365",
                   match = match_name)

          all_markets[[paste0("alt_shots_", line_val)]] <- temp_df
        }
      }
    }, error = function(e) {
      message("Error processing Alternate Player Shots: ", e$message)
    })
  }

  #=============================================================================
  # Player Shots On Target Over / Under
  #=============================================================================

  if ("Player Shots On Target Over/Under" %in% market_names) {
    tryCatch({
      shots_on_target_over_under_index <- which(market_names == "Player Shots On Target Over/Under")

      shots_on_target_players <-
        bet365_player_markets[[shots_on_target_over_under_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()

      shots_on_target_cols <-
        bet365_player_markets[[shots_on_target_over_under_index]] |>
        html_elements(".gl-Market_General")

      shots_on_target_over_index <- which(str_detect(shots_on_target_cols |> html_text(), "Over"))

      shots_on_target_over_lines <-
        shots_on_target_cols[[shots_on_target_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
        html_text()

      shots_on_target_over_odds <-
        shots_on_target_cols[[shots_on_target_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()

      shots_on_target_under_index <- which(str_detect(shots_on_target_cols |> html_text(), "Under"))

      shots_on_target_under_odds <-
        shots_on_target_cols[[shots_on_target_under_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()

      player_shots_on_target <-
        tibble(player = shots_on_target_players,
               line = as.numeric(shots_on_target_over_lines),
               over_price = as.numeric(shots_on_target_over_odds),
               under_price = as.numeric(shots_on_target_under_odds)) |>
        mutate(market_name = "Player Shots On Target",
               agency = "Bet365",
               match = match_name)

      all_markets$shots_on_target <- player_shots_on_target
    }, error = function(e) {
      message("Error processing Player Shots On Target Over/Under: ", e$message)
    })
  }

  #=============================================================================
  # Alternate Player Shots On Target
  #=============================================================================

  if ("Player Shots On Target" %in% market_names) {
    tryCatch({
      alternate_shots_on_target_index <- which(market_names == "Player Shots On Target")

      alternate_shots_on_target_players <-
        bet365_player_markets[[alternate_shots_on_target_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()

      alternate_shots_on_target_cols <-
        bet365_player_markets[[alternate_shots_on_target_index]] |>
        html_elements(".gl-Market_General")

      # Try to find all available lines
      all_line_headers <- alternate_shots_on_target_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text()

      # Extract unique line values
      unique_lines <- unique(na.omit(as.numeric(str_extract(all_line_headers, "\\d+\\.\\d+"))))

      # Process each line
      for (line_val in unique_lines) {
        line_index <- which(str_detect(all_line_headers, paste0(line_val)))

        if (length(line_index) > 0) {
          line_odds <-
            alternate_shots_on_target_cols[[line_index[1]]] |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()

          temp_df <- tibble(
            player = alternate_shots_on_target_players,
            line = line_val,
            over_price = as.numeric(line_odds)
          ) |>
            mutate(market_name = "Player Shots On Target",
                   agency = "Bet365",
                   match = match_name)

          all_markets[[paste0("alt_shots_on_target_", line_val)]] <- temp_df
        }
      }
    }, error = function(e) {
      message("Error processing Alternate Player Shots On Target: ", e$message)
    })
  }

  #=============================================================================
  # Player Tackles
  #=============================================================================

  if ("Player Tackles" %in% market_names) {
    tryCatch({
      alternate_tackles_index <- which(market_names == "Player Tackles")

      alternate_tackles_players <-
        bet365_player_markets[[alternate_tackles_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()

      alternate_tackles_cols <-
        bet365_player_markets[[alternate_tackles_index]] |>
        html_elements(".gl-Market_General")

      # Try to find all available lines
      all_line_headers <- alternate_tackles_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text()

      # Extract unique line values
      unique_lines <- unique(na.omit(as.numeric(str_extract(all_line_headers, "\\d+\\.\\d+"))))

      # Process each line
      for (line_val in unique_lines) {
        line_index <- which(str_detect(all_line_headers, paste0(line_val)))

        if (length(line_index) > 0) {
          line_odds <-
            alternate_tackles_cols[[line_index[1]]] |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()

          temp_df <- tibble(
            player = alternate_tackles_players,
            line = line_val,
            over_price = as.numeric(line_odds)
          ) |>
            mutate(market_name = "Player Tackles",
                   agency = "Bet365",
                   match = match_name)

          all_markets[[paste0("tackles_", line_val)]] <- temp_df
        }
      }
    }, error = function(e) {
      message("Error processing Player Tackles: ", e$message)
    })
  }

  #=============================================================================
  # Multi Scorers (Goals)
  #=============================================================================

  if ("Multi Scorers" %in% market_names) {
    tryCatch({
      multi_scorers_index <- which(market_names == "Multi Scorers")

      multi_scorers_players <-
        bet365_player_markets[[multi_scorers_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()

      multi_scorers_cols <-
        bet365_player_markets[[multi_scorers_index]] |>
        html_elements(".gl-Market_General")

      # Try to find all available lines (typically 2+, 3+, etc.)
      all_line_headers <- multi_scorers_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text()

      # Extract unique line values (look for patterns like "2+", "3+")
      unique_lines <- unique(na.omit(str_extract(all_line_headers, "\\d+")))

      # Process each line
      for (line_str in unique_lines) {
        line_index <- which(str_detect(all_line_headers, paste0(line_str, "\\+")))

        if (length(line_index) > 0) {
          line_odds <-
            multi_scorers_cols[[line_index[1]]] |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()

          temp_df <- tibble(
            player = multi_scorers_players,
            line = as.numeric(line_str) - 0.5,  # Convert "2+" to 1.5, "3+" to 2.5, etc.
            over_price = as.numeric(line_odds)
          ) |>
            mutate(market_name = "Player Goals",
                   agency = "Bet365",
                   match = match_name)

          all_markets[[paste0("goals_", line_str)]] <- temp_df
        }
      }
    }, error = function(e) {
      message("Error processing Multi Scorers: ", e$message)
    })
  }

  # Combine all markets
  if (length(all_markets) > 0) {
    combined_data <- bind_rows(all_markets) |>
      arrange(market_name, player, line) |>
      relocate(match, .before = player)

    return(combined_data)
  } else {
    return(NULL)
  }
}

#===============================================================================
# Process All Files
#===============================================================================

# Create safe version of function
get_all_player_props_safe <- safely(get_all_player_props)

# Map Over all html files
list_of_all_props <- map(scraped_files_player, get_all_player_props_safe)

# Keep only successful results
all_props_combined <-
  list_of_all_props |>
  # Keep if the error is null
  keep(~is.null(.x$error)) |>
  # Extract the result
  map_dfr("result")

# Add player names and teams
all_props_final <-
  all_props_combined |>
  mutate(player = fix_player_names(player)) |>
  left_join(epl_squads, by = c("player" = "player_name")) |>
  rename(player_name = player) |>
  mutate(player_team = fix_team_names(player_team))

#===============================================================================
# Split by Market and Output
#===============================================================================

# Split data by market
split_markets <- all_props_final |> split(all_props_final$market_name)

# Output each market to a separate CSV
for (market in names(split_markets)) {
  filename <- str_replace_all(str_to_lower(market), " ", "_")
  write_csv(split_markets[[market]], glue("Data/scraped_odds/EPL/bet365_{filename}.csv"))
  message("Saved: bet365_{filename}.csv")
}

# Also output a combined file
write_csv(all_props_final, "Data/scraped_odds/EPL/bet365_all_player_props.csv")
message("Saved: bet365_all_player_props.csv")
