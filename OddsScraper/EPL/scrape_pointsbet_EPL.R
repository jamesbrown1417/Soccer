# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

# Get Squads
epl_squads <- read_rds("Data/epl_squads.rds")

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

pointsbet_h2h_main <- function() {
    # URL of website
    pointsbet_url = "https://api.pointsbet.com/api/v2/competitions/136535/events/featured?includeLive=false&page=1"
    
    # Make request and get response
    pointsbet_response <-
        request(pointsbet_url) |>
        req_perform() |>
        resp_body_json()
    
    # List of matches and data
    events <- pointsbet_response$events
    
    # Loop through to get all data--------------------------------------------------
    
    # Create empty vectors
    match_names <- c()
    match_starts_at <- c()
    home_teams <- c()
    away_teams <- c()
    event_names <- c()
    outcome_names <- c()
    outcome_prices <- c()
    keys <- c()
    
    # Loop through events
    for (match in events) {
        for (market in match$fixedOddsMarkets) {
            for (outcome in market$outcomes) {
                # Append data to vectors
                match_names <- c(match_names, match$name)
                match_starts_at <- c(match_starts_at, match$startsAt)
                home_teams <- c(home_teams, match$homeTeam)
                away_teams <- c(away_teams, match$awayTeam)
                event_names <- c(event_names, market$eventName)
                outcome_names <- c(outcome_names, outcome$name)
                outcome_prices <- c(outcome_prices, outcome$price)
                keys <- c(keys, match$key)
            }
        }
    }
    
    # Output tibble
    pointsbet_data <-
        tibble(
            match = match_names,
            start_time = match_starts_at,
            home_team = home_teams,
            away_team = away_teams,
            event = event_names,
            outcome = outcome_names,
            price = outcome_prices
        ) |>
        mutate(match = paste(home_team, "v", away_team)) |>
        relocate(match, .before = start_time) |> 
        mutate(across(everything(), str_squish))
    
    #===============================================================================
    # Head to head markets
    #===============================================================================
    
    # Filter to head to head markets
    pointsbet_data_h2h <-
        pointsbet_data |>
        filter(event == "MATCH RESULT")
    
    # Home Teams
    pointsbet_data_h2h_home <-
        pointsbet_data_h2h |>
        filter(home_team == outcome) |>
        select(match,
               start_time,
               market = event,
               home_team,
               home_win = price) |> 
        mutate(home_team = fix_team_names(home_team))
    
    # Draws
    pointsbet_data_h2h_draw <-
        pointsbet_data_h2h |>
        filter(outcome == "Draw") |>
        select(match,
               start_time,
               market = event,
               draw = price)
    
    # Away Teams
    pointsbet_data_h2h_away <-
        pointsbet_data_h2h |>
        filter(away_team == outcome) |>
        select(match,
               start_time,
               market = event,
               away_team,
               away_win = price) |> 
        mutate(away_team = fix_team_names(away_team))
    
    # Combine
    pointsbet_h2h <-
        full_join(
            pointsbet_data_h2h_home,
            pointsbet_data_h2h_away,
            by = c("match", "start_time", "market")
        ) |>
        left_join(pointsbet_data_h2h_draw, by = c("match", "start_time", "market")) |>
        mutate(match = paste(home_team, "v", away_team)) |>
        mutate(market = "Head To Head") |>
        select(match,
               start_time,
               market_name = market,
               home_team,
               home_win,
               draw,
               away_team,
               away_win) |>
        mutate(home_win = as.numeric(home_win),
               draw = as.numeric(draw),
               away_win = as.numeric(away_win)) |>
        mutate(margin = round((1 / home_win + 1/draw + 1 / away_win), digits = 3)) |>
        mutate(agency = "Pointsbet")
    
    # Write to csv
    write_csv(pointsbet_h2h, "Data/scraped_odds/EPL/pointsbet_h2h.csv")
    
    #===============================================================================
    # Player Props
    #===============================================================================
    
    # Get unique keys
    keys <- unique(keys)
    
    # Get each match's api page
    match_urls <-
        paste0("https://api.au.pointsbet.com/api/mes/v3/events/", keys)
    
    # Create a function that gets the player props from each URL
    get_player_props <- function(url) {
        # Make request and get response
        pointsbet_response <-
            request(url) |>
            req_perform() |>
            resp_body_json()
        
        # Loop through to get prop data---------------------------------------------
        
        # Create empty vectors
        match_names <- c()
        market_names <- c()
        outcome_names <- c()
        outcome_types <- c()
        outcome_prices <- c()
        headers <- c()
        event_key <- c()
        market_key <- c()
        outcome_key <- c()
        
        # Loop through events
        for (market in pointsbet_response$fixedOddsMarkets) {
            for (outcome in market$outcomes) {
                # Append data to vectors
                match_names <- c(match_names, pointsbet_response$name)
                
                if (!is.null(market$name)) {
                    market_names <- c(market_names, market$name)
                } else {
                    market_names <- c(market_names, NA)
                }
                
                if (!is.null(outcome$name)) {
                    outcome_names <- c(outcome_names, outcome$name)
                } else {
                    outcome_names <- c(outcome_names, NA)
                }
                
                if (!is.null(outcome$groupByHeader)) {
                    headers <- c(headers, outcome$groupByHeader)
                } else {
                    headers <- c(headers, NA)
                }

                if (!is.null(outcome$outcomeType)) {
                    outcome_types <- c(outcome_types, outcome$outcomeType)
                } else {
                    outcome_types <- c(outcome_types, NA)
                }
                
                if (!is.null(outcome$price)) {
                    outcome_prices <- c(outcome_prices, outcome$price)
                } else {
                    outcome_prices <- c(outcome_prices, NA)
                }
                
                event_key <- c(event_key, pointsbet_response$key)
                
                if (!is.null(market$key)) {
                    market_key <- c(market_key, market$key)
                } else {
                    market_key <- c(market_key, NA)
                }
                
                if (!is.null(outcome$key)) {
                    outcome_key <- c(outcome_key, outcome$key)
                } else {
                    outcome_key <- c(outcome_key, NA)
                }
            }
        }
        
        # Output tibble
        tibble(
            match = match_names,
            market = market_names,
            headers = headers,
            outcome = outcome_names,
            outcome_type = outcome_types,
            price = outcome_prices,
            EventKey = event_key,
            MarketKey = market_key,
            OutcomeKey = outcome_key
        )
    }
    
    # Map function to each URL
    pointsbet_data_player_props <- map_df(match_urls, get_player_props)
    
    #===============================================================================
    # Player Shots
    #===============================================================================
    
    # Player Shots alternative totals-----------------------------------------------
    
    # Filter list to player shots
    pointsbet_player_shots_lines <-
        pointsbet_data_player_props |>
        filter(str_detect(market, "PLAYER TOTAL SHOTS \\(")) |>
        mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
        mutate(line = as.numeric(line) - 0.5) |>
        separate(
            match,
            into = c("home_team", "away_team"),
            sep = " v ",
            remove = FALSE
        ) |>
        mutate(home_team = fix_team_names(home_team),
               away_team = fix_team_names(away_team)) |>
        mutate(match = paste(home_team, "v", away_team)) |>
        mutate(player_name = str_remove_all(outcome, " To Have.*")) |> 
        left_join(epl_squads) |> 
        mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Player Shots",
            player_name,
            player_team,
            opposition_team,
            line,
            over_price = price,
            agency = "Pointsbet",
            EventKey,
            MarketKey,
            OutcomeKey
        )
    
    #===============================================================================
    # Player Shots On Target
    #===============================================================================
    
    # Player Shots On Target alternative totals-----------------------------------------------
    
    # Filter list to player shots on target
    pointsbet_player_shots_on_target_lines <-
        pointsbet_data_player_props |>
        filter(str_detect(market, "PLAYER TOTAL SHOTS ON TARGET \\(")) |>
        mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
        mutate(line = as.numeric(line) - 0.5) |>
        separate(
            match,
            into = c("home_team", "away_team"),
            sep = " v ",
            remove = FALSE
        ) |>
        mutate(home_team = fix_team_names(home_team),
               away_team = fix_team_names(away_team)) |>
        mutate(match = paste(home_team, "v", away_team)) |>
        mutate(player_name = str_remove_all(outcome, " To Have.*")) |> 
        left_join(epl_squads) |> 
        mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Player Shots On Target",
            player_name,
            player_team,
            opposition_team,
            line,
            over_price = price,
            agency = "Pointsbet",
            EventKey,
            MarketKey,
            OutcomeKey
        )
    
    #===============================================================================
    # Player Goals
    #===============================================================================
    
    # Player Goals Alternative Lines------------------------------------------------
    # Filter list to player goals
    pointsbet_player_goals <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "ANYTIME GOALSCORER|TO SCORE 2\\+|HAT-TRICK")) |> 
    mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
    mutate(line = case_when(
        str_detect(market, "TO SCORE 2") ~ 1.5,
        str_detect(market, "HAT-TRICK") ~ 2.5,
        TRUE ~ 0.5
    )) |> 
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove_all(outcome, " To Have.*")) |> 
    left_join(epl_squads) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Goals",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

    #===============================================================================
    # Write to CSV
    #===============================================================================
    
    # Shots
    pointsbet_player_shots_lines |>
        mutate(match = paste(home_team, away_team, sep = " v ")) |>
        select(any_of(c(
            "match",
            "home_team",
            "away_team",
            "market_name",
            "player_name",
            "player_team",
            "line",
            "over_price",
            "under_price",
            "agency",
            "opposition_team",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
        ))) |>
        mutate(agency = "Pointsbet") |>
        write_csv("Data/scraped_odds/EPL/pointsbet_player_shots.csv")
}

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

# This runs both the props and head to head as they use same info
h2h_safe_pointsbet <- safely(pointsbet_h2h_main)

# Run functions
h2h_safe_pointsbet()
