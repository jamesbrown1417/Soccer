# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/soccer/united-kingdom/english-premier-league"

# Get Squads
epl_squads <- read_rds("Data/epl_squads.rds")

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {
    # Get data from main market page
    matches <-
        sportsbet_url |>
        read_html() |>
        html_nodes(".White_fqa53j6")
    
    # Function to get team names
    get_team_names <- function(match) {
        team_names <-
            match |>
            html_nodes(".normal_fgzdi7m") |>
            html_text()
        
        # Home team and Away Team
        home_team <- team_names[1]
        away_team <- team_names[3]
        
        # Output
        tibble(home_team, away_team)
    }
    
    # Function to get odds
    get_odds <- function(match) {
        odds <-
            match |>
            html_nodes(".priceTextSize_frw9zm9") |>
            html_text() |>
            as.numeric()
        
        # Home team
        home_win <- odds[1]
        draw <- odds[2]
        away_win <- odds[3]
        
        # Output
        tibble(home_win, draw, away_win)
    }
    
    # Function to get start time
    get_start_time <- function(match) {
        start_time <-
            match |>
            html_nodes(".oneLine_f15ay66x") |>
            html_text()
        
        # Output
        tibble(start_time)
    }
    
    # Map functions to each match and combine together
    all_main_market_data <-
        bind_cols(
            map(matches, get_team_names) |> bind_rows(),
            map(matches, get_odds) |> bind_rows(),
            map(matches, get_start_time) |> bind_rows()
        )
    
    #===============================================================================
    # Head to Head markets---------------------------------------------------------#
    #===============================================================================
    
    sportsbet_h2h <-
        all_main_market_data |>
        mutate(home_team = fix_team_names(home_team)) |>
        mutate(away_team = fix_team_names(away_team)) |>
        mutate(match = paste(home_team, "v", away_team)) |>
        mutate(market_name = "Head To Head") |>
        mutate(home_win = as.numeric(home_win)) |>
        mutate(away_win = as.numeric(away_win)) |>
        select(match,
               market_name,
               home_team,
               home_win,
               draw,
               away_team,
               away_win) |>
        mutate(margin = round((1 / home_win + 1 / away_win + 1 / draw), digits = 3)) |>
        mutate(agency = "Sportsbet")
    
    # Write to csv
    write_csv(sportsbet_h2h, "Data/scraped_odds/EPL/sportsbet_h2h.csv")
    
}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {
    # Function to get team names
    get_team_names <- function(match) {
        team_names <-
            match |>
            html_nodes(".normal_fgzdi7m") |>
            html_text()
        
        # Home team and Away Team
        home_team <- team_names[1]
        away_team <- team_names[3]
        
        # Output
        tibble(home_team, away_team)
    }
    
    
    # Get match links
    match_links <-
        sportsbet_url |>
        read_html() |>
        html_nodes(".link_ft4u1lp") |>
        html_attr("href")
    
    # Get match IDs from links
    match_ids <-
        match_links |>
        str_extract("\\d{4,10}$") |>
        as.numeric()
    
    # Get data from main market page
    matches <-
        sportsbet_url |>
        read_html() |>
        html_nodes(".White_fqa53j6")
    
    # Get team names that correspond to each match link
    team_names <-
        map_dfr(matches, get_team_names) |>
        bind_cols("match_id" = match_ids) |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team))

    # Match info links
    match_info_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/SportCard?displayWinnersPriceMkt=true&includeLiveMarketGroupings=true&includeCollection=true"
    )
    
    # Top Markets
    top_market_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/84/Markets"
    )
    
    # Shot Market Links
    shot_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/638/Markets"
    )
    
    # Goal Scorer Markets
    goal_scorer_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/87/Markets"
    )
    
    # Pass Markets
    pass_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/792/Markets"
    )
    
    # Goal Markets
    goal_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/92/Markets"
    )
    
    # Card Markets
    card_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/100/Markets"
    )
    
    # Corner Markets
    corner_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/89/Markets"
    )
    
    # Goalkeeper Markets
    goalkeeper_links <- glue(
        "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/859/Markets"
    )
    
    # Get IDs needed for SGM engine-------------------------------------------------
    read_prop_url_metadata <- function(url) {
        # Make request and get response
        sb_response <-
            request(url) |>
            req_perform() |>
            resp_body_json()
        
        # Empty vectors to append to
        class_external_id = c()
        competition_external_id = c()
        event_external_id = c()
        
        # Append to vectors
        class_external_id = c(class_external_id, sb_response$classExternalId)
        competition_external_id = c(competition_external_id,
                                    sb_response$competitionExternalId)
        event_external_id = c(event_external_id, sb_response$externalId)
        
        # Output
        tibble(class_external_id,
               competition_external_id,
               event_external_id,
               url) |>
            mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
            rename(match_id = url) |>
            mutate(match_id = as.numeric(match_id))
    }
    
    # Safe version that just returns NULL if there is an error
    safe_read_prop_metadata <- safely(read_prop_url_metadata, otherwise = NULL)
    
    # Map function to player points urls
    player_prop_metadata <-
        map(match_info_links, safe_read_prop_metadata)
    
    # Get just result part from output
    player_prop_metadata <-
        player_prop_metadata |>
        map("result") |>
        map_df(bind_rows)
    
    # Function to read a url and get the player props-------------------------------
    
    read_prop_url <- function(url) {
        # Make request and get response
        sb_response <-
            request(url) |>
            req_perform() |>
            resp_body_json()
        
        # Empty vectors to append to
        prop_market_name = c()
        selection_name_prop = c()
        prop_market_selection = c()
        prop_market_price = c()
        player_id = c()
        market_id = c()
        handicap = c()
        
        # Loop through each market
        for (market in sb_response) {
            for (selection in market$selections) {
                # Append to vectors
                prop_market_name = c(prop_market_name, market$name)
                selection_name_prop = c(selection_name_prop, selection$name)
                prop_market_selection = c(prop_market_selection, selection$resultType)
                prop_market_price = c(prop_market_price, selection$price$winPrice)
                player_id = c(player_id, selection$externalId)
                market_id = c(market_id, market$externalId)
                if (is.null(selection$unformattedHandicap)) {
                    selection$unformattedHandicap = NA
                    handicap = c(handicap, selection$unformattedHandicap)
                } else {
                    selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
                    handicap = c(handicap, selection$unformattedHandicap)
                }
            }
        }
        
        # Output
        tibble(
            prop_market_name,
            selection_name_prop,
            prop_market_selection,
            prop_market_price,
            player_id,
            market_id,
            handicap,
            url
        )
        
    }
    
    # Safe version that just returns NULL if there is an error
    safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)
    
    #===========================================================================
    # Top Markets
    #===========================================================================
    
    # Map function to top market urls]
    top_market_data <-
        map(top_market_links, safe_read_prop_url)
    
    # Get just result part from output
    top_market_data <-
        top_market_data |>
        map("result") |>
        map_df(bind_rows)
    
    # Add market name
    top_market_data <-
        top_market_data |>
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
        rename(match_id = url) |>
        mutate(match_id = as.numeric(match_id)) |>
        left_join(team_names, by = "match_id") |>
        mutate(match = paste(home_team, "v", away_team)) |>
        left_join(player_prop_metadata)
    
    # Get Both Teams To Score---------------------------------------------------
    both_teams_to_score_data <-
        top_market_data |>
        filter(str_detect(prop_market_name, "^Both Teams To Score$"))
    
    # Get Both Teams To Score Yes
    both_teams_to_score_yes <-
        both_teams_to_score_data |>
        filter(str_detect(selection_name_prop, "Yes")) |> 
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Both Teams to Score",
            yes_price = prop_market_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    # Get Both Teams To Score No
    both_teams_to_score_no <-
        both_teams_to_score_data |>
        filter(str_detect(selection_name_prop, "No")) |> 
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Both Teams to Score",
            no_price = prop_market_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id_under = player_id
        )
    
    # Join Together
    both_teams_to_score_all <-
        both_teams_to_score_yes |> 
        left_join(both_teams_to_score_no) |> 
        relocate(no_price, .after = yes_price)
    
    #===========================================================================
    # Shot Markets
    #===========================================================================
    
    # Map function to shots urls
    shots_data <-
        map(shot_links, safe_read_prop_url)
    
    # Get just result part from output
    shots_data <-
        shots_data |>
        map("result") |>
        map_df(bind_rows)
    
    # Add market name
    shots_data <-
        shots_data |>
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
        rename(match_id = url) |>
        mutate(match_id = as.numeric(match_id)) |>
        left_join(team_names, by = "match_id") |>
        mutate(match = paste(home_team, "v", away_team)) |>
        left_join(player_prop_metadata)
    
    # Get player shots alternate lines---------------------------------------------
    player_shots_alternate <-
        shots_data |>
        filter(str_detect(prop_market_name, "^Player To Have [0-9] Or More Shots$")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
        mutate(line = as.numeric(line) - 0.5) |>
        rename(player_name = selection_name_prop) |>
        mutate(player_name = fix_player_names(player_name)) |>
        rename(over_price = prop_market_price) |>
        left_join(epl_squads) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
        relocate(match, .before = player_name) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Player Shots",
            player_name,
            player_team,
            opposition_team,
            line,
            over_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    # Get player shots on target alternate lines--------------------------------
    player_shots_on_target_alternate <-
        shots_data |>
        filter(str_detect(prop_market_name, "^Player To Have [0-9] Or More Shots On Target$")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
        mutate(line = as.numeric(line) - 0.5) |>
        rename(player_name = selection_name_prop) |>
        mutate(player_name = fix_player_names(player_name)) |>
        rename(over_price = prop_market_price) |>
        left_join(epl_squads) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
        relocate(match, .before = player_name) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Player Shots On Target",
            player_name,
            player_team,
            opposition_team,
            line,
            over_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    # Get team shots on target alternate lines----------------------------------
    team_shots_on_target_alternate <-
        shots_data |>
        filter(str_detect(prop_market_name, "^Team To Have [0-9]{1,2} Or More Shots On Target$")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
        mutate(line = as.numeric(line) - 0.5) |>
        rename(team = selection_name_prop) |>
        rename(over_price = prop_market_price) |>
        relocate(match, .before = team) |>
        mutate(team = fix_team_names(team)) |> 
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Team Shots On Target",
            team,
            line,
            over_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    #===========================================================================
    # Pass Markets
    #===========================================================================
    
    # Map function to pass urls
    pass_data <-
        map(pass_links, safe_read_prop_url)
    
    # Get just result part from output
    pass_data <-
        pass_data |>
        map("result") |>
        map_df(bind_rows)
    
    # Add market name
    pass_data <-
        pass_data |>
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
        rename(match_id = url) |>
        mutate(match_id = as.numeric(match_id)) |>
        left_join(team_names, by = "match_id") |>
        mutate(match = paste(home_team, "v", away_team)) |>
        left_join(player_prop_metadata)
    
    # Get player attempted passes alternate lines-------------------------------
    player_attempted_passes_alternate <-
        pass_data |>
        filter(str_detect(prop_market_name, "^Player To Attempt [0-9]{1,3} Or More Passes$")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
        mutate(line = as.numeric(line) - 0.5) |>
        rename(player_name = selection_name_prop) |>
        mutate(player_name = fix_player_names(player_name)) |>
        rename(over_price = prop_market_price) |>
        left_join(epl_squads) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
        relocate(match, .before = player_name) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Player Attempted Passes",
            player_name,
            player_team,
            opposition_team,
            line,
            over_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
        
    #===========================================================================
    # Goal Markets
    #===========================================================================
    
    # Map function to goal urls
    goal_data <-
        map(goal_links, safe_read_prop_url)
    
    # Get just result part from output
    goal_data <-
        goal_data |>
        map("result") |>
        map_df(bind_rows)
    
    # Add market name
    goal_data <-
        goal_data |>
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
        rename(match_id = url) |>
        mutate(match_id = as.numeric(match_id)) |>
        left_join(team_names, by = "match_id") |>
        mutate(match = paste(home_team, "v", away_team)) |>
        left_join(player_prop_metadata)
    
    # Get total goal lines------------------------------------------------------
    
    # Overs
    total_goals_overs <-
        goal_data |>
        filter(str_detect(prop_market_name, "^Over/Under [0-9\\.]{2,3} Goals$")) |>
        filter(str_detect(selection_name_prop, "Over")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}\\.\\d{1,2}")) |>
        mutate(line = as.numeric(line)) |>
        rename(over_price = prop_market_price) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Match Goals",
            line,
            over_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    # Unders
    total_goals_unders <-
        goal_data |>
        filter(str_detect(prop_market_name, "^Over/Under [0-9\\.]{2,3} Goals$")) |>
        filter(str_detect(selection_name_prop, "Under")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}\\.\\d{1,2}")) |>
        mutate(line = as.numeric(line)) |>
        rename(under_price = prop_market_price) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Match Goals",
            line,
            under_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id_under = player_id
        )
    
    # Join Together
    total_goals_all <-
        total_goals_overs |> 
        left_join(total_goals_unders) |> 
        relocate(under_price, .after = over_price)
    
    # Get home team goal lines--------------------------------------------------
    
    # Overs
    home_goals_overs <-
        goal_data |>
        filter(str_detect(prop_market_name, "^Home Team Over/Under [0-9\\.]{2,3}$")) |>
        filter(str_detect(selection_name_prop, "Over")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}\\.\\d{1,2}")) |>
        mutate(line = as.numeric(line)) |>
        mutate(team = home_team) |> 
        rename(over_price = prop_market_price) |>
        mutate(team = fix_team_names(team)) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Team Goals",
            team,
            line,
            over_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    # Unders
    home_goals_unders <-
        goal_data |>
        filter(str_detect(prop_market_name, "^Home Team Over/Under [0-9\\.]{2,3}$")) |>
        filter(str_detect(selection_name_prop, "Under")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}\\.\\d{1,2}")) |>
        mutate(line = as.numeric(line)) |>
        mutate(team = home_team) |> 
        rename(under_price = prop_market_price) |>
        mutate(team = fix_team_names(team)) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Team Goals",
            team,
            line,
            under_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id_under = player_id
        )
    
    # Join Together
    home_goals_all <-
        home_goals_overs |> 
        left_join(home_goals_unders) |> 
        relocate(under_price, .after = over_price)
    
    # Get away team goal lines--------------------------------------------------
    # Overs
    away_goals_overs <-
        goal_data |>
        filter(str_detect(prop_market_name, "^Away Team Over/Under [0-9\\.]{2,3}$")) |>
        filter(str_detect(selection_name_prop, "Over")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}\\.\\d{1,2}")) |>
        mutate(line = as.numeric(line)) |>
        mutate(team = away_team) |> 
        rename(over_price = prop_market_price) |>
        mutate(team = fix_team_names(team)) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Team Goals",
            team,
            line,
            over_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    # Unders
    away_goals_unders <-
        goal_data |>
        filter(str_detect(prop_market_name, "^Away Team Over/Under [0-9\\.]{2,3}$")) |>
        filter(str_detect(selection_name_prop, "Under")) |>
        mutate(line = str_extract(prop_market_name, "\\d{1,2}\\.\\d{1,2}")) |>
        mutate(line = as.numeric(line)) |>
        mutate(team = away_team) |> 
        rename(under_price = prop_market_price) |>
        mutate(team = fix_team_names(team)) |>
        transmute(
            match,
            home_team,
            away_team,
            market_name = "Team Goals",
            team,
            line,
            under_price,
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id_under = player_id
        )
    
    # Join Together
    away_goals_all <-
        away_goals_overs |> 
        left_join(away_goals_unders) |> 
        relocate(under_price, .after = over_price)
    
    # Join all goal lines together
    goal_lines_all <-
        home_goals_all |> 
        bind_rows(away_goals_all) |>
        arrange(match, team)
    
    #===========================================================================
    # Player Goal Markets
    #===========================================================================
    
    # Map function to goal urls
    player_goal_data <-
        map(goal_scorer_links, safe_read_prop_url)
    
    # Get just result part from output
    player_goal_data <-
        player_goal_data |>
        map("result") |>
        map_df(bind_rows)
    
    # Add market name
    player_goal_data <-
        player_goal_data |>
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
        rename(match_id = url) |>
        mutate(match_id = as.numeric(match_id)) |>
        left_join(team_names, by = "match_id") |>
        mutate(match = paste(home_team, "v", away_team)) |>
        left_join(player_prop_metadata)
    
    # Get player goals lines----------------------------------------------------
    player_goalscorer_lines <-
        player_goal_data |>
        filter(str_detect(prop_market_name, "^Anytime Goalscorer$|^To Score 2 or More Goals$|^To Score a Hat-Trick$")) |>
        rename(player_name = selection_name_prop) |>
        mutate(player_name = fix_player_names(player_name)) |>
        rename(price = prop_market_price) |>
        left_join(epl_squads) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
        relocate(match, .before = player_name) |>
        mutate(line = case_when(
            str_detect(prop_market_name, "To Score 2 or More Goals") ~ 1.5,
            str_detect(prop_market_name, "To Score a Hat-Trick") ~ 2.5,
            TRUE ~ 0.5
        )) |>
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
            agency = "Sportsbet",
            class_external_id,
            competition_external_id,
            event_external_id,
            market_id,
            player_id
        )
    
    #===========================================================================
    # Write all to CSV
    #===========================================================================
    
    # Both Teams To Score
    write_csv(both_teams_to_score_all, "Data/scraped_odds/EPL/sportsbet_both_teams_to_score.csv")
    
    # Player Shots
    write_csv(player_shots_alternate, "Data/scraped_odds/EPL/sportsbet_player_shots.csv")
    
    # Player Shots On Target
    write_csv(player_shots_on_target_alternate, "Data/scraped_odds/EPL/sportsbet_player_shots_on_target.csv")
    
    # Team Shots On Target
    write_csv(team_shots_on_target_alternate, "Data/scraped_odds/EPL/sportsbet_team_shots_on_target.csv")
    
    # Player Attempted Passes
    write_csv(player_attempted_passes_alternate, "Data/scraped_odds/EPL/sportsbet_player_attempted_passes.csv")
    
    # Match Goals
    write_csv(total_goals_all, "Data/scraped_odds/EPL/sportsbet_total_goals.csv")
    
    # Team Goals
    write_csv(goal_lines_all, "Data/scraped_odds/EPL/sportsbet_team_goals.csv")
    
    # Player Goals
    write_csv(player_goalscorer_lines, "Data/scraped_odds/EPL/sportsbet_player_goals.csv")
}

# Run Functions
main_markets_function()
player_props_function()