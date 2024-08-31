# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(httr)
library(jsonlite)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/Soccer/competitions/English%20Premier%20League?jurisdiction=SA"

# Get Squads
epl_squads <- read_rds("Data/epl_squads.rds")

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

# Set the headers
headers <- c(
    "accept" = "application/json, text/plain, */*",
    "accept-language" = "en-US,en;q=0.9",
    "origin" = "https://www.tab.com.au",
    "referer" = "https://www.tab.com.au/",
    "sec-ch-ua" = '"Not/A)Brand";v="8", "Chromium";v="126", "Google Chrome";v="126"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = '"Windows"',
    "sec-fetch-dest" = "empty",
    "sec-fetch-mode" = "cors",
    "sec-fetch-site" = "same-site",
    "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
)

# Try response, if nothing in 3 seconds, make it null
response <- tryCatch({
    GET(tab_url, add_headers(.headers = headers), timeout(5))
}, error = function(e) {
    return(NULL)
})

# Get response body
tab_response <- content(response, as = "parsed")

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name, propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions
    )
}

# Map functions to data
all_tab_markets <-
    map(tab_response$matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
    all_tab_markets |>
    unnest_wider(col = propositions, names_sep = "_") |>
    select(any_of(c("match", "start_time", "market_name")),
           prop_name = propositions_name,
           prop_id = propositions_id,
           price = propositions_returnWin)

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(market_name == "Result") |>
    group_by(match) |>
    filter(row_number() == 1) |>
    rename(home_win = price) |>
    select(-prop_name, -prop_id)

# Draw
draw_h2h <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(market_name == "Result") |>
    group_by(match) |>
    filter(row_number() == 2) |>
    rename(draw = price) |>
    select(-prop_name, -prop_id)

# Away teams
away_teams <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(market_name == "Result") |>
    group_by(match) |>
    filter(row_number() == 3) |>
    rename(away_win = price) |>
    select(-prop_name, -prop_id)

# Combine
tab_head_to_head_markets <-
    home_teams |>
    left_join(draw_h2h) |>
    left_join(away_teams) |>
    select(match,
           start_time,
           market_name,
           home_team,
           home_win,
           draw,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win + 1 / draw), digits = 3)) |>
    mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
    tab_head_to_head_markets |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head")

# Write to csv
write_csv(tab_head_to_head_markets, "Data/scraped_odds/EPL/tab_h2h.csv")

#===============================================================================
# Match Total Goals Markets
#===============================================================================

# Overs
totals_overs <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(str_detect(market_name, "Total Goals Over/Under")) |> 
    filter(str_detect(prop_name, "Over")) |>
    mutate(line = str_extract(prop_name, "\\d+\\.\\d+")) |> 
    rename(over_price = price) |>
    select(-prop_name, -prop_id)

# Unders
totals_unders <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(str_detect(market_name, "Total Goals Over/Under")) |> 
    filter(str_detect(prop_name, "Under")) |>
    mutate(line = str_extract(prop_name, "\\d+\\.\\d+")) |> 
    rename(under_price = price) |>
    select(-prop_name, -prop_id)

# Combine
tab_total_goals_markets <-
    totals_overs |>
    left_join(totals_unders) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Match Goals") |> 
    select(match,
           start_time,
           market_name,
           home_team,
           away_team,
           line,
           over_price,
           under_price) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "TAB")

# Write to csv
write_csv(tab_total_goals_markets, "Data/scraped_odds/EPL/tab_total_goals.csv")
    
#===============================================================================
# Team Total Goals Markets
#===============================================================================

# Overs
team_totals_overs <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(str_detect(market_name, "Team Total Goals O/U")) |> 
    filter(str_detect(prop_name, "Over")) |>
    mutate(line = str_extract(prop_name, "\\d+\\.\\d+")) |> 
    mutate(team = str_remove(prop_name, " Over.*$")) |> 
    rename(over_price = price) |>
    select(-prop_name, -prop_id)

# Unders
team_totals_unders <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(str_detect(market_name, "Team Total Goals O/U")) |> 
    filter(str_detect(prop_name, "Under")) |>
    mutate(line = str_extract(prop_name, "\\d+\\.\\d+")) |> 
    mutate(team = str_remove(prop_name, " Under.*$")) |> 
    rename(under_price = price) |>
    select(-prop_name, -prop_id)

# Combine
tab_team_total_goals_markets <-
    team_totals_overs |>
    left_join(team_totals_unders) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(team = fix_team_names(team)) |>
    select(match,
           start_time,
           market_name,
           home_team,
           away_team,
           team,
           line,
           over_price,
           under_price) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "TAB") |> 
    mutate(market_name = "Team Goals")

# Write to csv
write_csv(tab_team_total_goals_markets, "Data/scraped_odds/EPL/tab_team_total_goals.csv")

#===============================================================================
# Both Teams to Score Markets
#===============================================================================

# Both Teams To Score - Yes
both_teams_to_score <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(str_detect(market_name, "^Both Teams to Score")) |> 
    filter(prop_name == "Both Teams to Score") |> 
    rename(yes_price = price) |>
    select(-prop_name, -prop_id)

# Both Teams To Score - No
both_teams_not_to_score <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(str_detect(market_name, "^Both Teams to Score")) |> 
    filter(prop_name == "Only One or Neither to score") |> 
    rename(no_price = price) |>
    select(-prop_name, -prop_id)

# Combine
tab_both_teams_to_score_markets <-
    both_teams_to_score |>
    left_join(both_teams_not_to_score) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    select(match,
           start_time,
           market_name,
           home_team,
           away_team,
           yes_price,
           no_price) |>
    mutate(margin = round((1 / yes_price + 1 / no_price), digits = 3)) |>
    mutate(agency = "TAB")

# Write to csv
write_csv(tab_both_teams_to_score_markets, "Data/scraped_odds/EPL/tab_both_teams_to_score.csv")

#===============================================================================
# Player Goals
#===============================================================================

# Player Goals
player_goals <-
    all_tab_markets |>
    separate(
        match,
        into = c("home_team", "away_team"),
        sep = " v ",
        remove = FALSE
    ) |>
    filter(
        str_detect(market_name, "Anytime Goalscorer") |
            str_detect(market_name, "To Score \\d+\\+ Goals")
    ) |>
    mutate(market_name = str_replace(market_name, "Anytime", "1+")) |>
    mutate(line = str_extract(market_name, "\\d+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(player_name = prop_name) |>
    mutate(market_name = "Player Goals") |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(epl_squads) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
select(match,
       start_time,
       market_name,
       home_team,
       away_team,
       player_name,
       player_team,
       opposition_team,
       line,
       over_price = price) |>
    mutate(agency = "TAB") |> 
    arrange(start_time, match, player_name, line, over_price)

# Write to csv
write_csv(player_goals, "Data/scraped_odds/EPL/tab_player_goals.csv")
