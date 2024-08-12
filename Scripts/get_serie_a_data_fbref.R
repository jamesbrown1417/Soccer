#===============================================================================
# Set up
#===============================================================================

# Libraries
library(worldfootballR)
library(tidyverse)
library(furrr)

# Set up parallel processing
plan(multisession)

#===============================================================================
# Team Stats
#===============================================================================

# Function to get team stats for a given year-----------------------------------
get_team_stats <- function(season) {
    
    # Get match results
    serie_a_results <-
        load_match_results(
            country = "ITA",
            gender = "M",
            tier = "1st",
            season_end_year = season
        ) |>
        transmute(
            Competition = "Serie A",
            Season = paste(Season_End_Year - 1, Season_End_Year, sep = "/"),
            Week = Wk,
            Match = paste(Home, "v", Away),
            MatchURL,
            Date,
            Time,
            Venue,
            Attendance,
            Referee,
            Home,
            HomeGoals,
            Home_xG,
            Away,
            AwayGoals,
            Away_xG
        )
    
    # Get match summary stats
    serie_a_match_summary_stats_all <- load_fb_advanced_match_stats(
        country = "ITA",
        gender = "M",
        tier = "1st",
        season_end_year = season,
        stat_type = "summary",
        team_or_player = "team"
    )
    
    serie_a_match_summary_stats <-
        serie_a_match_summary_stats_all |>
        transmute(
            MatchURL,
            Home_Formation,
            Home_Scorers = Home_Goals,
            Home_Yellow_Cards,
            Home_Red_Cards,
            Away_Formation,
            Away_Scorers = Away_Goals,
            Away_Yellow_Cards,
            Away_Red_Cards,
            Team,
            Home_Away,
            Assists = Ast,
            Shots = Sh,
            Shots_On_Target = SoT,
            Tackles = Tkl,
            Interceptions = Int,
            Passes = Cmp_Passes,
            Passes_Attempted = Att_Passes,
            Pass_Completion_Rate = Cmp_percent_Passes)
    
    # Join with match results
    serie_a_match_summary_stats_final <-
        serie_a_results |>
        left_join(serie_a_match_summary_stats, by = "MatchURL") |>
        relocate(Home_Formation,
                 Home_Scorers,
                 Home_Yellow_Cards,
                 Home_Red_Cards,
                 .after = "Home_xG") 
    
    return(serie_a_match_summary_stats_final)
}

# Get team stats for the last 5 seasons
serie_a_team_stats <-
    future_map(2020:2024, get_team_stats, .progress = TRUE) |>
    bind_rows()

#===============================================================================
# Player Stats
#===============================================================================

# Function to get player stats for a given year---------------------------------
get_player_stats <- function(season) {
    # Get match results
    serie_a_results <-
        load_match_results(
            country = "ITA",
            gender = "M",
            tier = "1st",
            season_end_year = season
        ) |>
        transmute(
            Competition = "Serie A",
            Season = paste(Season_End_Year - 1, Season_End_Year, sep = "/"),
            Week = Wk,
            Match = paste(Home, "v", Away),
            MatchURL,
            Date,
            Time,
            Venue,
            Attendance,
            Referee,
            Home,
            HomeGoals,
            Home_xG,
            Away,
            AwayGoals,
            Away_xG
        )
    
    # Get player summary stats
    serie_a_player_summary_stats <-
        load_fb_advanced_match_stats(
            country = "ITA",
            gender = "M",
            tier = "1st",
            season_end_year = season,
            stat_type = "summary",
            team_or_player = "player"
        ) |>
        transmute(
            MatchURL,
            Player,
            Player_Team = Team,
            Home_Away,
            Player_Number = Player_Num,
            Position = Pos,
            Minutes = Min,
            Yellow_Cards = CrdY,
            Red_Cards = CrdR,
            Goals = Gls,
            Assists = Ast,
            Shots = Sh,
            Shots_On_Target = SoT,
            Tackles = Tkl,
            Interceptions = Int,
            Passes = Cmp_Passes,
            Passes_Attempted = Att_Passes,
            Pass_Completion_Rate = Cmp_percent_Passes
        )
    
    # Combine
    serie_a_player_stats <-
        serie_a_results |>
        left_join(serie_a_player_summary_stats, by = "MatchURL")
    
    # Return
    return(serie_a_player_stats)
}

# Get player stats for the last 5 seasons
serie_a_player_stats <-
    future_map(2020:2024, get_player_stats, .progress = TRUE) |>
    bind_rows()

#===============================================================================
# Write out data to RDS
#===============================================================================

# Write out team stats
write_rds(serie_a_team_stats, "data/serie_a_team_stats.rds")
write_rds(serie_a_player_stats, "data/serie_a_player_stats.rds")