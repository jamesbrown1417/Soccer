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
epl_results <-
    load_match_results(
        country = "ENG",
        gender = "M",
        tier = "1st",
        season_end_year = season
    ) |>
    transmute(
        Competition = "EPL",
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
epl_match_summary_stats_all <- load_fb_advanced_match_stats(
    country = "ENG",
    gender = "M",
    tier = "1st",
    season_end_year = season,
    stat_type = "summary",
    team_or_player = "team"
)

epl_match_summary_stats <-
    epl_match_summary_stats_all |>
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
epl_match_summary_stats_final <-
    epl_results |>
    left_join(epl_match_summary_stats, by = "MatchURL") |>
    relocate(Home_Formation,
             Home_Scorers,
             Home_Yellow_Cards,
             Home_Red_Cards,
             .after = "Home_xG") 

return(epl_match_summary_stats_final)
}

# Get team stats for the last 5 seasons
epl_team_stats <-
    future_map(2020:2025, get_team_stats, .progress = TRUE) |>
    bind_rows()

#===============================================================================
# Player Stats
#===============================================================================

# Function to get player stats for a given year---------------------------------
get_player_stats <- function(season) {
    # Get match results
    epl_results <-
        load_match_results(
            country = "ENG",
            gender = "M",
            tier = "1st",
            season_end_year = season
        ) |>
        transmute(
            Competition = "EPL",
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
    epl_player_summary_stats <-
        load_fb_advanced_match_stats(
            country = "ENG",
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
    epl_player_stats <-
        epl_results |>
        left_join(epl_player_summary_stats, by = "MatchURL")
    
    # Return
    return(epl_player_stats)
}

# Get player stats for the last 5 seasons
epl_player_stats <-
    future_map(2020:2025, get_player_stats, .progress = TRUE) |>
    bind_rows()

#===============================================================================
# Write out data to RDS
#===============================================================================

# Write out team stats
write_rds(epl_team_stats, "data/epl_team_stats.rds")
write_rds(epl_player_stats, "data/epl_player_stats.rds")