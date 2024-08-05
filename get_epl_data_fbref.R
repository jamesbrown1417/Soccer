#===============================================================================
# Set up
#===============================================================================

# Libraries
library(worldfootballR)
library(tidyverse)

#===============================================================================
# Team Stats
#===============================================================================

# Season 2023/2024--------------------------------------------------------------

# Get match results
epl_results <-
    load_match_results(
        country = "ENG",
        gender = "M",
        tier = "1st",
        season_end_year = 2024
    ) |>
    transmute(
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
epl_match_summary_stats <- load_fb_advanced_match_stats(
    country = "ENG",
    gender = "M",
    tier = "1st",
    season_end_year = 2024,
    stat_type = "summary",
    team_or_player = "team"
)

#===============================================================================
# Player Stats
#===============================================================================

# Get player summary stats
epl_player_summary_stats <- load_fb_advanced_match_stats(
    country = "ENG",
    gender = "M",
    tier = "1st",
    season_end_year = 2024,
    stat_type = "summary",
    team_or_player = "player"
)
