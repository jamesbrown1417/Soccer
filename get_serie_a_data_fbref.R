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
serie_a_results <-
    load_match_results(
        country = "ITA",
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
serie_a_match_summary_stats <- load_fb_advanced_match_stats(
    country = "ITA",
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
serie_a_player_summary_stats <- load_fb_advanced_match_stats(
    country = "ITA",
    gender = "M",
    tier = "1st",
    season_end_year = 2024,
    stat_type = "summary",
    team_or_player = "player"
)
