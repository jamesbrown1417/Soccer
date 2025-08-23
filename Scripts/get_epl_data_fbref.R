#===============================================================================
# Set up
#===============================================================================

library(worldfootballR)
library(tidyverse)
library(furrr)
library(janitor)

# Be polite to FBref (you can tweak this)
options(worldfootballR.time_pause = 1)

# Parallel across seasons
plan(multisession)

# Helper: safe wrapper so a bad URL doesn't kill the job
safe_fb_advanced <- purrr::possibly(fb_advanced_match_stats, otherwise = tibble())

#===============================================================================
# Helpers to fetch per-match TEAM and PLAYER advanced stats, then join pieces
#===============================================================================

# ---- TEAM stats for ONE match (summary + defense + passing) -------------------
team_stats_one_match <- function(url) {
  # pull three stat tables; clean names; tag match_url
  s <- safe_fb_advanced(url, stat_type = "summary",  team_or_player = "team")  |> clean_names()
  d <- safe_fb_advanced(url, stat_type = "defense",  team_or_player = "team")  |> clean_names()
  p <- safe_fb_advanced(url, stat_type = "passing",  team_or_player = "team")  |> clean_names()

  if (nrow(s) + nrow(d) + nrow(p) == 0) return(tibble())  # nothing returned

  s <- s |> mutate(match_url = url) |>
    select(match_url, team, home_away, any_of(c("ast","sh","sot","gls")))
  d <- d |> mutate(match_url = url) |>
    select(match_url, team, home_away, any_of(c("tkl","int")))
  p <- p |> mutate(match_url = url) |>
    select(match_url, team, home_away, any_of(c("cmp","att","cmp_percent")))

  # join on team + home_away within match
  out <- s |>
    full_join(d, by = c("match_url","team","home_away")) |>
    full_join(p, by = c("match_url","team","home_away")) |>
    rename(
      Assists              = ast,
      Shots                = sh,
      Shots_On_Target      = sot,
      Goals                = gls,
      Tackles              = tkl,
      Interceptions        = int,
      Passes               = cmp,
      Passes_Attempted     = att,
      Pass_Completion_Rate = cmp_percent
    )

  out
}

# ---- PLAYER stats for ONE match (summary + defense + passing) -----------------
player_stats_one_match <- function(url) {
  s <- safe_fb_advanced(url, stat_type = "summary", team_or_player = "player") |> clean_names()
  d <- safe_fb_advanced(url, stat_type = "defense", team_or_player = "player") |> clean_names()
  p <- safe_fb_advanced(url, stat_type = "passing", team_or_player = "player") |> clean_names()

  if (nrow(s) + nrow(d) + nrow(p) == 0) return(tibble())

  s <- s |> mutate(match_url = url) |>
    select(match_url, player, team, home_away,
           any_of(c("player_num","pos","min","crdy","crdr","gls","ast","sh","sot")))
  d <- d |> mutate(match_url = url) |>
    select(match_url, player, team, home_away, any_of(c("tkl","int")))
  p <- p |> mutate(match_url = url) |>
    select(match_url, player, team, home_away, any_of(c("cmp","att","cmp_percent")))

  s |>
    left_join(d, by = c("match_url","player","team","home_away")) |>
    left_join(p, by = c("match_url","player","team","home_away")) |>
    rename(
      Player_Number         = player_num,
      Position              = pos,
      Minutes               = min,
      Yellow_Cards          = crdy,
      Red_Cards             = crdr,
      Goals                 = gls,
      Assists               = ast,
      Shots                 = sh,
      Shots_On_Target       = sot,
      Tackles               = tkl,
      Interceptions         = int,
      Passes                = cmp,
      Passes_Attempted      = att,
      Pass_Completion_Rate  = cmp_percent
    )
}

#===============================================================================
# Team Stats for a SEASON
#===============================================================================

get_team_stats <- function(season) {
  # Core match results & metadata
  res <- fb_match_results(
    country = "ENG",
    gender  = "M",
    tier    = "1st",
    season_end_year = season
  ) |>
    clean_names() |>
    transmute(
      Competition = "EPL",
      Season = paste(season - 1, season, sep = "/"),
      Week   = wk,
      Match  = paste(home_team, "v", away_team),
      match_url,
      Date   = match_date,
      Time   = match_time,
      Venue  = venue,
      Attendance = attendance,
      Referee    = referee,
      Home       = home_team,
      HomeGoals  = home_goals,
      Home_xG    = home_xg,
      Away       = away_team,
      AwayGoals  = away_goals,
      Away_xG    = away_xg
    )

  urls <- unique(res$match_url)

  team_adv <- purrr::map(urls, team_stats_one_match) |> list_rbind()

  # long (team-per-row) join on match_url
  out <- res |>
    left_join(team_adv, by = "match_url")


  out
}

#===============================================================================
# Player Stats for a SEASON
#===============================================================================

get_player_stats <- function(season) {
  res <- fb_match_results(
    country = "ENG",
    gender  = "M",
    tier    = "1st",
    season_end_year = season
  ) |>
    clean_names() |>
    transmute(
      Competition = "EPL",
      Season = paste(season - 1, season, sep = "/"),
      Week   = wk,
      Match  = paste(home_team, "v", away_team),
      match_url,
      Date   = match_date,
      Time   = match_time,
      Venue  = venue,
      Attendance = attendance,
      Referee    = referee,
      Home       = home_team,
      HomeGoals  = home_goals,
      Home_xG    = home_xg,
      Away       = away_team,
      AwayGoals  = away_goals,
      Away_xG    = away_xg
    )

  urls <- unique(res$match_url)

  player_adv <- purrr::map(urls, player_stats_one_match) |> list_rbind()

  res |> left_join(player_adv, by = "match_url")
}

#===============================================================================
# Run for seasons 2019/20 → 2024/25 (season_end_year 2020:2025)
#===============================================================================

seasons <- 2020:2025

epl_team_stats   <- future_map(seasons, get_team_stats,   .progress = TRUE) |> list_rbind()
epl_player_stats <- future_map(seasons, get_player_stats, .progress = TRUE) |> list_rbind()

#===============================================================================
# Write out data to RDS
#===============================================================================

if (!dir.exists("data")) dir.create("data")
write_rds(epl_team_stats,   "data/epl_team_stats.rds")
write_rds(epl_player_stats, "data/epl_player_stats.rds")