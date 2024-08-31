library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)

#===============================================================================
# Load in data
#===============================================================================

# Load in team stats
epl_team_stats <- read_rds("../../Data/epl_team_stats.rds")
serie_a_team_stats <- read_rds("../../Data/serie_a_team_stats.rds")

# Combine team stats
all_team_stats <- bind_rows(epl_team_stats, serie_a_team_stats)

# Create Match stats table

# Load in player stats
epl_player_stats <- read_rds("../../Data/epl_player_stats.rds")

# Unique players, events, and teams
unique_players <- epl_player_stats$Player |> unique() |> sort()
unique_events <- c("EPL", "Serie A")
unique_teams <- all_team_stats$Team |> unique() |> sort()
unique_seasons <- all_team_stats$Season |> unique() |> sort()

#===============================================================================
# Read in scraped odds
#===============================================================================

# Head to head
head_to_head_data <-
    read_rds("../../Data/processed_odds/h2h_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name) |> 
    mutate(CV = sd(home_win) / abs(mean(home_win))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Total Goals
total_goals_data <-
    read_rds("../../Data/processed_odds/total_goals_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name, line) |>
    mutate(CV = sd(over_price) / abs(mean(over_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Player goals
player_goals_data <-
    read_rds("../../Data/processed_odds/player_goals_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name, line, player_name) |>
    mutate(CV = sd(over_price) / abs(mean(over_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Team goals
team_goals_data <-
    read_rds("../../Data/processed_odds/team_goals_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name, line, team) |>
    mutate(CV = sd(over_price) / abs(mean(over_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Both teams to score
btts_data <-
    read_rds("../../Data/processed_odds/btts_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name) |>
    mutate(CV = sd(yes_price) / abs(mean(yes_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Player Shots
player_shots_data <-
    read_rds("../../Data/processed_odds/player_shots_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name, line, player_name) |>
    mutate(CV = sd(over_price) / abs(mean(over_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Player Shots On Target
player_shots_on_target_data <-
    read_rds("../../Data/processed_odds/player_shots_on_target_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name, line, player_name) |>
    mutate(CV = sd(over_price) / abs(mean(over_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Player Tackles
player_tackles_data <-
    read_rds("../../Data/processed_odds/player_tackles_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name, line, player_name) |>
    mutate(CV = sd(over_price) / abs(mean(over_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

# Player Assists
player_assists_data <-
    read_rds("../../Data/processed_odds/player_assists_data.rds") |> 
    mutate(competition = "EPL") |> 
    group_by(match, competition, market_name, line, player_name) |>
    mutate(CV = sd(over_price) / abs(mean(over_price))) |>
    mutate(CV = round(CV, 2)) |> 
    ungroup()

#===============================================================================
# Create wide mode for odds tables
#===============================================================================

# Head to head------------------------------------------------------------------

# Best Home Odds
head_to_head_best_home_odds <-
    head_to_head_data |>
    arrange(match, desc(home_win)) |> 
    group_by(match) |> 
    slice_head(n = 1) |> 
    select(-CV, -draw, -away_win, -margin) |> 
    rename(home_agency = agency)

# Best Draw Odds
head_to_head_best_draw_odds <-
    head_to_head_data |>
    arrange(match, desc(draw)) |> 
    group_by(match) |> 
    slice_head(n = 1) |> 
    select(-CV, -home_win, -away_win, -margin) |> 
    rename(draw_agency = agency)

# Best Away Odds
head_to_head_best_away_odds <-
    head_to_head_data |>
    arrange(match, desc(away_win)) |> 
    group_by(match) |> 
    slice_head(n = 1) |> 
    select(-CV, -draw, -home_win, -margin) |> 
    rename(away_agency = agency)

head_to_head_wide <-
    head_to_head_best_home_odds |>
    left_join(head_to_head_best_draw_odds) |>
    left_join(head_to_head_best_away_odds) |> 
    relocate(home_win, home_agency, draw, draw_agency, away_win, away_agency, .after = competition) |> 
    mutate(margin = 1/home_win + 1/draw + 1/away_win) |>
    mutate(margin = round(margin, 3)) |> 
    arrange(margin)

# Total Goals-------------------------------------------------------------------

# Best Over Odds
total_goals_best_over_odds <-
    total_goals_data |>
    arrange(match, line, desc(over_price)) |> 
    group_by(match, line) |> 
    slice_head(n = 1) |> 
    select(-CV, -under_price) |> 
    rename(over_agency = agency)

# Best Under Odds
total_goals_best_under_odds <-
    total_goals_data |>
    arrange(match, line, desc(under_price)) |> 
    group_by(match, line) |> 
    slice_head(n = 1) |> 
    select(-CV, -over_price) |> 
    rename(under_agency = agency)

total_goals_wide <-
    total_goals_best_over_odds |>
    left_join(total_goals_best_under_odds) |> 
    relocate(over_price, over_agency, under_price, under_agency, .after = competition) |> 
    mutate(margin = 1/over_price + 1/under_price) |>
    mutate(margin = round(margin, 3)) |> 
    arrange(margin) |> 
    relocate(margin, .after = under_agency)

# Team Goals--------------------------------------------------------------------

# Best Over Odds
team_goals_best_over_odds <-
    team_goals_data |>
    arrange(match, line, team, desc(over_price)) |> 
    group_by(match, line, team) |> 
    slice_head(n = 1) |> 
    select(-CV, -under_price) |> 
    rename(over_agency = agency)

# Best Under Odds
team_goals_best_under_odds <-
    team_goals_data |>
    arrange(match, line, team, desc(under_price)) |> 
    group_by(match, line, team) |> 
    slice_head(n = 1) |> 
    select(-CV, -over_price) |> 
    rename(under_agency = agency)

team_goals_wide <-
    team_goals_best_over_odds |>
    left_join(team_goals_best_under_odds) |> 
    relocate(over_price, over_agency, under_price, under_agency, .after = competition) |> 
    mutate(margin = 1/over_price + 1/under_price) |>
    mutate(margin = round(margin, 3)) |>
    arrange(margin) |> 
    relocate(margin, .after = under_agency)

# Both Teams to Score-----------------------------------------------------------

# Best Yes Odds
btts_best_yes_odds <-
    btts_data |>
    arrange(match, desc(yes_price)) |> 
    group_by(match) |> 
    slice_head(n = 1) |> 
    select(-CV, -no_price) |> 
    rename(yes_agency = agency)

# Best No Odds
btts_best_no_odds <-
    btts_data |>
    arrange(match, desc(no_price)) |> 
    group_by(match) |> 
    slice_head(n = 1) |> 
    select(-CV, -yes_price) |> 
    rename(no_agency = agency)

btts_wide <-
    btts_best_yes_odds |>
    left_join(btts_best_no_odds) |> 
    relocate(yes_price, yes_agency, no_price, no_agency, .after = competition) |> 
    mutate(margin = 1/yes_price + 1/no_price) |>
    mutate(margin = round(margin, 3)) |> 
    arrange(margin) |> 
    relocate(margin, .after = no_agency)

# Player Shots------------------------------------------------------------------

# Best Over Odds
player_shots_best_over_odds <-
    player_shots_data |>
    arrange(match, line, player_name, desc(over_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -under_price) |> 
    rename(over_agency = agency)

# Best Under Odds
player_shots_best_under_odds <-
    player_shots_data |>
    filter(!is.na(under_price)) |> 
    arrange(match, line, player_name, desc(under_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -over_price) |> 
    rename(under_agency = agency)

player_shots_wide <-
    player_shots_best_over_odds |>
    inner_join(player_shots_best_under_odds) |> 
    relocate(over_price, over_agency, under_price, under_agency, .after = competition) |> 
    mutate(margin = 1/over_price + 1/under_price) |>
    mutate(margin = round(margin, 3)) |> 
    arrange(margin) |> 
    relocate(margin, .after = under_agency)

# Player Shots On Target--------------------------------------------------------

# Best Over Odds
player_shots_on_target_best_over_odds <-
    player_shots_on_target_data |>
    arrange(match, line, player_name, desc(over_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -under_price) |> 
    rename(over_agency = agency)

# Best Under Odds
player_shots_on_target_best_under_odds <-
    player_shots_on_target_data |>
    filter(!is.na(under_price)) |> 
    arrange(match, line, player_name, desc(under_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -over_price) |> 
    rename(under_agency = agency)

player_shots_on_target_wide <-
    player_shots_on_target_best_over_odds |>
    inner_join(player_shots_on_target_best_under_odds) |> 
    relocate(over_price, over_agency, under_price, under_agency, .after = competition) |> 
    mutate(margin = 1/over_price + 1/under_price) |>
    mutate(margin = round(margin, 3)) |> 
    arrange(margin) |> 
    relocate(margin, .after = under_agency)

# Player Tackles----------------------------------------------------------------

# Best Over Odds
player_tackles_best_over_odds <-
    player_tackles_data |>
    arrange(match, line, player_name, desc(over_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -under_price) |> 
    rename(over_agency = agency)

# Best Under Odds
player_tackles_best_under_odds <-
    player_tackles_data |>
    filter(!is.na(under_price)) |> 
    arrange(match, line, player_name, desc(under_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -over_price) |> 
    rename(under_agency = agency)

player_tackles_wide <-
    player_tackles_best_over_odds |>
    inner_join(player_tackles_best_under_odds) |> 
    relocate(over_price, over_agency, under_price, under_agency, .after = competition) |> 
    mutate(margin = 1/over_price + 1/under_price) |>
    mutate(margin = round(margin, 3)) |> 
    arrange(margin) |> 
    relocate(margin, .after = under_agency)

# Player Assists----------------------------------------------------------------

# Best Over Odds
player_assists_best_over_odds <-
    player_assists_data |>
    arrange(match, line, player_name, desc(over_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -under_price) |> 
    rename(over_agency = agency)

# Best Under Odds
player_assists_best_under_odds <-
    player_assists_data |>
    filter(!is.na(under_price)) |> 
    arrange(match, line, player_name, desc(under_price)) |> 
    group_by(match, line, player_name) |> 
    slice_head(n = 1) |> 
    select(-CV, -over_price) |> 
    rename(under_agency = agency)

player_assists_wide <- 
    player_assists_best_over_odds |>
    inner_join(player_assists_best_under_odds) |> 
    relocate(over_price, over_agency, under_price, under_agency, .after = competition) |> 
    mutate(margin = 1/over_price + 1/under_price) |>
    mutate(margin = round(margin, 3)) |> 
    arrange(margin) |> 
    relocate(margin, .after = under_agency)

#===============================================================================
# UI
#===============================================================================

ui <- page_navbar(
    title = "Soccer Data",
    selected = "Player Stats",
    collapsible = TRUE,
    theme = bslib::bs_theme(),
    tags$head(tags$style(
        HTML(
            "
      .tab-content, .tab-pane {
        height: 1250px;
        overflow-y: auto;
      }
      .dataTables_wrapper {
        overflow-x: auto;
      }
    "
        )
    )),
    nav_panel(
        title = "Player Stats",
        grid_container(
            layout = c("stats player_stat_plot"),
            row_sizes = c("1fr"),
            col_sizes = c("250px", "1fr"),
            gap_size = "10px",
            grid_card(
                area = "stats",
                card_header("Settings"),
                card_body(
                    selectInput(
                        inputId = "player_name_input_a",
                        label = "Select Player:",
                        selected = "Rodri",
                        choices = unique_players,
                    ),
                    selectInput(
                        inputId = "event_input_a",
                        label = "Select Competition:",
                        choices = unique_events,
                        multiple = TRUE,
                        selected = unique_events
                    ),
                    selectInput(
                        inputId = "season_input_a",
                        label = "Select Season:",
                        choices = unique_seasons,
                        multiple = TRUE,
                        selected = c("2023/2024", "2024/2025")
                    ),
                    selectInput(
                        inputId = "stat_input_a",
                        label = "Select Statistic:",
                        choices = c("Goals",
                                    "Assists",
                                    "Shots",
                                    "Passes"),
                        multiple = FALSE,
                        selected = "Goals"
                    ),
                    selectInput(
                        inputId = "venue_input_a",
                        label = "Select Venue:",
                        choices = epl_team_stats$Venue |> unique() |> sort(),
                        multiple = TRUE,
                        selected = NULL,
                        selectize = TRUE
                    ),
                    markdown(mds = c("__Select Only Last n Games:__")),
                    numericInput(
                        inputId = "last_games",
                        label = "Number of Games",
                        value = NA
                    ),
                    markdown(mds = c("__Select Reference Line:__")),
                    numericInput(
                        inputId = "reference_line",
                        label = "Line Value",
                        value = 0.5
                    ))),
            grid_card(area = "player_stat_plot",
                      card_body(
                          tabsetPanel(
                              id = "stat_tabs",
                              tabPanel("Plot",
                                       plotOutput(outputId = "player_stat_plot", height = "800px")),
                              tabPanel(
                                  "Table",
                                  DTOutput(
                                      outputId = "player_stat_table",
                                      width = "100%",
                                      height = "800px"
                                  )
                              )
                          )
                      ))))
    ,
    nav_panel(
        title = "Team Stats",
        grid_container(
            layout = c("team_stats team_stat_plot"),
            row_sizes = c("1fr"),
            col_sizes = c("250px", "1fr"),
            gap_size = "10px",
            grid_card(
                area = "team_stats",
                card_header("Settings"),
                card_body(
                    selectInput(
                        inputId = "team_name_input_c",
                        label = "Select Team:",
                        choices = unique_teams,
                        multiple = TRUE,
                        selectize = TRUE
                    ),
                    selectInput(
                        inputId = "event_input_c",
                        label = "Select Event:",
                        choices = unique_events,
                        multiple = TRUE,
                        selected = unique_events
                    ),
                    selectInput(
                        inputId = "season_input_c",
                        label = "Select Season:",
                        choices = unique_seasons,
                        multiple = TRUE,
                        selected = c("2023/2024", "2024/2025")
                    ),
                    selectInput(
                        inputId = "stat_input_c",
                        label = "Select Statistic:",
                        choices = c("Passes",
                                    "Shots",
                                    "Shots_On_Target"),
                        multiple = FALSE,
                        selected = "Shots"
                    ),
                    selectInput(
                        inputId = "venue_input_c",
                        label = "Select Venue:",
                        choices = all_team_stats$Venue |> unique() |> sort(),
                        multiple = TRUE,
                        selected = NULL,
                        selectize = TRUE
                    ),
                    markdown(mds = c("__Select Only Last n Games:__")),
                    numericInput(
                        inputId = "last_games_c",
                        label = "Number of Games",
                        value = NA
                    ),
                    markdown(mds = c("__Select Reference Line:__")),
                    numericInput(
                        inputId = "reference_line_c",
                        label = "Line Value",
                        value = 14.5
                    ))),
            grid_card(area = "team_stat_plot",
                      card_body(
                          tabsetPanel(
                              id = "stat_tabs_team",
                              tabPanel("Plot",
                                       plotOutput(outputId = "team_stat_plot", height = "800px")),
                              tabPanel("Summary",
                                       DTOutput(outputId = "team_stat_summary",width = "100%", height = "800px")),
                              tabPanel(
                                  "Table",
                                  DTOutput(
                                      outputId = "team_stat_table",
                                      width = "100%",
                                      height = "800px"
                                  )
                              )
                          )
                      ))))
    ,
    nav_panel(
        title = "Odds Screen",
        grid_container(
            layout = c("odds_screen odds_table"),
            row_sizes = c("1fr"),
            col_sizes = c("250px", "1fr"),
            gap_size = "10px",
            grid_card(
                area = "odds_screen",
                card_header("Settings"),
                card_body(
                    selectInput(
                      inputId = "agency_input",
                      label = "Select Agencies:",
                      choices = head_to_head_data$agency |> unique(),
                      multiple = TRUE,
                      selectize = TRUE,
                      selected = head_to_head_data$agency |> unique(),
                    ),
                    selectInput(
                        inputId = "event_input_odds",
                        label = "Select Competition:",
                        choices = c("EPL", "Serie A"),
                        multiple = TRUE,
                        selectize = TRUE,
                        selected = c("EPL")
                    ),
                    selectInput(
                        inputId = "market_input",
                        label = "Select Market:",
                        choices = c("Head To Head", "Both Teams To Score", "Match Goals", "Team Goals", "Player Goals"),
                        multiple = FALSE
                    ),
                    checkboxInput(
                        inputId = "wide_mode_odds",
                        label = "Arb / Rollover Mode",
                        value = FALSE
                    ),
                    selectInput(
                      inputId = "match_input",
                      label = "Select Matches:",
                      choices = head_to_head_data$match |> unique(),
                      multiple = TRUE,
                      selectize = FALSE,
                      selected = head_to_head_data$match |> unique()
                    ),
                    textInput(
                        inputId = "player_name_input_b",
                        label = "Select Player:",
                        value = NA
                    ),
                    checkboxInput(
                        inputId = "only_unders",
                        label = "Only Show Markets With Unders",
                        value = FALSE
                    ),
                    checkboxInput(
                        inputId = "only_best",
                        label = "Only Show Best Market Odds - Overs",
                        value = FALSE
                    ),
                    checkboxInput(
                        inputId = "only_best_unders",
                        label = "Only Show Best Market Odds - Unders",
                        value = FALSE
                    )
                )
            ),
            grid_card(area = "odds_table",
                      card_body(
                          DTOutput(outputId = "scraped_odds_table", height = "1000px")
                      ))
        )
    )
)

#===============================================================================
# Server
#===============================================================================

server <- function(input, output, session) {
    # Add your server-side code here
    
    #=============================================================================
    # Filter player stats
    #=============================================================================
    
    filtered_player_stats <- reactive({
        # Filter player stats
        
        all_player_stats <-
            epl_player_stats |>
            mutate(Player_Team_Goals = ifelse(Player_Team == Home, HomeGoals, AwayGoals))
        
            filtered_player_stats <-
                all_player_stats |>
                filter(
                    Player == input$player_name_input_a,
                ) |>
                arrange(Date) |>
                mutate(game_number = row_number()) |> 
                select(Date,
                       Competition,
                       Season,
                       Venue,
                       Match,
                       Home_Away,
                       Player,
                       Position,
                       Minutes,
                       Yellow_Cards,
                       Red_Cards,
                       Player_Team,
                       Player_Team_Goals,
                       Goals,
                       Assists,
                       Passes,
                       Passes_Attempted,
                       Pass_Completion_Rate,
                       Shots,
                       Shots_On_Target, 
                       game_number) |> 
                arrange(desc(Date))
        
        # Filter by last n games
        if (!is.na(input$last_games)) {
            filtered_player_stats <-
                filtered_player_stats |>
                slice_head(n = input$last_games)
        }
        
        # Filter by event
        filtered_player_stats <-
            filtered_player_stats |>
            filter(Competition %in% input$event_input_a)
        
        # Filter by season
        filtered_player_stats <-
            filtered_player_stats |>
            filter(Season %in% input$season_input_a)
        
        # Filter by venue
        if (!is.null(input$venue_input_a)) {
            filtered_player_stats <-
                filtered_player_stats |>
                filter(Venue %in% input$venue_input_a)
        }
        
        # Return filtered player stats
        return(filtered_player_stats)
        
    })
    
    #=============================================================================
    # Get Proportion above reference line
    #=============================================================================
    
    proportion_above_reference_line <- reactive({
        # Get proportion above reference line
        proportion_above_reference_line <-
            filtered_player_stats() |>
            filter(!!sym(input$stat_input_a) >= input$reference_line) |>
            nrow() / nrow(filtered_player_stats())
        
        # Get implied Odds
        implied_odds <- 1 / proportion_above_reference_line
        implied_odds_under <- 1 / (1 - proportion_above_reference_line)
        
        # Get string to output
        output_string <- paste0(
            "Proportion Above Reference Line: ",
            round(proportion_above_reference_line, 2),
            "\n",
            "Implied Odds - Over: ",
            round(implied_odds, 2),
            "\n",
            "Implied Odds - Under: ",
            round(implied_odds_under, 2),
            "\n",
            "Sample Size: ",
            nrow(filtered_player_stats())
        )
        
        return(output_string)
        
    })
    
    #=============================================================================
    # Plot player stats
    #=============================================================================
    
    output$player_stat_plot <- renderPlot({
        # Create a new variable that checks if the y-value is above the reference line
        df_with_color <- filtered_player_stats() %>%
            mutate(color_condition = ifelse(
                !!sym(input$stat_input_a) >= input$reference_line,
                "limegreen",
                "red1"
            ))
        
        # Plot player stats
        p <- df_with_color %>%
            ggplot(aes(
                x = game_number,
                y = !!sym(input$stat_input_a),
                color = color_condition
            )) +
            
            # Basic Elements
            geom_point(size = 3) +
            geom_smooth(
                method = "loess",
                se = FALSE,
                inherit.aes = FALSE,
                mapping = aes(x = game_number, y = !!sym(input$stat_input_a))
            ) +
            geom_hline(
                yintercept = input$reference_line,
                linetype = "dashed",
                color = "grey4",
                size = 1
            )+
            
            # Add text
            annotate(
                geom = "text",
                x = 1,
                y = max(filtered_player_stats() %>% pull(!!sym(
                    input$stat_input_a
                ))),
                label = proportion_above_reference_line(),
                hjust = 0,
                vjust = 1,
                color = "black",
                size = 6
            ) +
            
            # Aesthetics
            theme_bw() +
            theme(
                plot.background = element_rect(fill = "white", colour = "white"),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12)
            ) +
            
            # Labels & Titles
            labs(title = "",
                 x = "Game Number") +
            
            # Set manual color scale
            scale_color_identity() +
            
            # Additional
            theme(legend.position = "none")
        
        print(p)
    })
    
    #=============================================================================
    # Table player stats
    #=============================================================================
    
    output$player_stat_table <- renderDT({
        datatable(
            filtered_player_stats(),
            options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
            width = "100%",
            height = "800px"
        )
    })
    
    #=============================================================================
    # Filter Team stats
    #=============================================================================
    
    filtered_team_stats <- reactive({
        # Filter player stats

        filtered_team_stats <-
            all_team_stats |>
            select(-MatchURL, -Home_Scorers, -Away_Scorers) |> 
            arrange(desc(Date))

        # Filter by event
        filtered_team_stats <-
            filtered_team_stats |>
            filter(Competition %in% input$event_input_c)

        # Filter by season
        filtered_team_stats <-
            filtered_team_stats |>
            filter(Season %in% input$season_input_c)
        
        # Filter by venue
        if (!is.null(input$venue_input_c)) {
            filtered_team_stats <-
                filtered_team_stats |>
                filter(Venue %in% input$venue_input_c)
        }

        # Filter by team
        if (!is.null(input$team_name_input_c)) {
            filtered_team_stats <-
                filtered_team_stats |>
                filter(Team %in% input$team_name_input_c)
        }

        # Now make game number
        filtered_team_stats <-
            filtered_team_stats |>
            mutate(game_number = row_number())
        
        # Filter by last n games
        if (!is.na(input$last_games_c)) {
            filtered_team_stats <-
                filtered_team_stats |>
                slice_head(n = input$last_games_c)
        }

        # Return filtered player stats
        return(filtered_team_stats)

    })
    
    #=============================================================================
    # Get Proportion above reference line
    #=============================================================================
    
    proportion_above_reference_line_team <- reactive({
        # Get proportion above reference line
        proportion_above_reference_line_team <-
            filtered_team_stats() |>
            filter(!!sym(input$stat_input_c) >= input$reference_line_c) |>
            nrow() / nrow(filtered_team_stats())
        
        # Get implied Odds
        implied_odds <- 1 / proportion_above_reference_line_team
        implied_odds_under <- 1 / (1 - proportion_above_reference_line_team)
        
        # Get string to output
        output_string_team <- paste0(
            "Proportion Above Reference Line: ",
            round(proportion_above_reference_line_team, 2),
            "\n",
            "Implied Odds - Over: ",
            round(implied_odds, 2),
            "\n",
            "Implied Odds - Under: ",
            round(implied_odds_under, 2),
            "\n",
            "Sample Size: ",
            nrow(filtered_team_stats())
        )
        
        return(output_string_team)
        
    })
    
    #=============================================================================
    # Plot team stats
    #=============================================================================
    
    output$team_stat_plot <- renderPlot({
        # Create a new variable that checks if the y-value is above the reference line
        df_with_color <- filtered_team_stats() %>%
            mutate(color_condition = ifelse(
                !!sym(input$stat_input_c) >= input$reference_line_c,
                "limegreen",
                "red1"
            ))
        
        # Plot player stats
        p <- df_with_color %>%
            ggplot(aes(
                x = game_number,
                y = !!sym(input$stat_input_c),
                color = color_condition
            )) +
            
            # Basic Elements
            geom_point(size = 3) +
            geom_smooth(
                method = "loess",
                se = FALSE,
                inherit.aes = FALSE,
                mapping = aes(x = game_number, y = !!sym(input$stat_input_c))
            ) +
            geom_hline(
                yintercept = input$reference_line_c,
                linetype = "dashed",
                color = "grey4",
                size = 1
            )+
            
            # Add text
            annotate(
                geom = "text",
                x = 1,
                y = max(filtered_team_stats() %>% pull(!!sym(
                    input$stat_input_c
                ))),
                label = proportion_above_reference_line_team(),
                hjust = 0,
                vjust = 1,
                color = "black",
                size = 6
            ) +
            
            # Aesthetics
            theme_bw() +
            theme(
                plot.background = element_rect(fill = "white", colour = "white"),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12)
            ) +
            
            # Labels & Titles
            labs(title = "",
                 x = "Game Number") +
            
            # Set manual color scale
            scale_color_identity() +
            
            # Additional
            theme(legend.position = "none")
        
        print(p)
    })
    
    #=============================================================================
    # Table team stats
    #=============================================================================
    
    output$team_stat_table <- renderDT({
        datatable(
            filtered_team_stats(),
            options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
            width = "100%",
            height = "800px"
        )
    })
    
    #=============================================================================
    # Table team summary
    #=============================================================================
    
    output$team_stat_summary <- renderDT({
        datatable(
            filtered_team_stats() |> 
                summarise(
                    mean = round(mean(!!sym(input$stat_input_c), na.rm = TRUE), 2),
                    median = round(median(!!sym(input$stat_input_c), na.rm = TRUE), 2),
                    sd = round(sd(!!sym(input$stat_input_c), na.rm = TRUE), 2),
                    min = round(min(!!sym(input$stat_input_c), na.rm = TRUE), 2),
                    max = round(max(!!sym(input$stat_input_c), na.rm = TRUE), 2),
                    n = n()  
                ),
            options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
            width = "100%",
            height = "800px"
        )
    })
    
    #=============================================================================
    # Reactive function to scrape odds
    #=============================================================================
    
    scraped_odds <- reactive({
        # Get odds---------------------------------------------------------------
        
        # H2H
        if (input$market_input == "Head To Head") {
            
            if (input$wide_mode_odds) {
                odds <-
                    head_to_head_wide |>
                    filter(competition %in% input$event_input_odds) |>
                    filter(match %in% input$match_input)
            }
            
            else {
                odds <-
                    head_to_head_data |>
                    filter(competition %in% input$event_input_odds) |>
                    filter(match %in% input$match_input)
            }
        }
        
        # Both Teams To Score
        if (input$market_input == "Both Teams To Score") {
            
            if (input$wide_mode_odds) {
                odds <-
                    btts_wide |>
                    filter(competition %in% input$event_input_odds) |>
                    filter(match %in% input$match_input)
            }
            
            else {
                odds <-
                    btts_data |>
                    filter(competition %in% input$event_input_odds) |>
                    filter(match %in% input$match_input)
            }
            
        }
        
        # Match Total Goals
        if (input$market_input == "Match Goals") {
            odds <-
                total_goals_data |> 
                filter(competition %in% input$event_input_odds) |> 
                filter(match %in% input$match_input)
        }
        
        # Team Goals
        if (input$market_input == "Team Goals") {
            odds <-
                team_goals_data |> 
                filter(competition %in% input$event_input_odds) |> 
                filter(match %in% input$match_input)
        }
        
        # Player Goals
        if (input$market_input == "Player Goals") {
            odds <-
                player_goals_data |> 
                filter(competition %in% input$event_input_odds) |> 
                filter(match %in% input$match_input)
            
            if (input$player_name_input_b != "") {
                odds <-
                    odds |>
                    filter(str_detect(player_name, input$player_name_input_b))
            }
            
            if (input$only_best == TRUE) {
                odds <-
                    odds |> 
                    arrange(player_name, line, desc(over_price)) |>
                    group_by(player_name, market, line) |> 
                    slice_head(n = 1) |>
                    ungroup()
            }
            
            if (input$only_best_unders == TRUE) {
                odds <-
                    odds |> 
                    arrange(player_name, line, desc(under_price)) |>
                    group_by(player_name, market, line) |> 
                    slice_head(n = 1) |>
                    ungroup()
            }
            
            if (input$only_unders == TRUE) {
                odds <-
                    odds |> 
                    filter(!is.na(under_price))
            }
        }
        
        # Return odds
        return(odds)
    })
    
    # Table output
    output$scraped_odds_table <- renderDT({
        datatable(scraped_odds(),
                  fillContainer = TRUE,
                  filter = "top",
                  options = list(
                      pageLength = 17,
                      autoWidth = FALSE,
                      scrollX = TRUE, scrollY = TRUE,
                      lengthMenu = c(5, 10, 15, 20, 25, 30)
                  ))
    })
}

# Run the application
shinyApp(ui, server)