library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)

#===============================================================================
# Load in data
#===============================================================================

# Load in team stats
epl_team_stats <- read_rds("../../Data/epl_team_stats.rds")

# Load in player stats
epl_player_stats <- read_rds("../../Data/epl_player_stats.rds")

# Unique players
unique_players <- epl_player_stats$Player |> unique() |> sort()

unique_events <- c("EPL", "Serie A")

unique_teams <- epl_team_stats$Team |> unique() |> sort()

#===============================================================================
# Read in scraped odds
#===============================================================================

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
                        inputId = "stat_input_c",
                        label = "Select Statistic:",
                        choices = c("Runs",
                                    "Wickets",
                                    "Runs at Fall of First Wicket",
                                    "First Wicket Caught",
                                    "First Over Runs",
                                    "Chasing Team Win",
                                    "4s",
                                    "6s"),
                        multiple = FALSE,
                        selected = "Innings Total"
                    ),
                    selectInput(
                        inputId = "venue_input_c",
                        label = "Select Venue:",
                        choices = epl_player_stats$venue |> unique() |> sort(),
                        multiple = TRUE,
                        selected = NULL,
                        selectize = TRUE
                    ),
                    selectInput(
                        inputId = "innings_input_c",
                        label = "Select Innings:",
                        choices = c("1", "2"),
                        multiple = TRUE,
                        selected = c("1", "2")
                    ),
                    markdown(mds = c("__Select Minimum Innings Length:__")),
                    numericInput(
                        inputId = "innings_balls_bowled_min_c",
                        label = "Number of Balls",
                        value = 0
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
                        value = 149.5
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
                    # Add your specific input fields here
                    # Example: selectInput, textInput, numericInput, etc.
                    # selectInput(
                    #   inputId = "agency_input",
                    #   label = "Select Agencies:",
                    #   choices = player_runs_data$agency |> unique(),
                    #   multiple = TRUE,
                    #   selectize = TRUE,
                    #   selected = player_runs_data$agency |> unique(),
                    # ),
                    selectInput(
                        inputId = "market_input",
                        label = "Select Market:",
                        choices = c("Runs", "Wickets", "Boundaries"),
                        multiple = FALSE
                    ),
                    # selectInput(
                    #   inputId = "match_input",
                    #   label = "Select Matches:",
                    #   choices = player_runs_data$match |> unique(),
                    #   multiple = TRUE,
                    #   selectize = FALSE,
                    #   selected = player_runs_data$match |> unique()
                    # ),
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
            epl_player_stats
        
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
    
    # filtered_team_stats <- reactive({
    #     # Filter player stats
    #     
    #     filtered_team_stats <-
    #         innings_stats_long |>
    #         select(Date = match_date,
    #                Event = event,
    #                Venue = venue,
    #                `Toss Winner` = toss_winner,
    #                `Toss Decision` = toss_decision,
    #                `Match Winner` = match_winner,
    #                `Chasing Team Win` = chasing_team_won,
    #                Innings = innings,
    #                Innings_Balls = innings_balls,
    #                Runs = innings_total,
    #                `4s` = innings_fours,
    #                `6s` = innings_sixes,
    #                Wickets = innings_wickets,
    #                Batting = innings_batting_team,
    #                Fielding = innings_fielding_team, 
    #                `First Dismissal Method` = innings_method_of_first_dismissal,
    #                `First Wicket Caught` = first_wicket_caught,
    #                `Runs at Fall of First Wicket` = innings_fall_of_first_wicket,
    #                `First Over Bowler` = first_over_bowler,
    #                `First Over Runs` = first_over_total,
    #                `First Over Wickets` = first_over_wickets,
    #                `First Over 4s` = first_over_fours,
    #                `First Over 6s` = first_over_sixes) |> 
    #         arrange(desc(Date))
    #     
    #     # Filter by last n games
    #     if (!is.na(input$last_games_c)) {
    #         filtered_team_stats <-
    #             filtered_team_stats |>
    #             slice_head(n = input$last_games_c)
    #     }
    #     
    #     # Filter by innings balls bowled
    #     if (!is.na(input$innings_balls_bowled_min_c)) {
    #         filtered_team_stats <-
    #             filtered_team_stats |>
    #             filter(Innings_Balls >= input$innings_balls_bowled_min_c)
    #     }
    #     
    #     # Filter by innings
    #     filtered_team_stats <-
    #         filtered_team_stats |>
    #         filter(Innings %in% input$innings_input_c)
    #     
    #     # Filter by event
    #     filtered_team_stats <-
    #         filtered_team_stats |>
    #         filter(Event %in% input$event_input_c)
    #     
    #     # Filter by venue  
    #     if (!is.null(input$venue_input_c)) {
    #         filtered_team_stats <-
    #             filtered_team_stats |>
    #             filter(Venue %in% input$venue_input_c)
    #     }
    #     
    #     # Filter by team
    #     if (!is.null(input$team_name_input_c)) {
    #         filtered_team_stats <-
    #             filtered_team_stats |>
    #             filter(Batting %in% input$team_name_input_c)
    #     }
    #     
    #     # Now make game number
    #     filtered_team_stats <-
    #         filtered_team_stats |>
    #         mutate(game_number = row_number())
    #     
    #     # Return filtered player stats
    #     return(filtered_team_stats)
    #     
    # })
    
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
        
        # Runs
        if (input$market_input == "Runs") {
            odds <-
                player_runs_data |> 
                mutate(variation = round(variation, 2)) |>
                filter(match %in% input$match_input) |>
                select(-match)
        }
        
        # Boundaries
        if (input$market_input == "Boundaries") {
            odds <-
                player_boundaries_data |> 
                mutate(variation = round(variation, 2)) |>
                filter(match %in% input$match_input) |>
                select(-match) 
        }
        
        # Wickets
        if (input$market_input == "Wickets") {
            odds <-
                player_wickets_data |> 
                mutate(variation = round(variation, 2)) |>
                filter(match %in% input$match_input) |>
                select(-match)
        }
        
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