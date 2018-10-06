library(shinythemes)

fluidPage(
  theme = shinytheme("paper"),
  tags$head(
    includeScript("www/google-analytics.js"),
    tags$link(rel = "icon", type = "image/png", href = "EStinylogo.png")
  ),
  (img(
    src = "ESlogo.png",
    height = 70,
    align = "top"
  )),
  navbarPage(
    title = ("Even-Strength"),
    
    #  ( (img(
    #    src = "EStinylogo.png",
    #   height = 20,
    #   align = "top"
    # ))),
    id = "nav",
    tabPanel("Home",
             h2("Welcome to Even-Strength"),
             h6(
               HTML(
                 "<br>Welcome to Even-Strength!<br>
                 <br> Our goal is to provide to fans with an accessible platform to navigate NWHL stats.
                 <br> On this site, you'll be able to find player and team data, as well as charts
                 <br> and tools for easy analysis.
                 <br>
                 <br> References and source information can be found in the <i>Sources</i> tab under <i>More</i>.
                 <br>
                 <br>This is still a work in progress, so please contact <a href = 'https://twitter.com/Even_Strength'>@even_strength</a> on Twitter if
                 <br>you find any problems or have any suggestions.
                 <br>
                 <br> Additionally, we've created a <a href = 'https://www.patreon.com/even_strength'> Patreon Page </a> to help support website/server costs.
                 <br>
                 "
               ) ),
              
             h6(HTML(
               "Thanks,<br>Jake, CJ, Matt & Alyssa"
             ))),
    #Players ----
    navbarMenu(
      "Players",
      tabPanel(
        "Player Table",
        fluidRow(
          column(
            2,
            selectInput(
              "position",
              "Position",
              choices = c(unique(player_data$Position), "Skaters", "All"),
              multiple = FALSE,
              selected = "All"
            )
          ),
          column(
            2,
            selectInput(
              "team_table",
              "Team",
              choices = c(team_names, "All"),
              multiple = F,
              selected = "All"
            )
          ),
          column(
            2,
            sliderInput(
              "season",
              "Season",
              min = min(player_data$Season),
              max = max(player_data$Season),
              sep = "",
              step = 10001,
              value = c(min(player_data$Season), max(player_data$Season))
            )
          ),
          column(
            2,
            radioButtons(
              "aggregate",
              "Choose View",
              c("Game", "Season", "Career"),
              selected = "Season"
            )
          ),
          conditionalPanel(condition = "input.aggregate  == 'Season' || input.aggregate == 'Career'",
                           column(
                             2,
                             radioButtons("pergame",
                                          "Select Rate",
                                          c("Totals", "per Game", "per 60"))
                           ),
                           column(
                             2,
                             sliderInput(
                               "games_played",
                               "Games Played",
                               min = 1,
                               max = 50,
                               value = c(1, 50),
                               step = 1
                             )
                           ))
        ),
        actionButton(
          inputId = "submit_button_player",
          label = "Submit",
          width = "25%"
        ),
        div(dataTableOutput("gameStats"), style = "font-size: 90%; width: 100%")
      ),
      tabPanel("League Leaders",
               sidebarLayout(
                 div(
                   sidebarPanel(
                     selectizeInput(
                       "leader_position",
                       "Choose Position",
                       choices = c("F", "D", "All"),
                       selected = "All"
                     ),
                     selectizeInput(
                       "leader_team",
                       "Choose Team",
                       choices = c(team_names, "All"),
                       selected = "All"
                     ),
                     checkboxInput("leader_pergame",
                                   "Per Game Rates",
                                   value = F),
                     sliderInput(
                       "GP_filter",
                       "Minimum GP",
                       value  = 1,
                       step = 1,
                       min = 1,
                       max = 20
                     ),
                     sliderInput(
                       "season_leader",
                       "Choose Season",
                       value = c(min(player_data$Season),  max(player_data$Season)),
                       step = 10001,
                       sep = "",
                       min = min(player_data$Season),
                       max = max(player_data$Season)
                     ),
                     actionButton("submit_buttom_leader",
                                  "Submit",
                                  width = "50%")
                   ),
                   style = "width: 75%"
                 ),
                 mainPanel(plotOutput("leagueChart"))
               )),
      tabPanel(
        "Player Profile",
        fluidRow(
          column(
            3,
            selectizeInput(
              "player_select",
              "Select Player",
              choices = all_players,
              multiple = F
            ),
            selectInput(
              "season_profile",
              "Select Season",
              choices = seq(min(player_data$Season), max(player_data$Season), by = 10001),
              selected = 20172018
            ),
            
            checkboxInput("isGP", "Per Game", value = F)
          )
          ,
          column(1,
                 htmlOutput("picture")),
          column(6,
                 htmlOutput("playerInfo")),
          plotOutput("playerProfs", width = "75%")
        )
        
        
      ),
      tabPanel("Player Comparison",
               div(htmlOutput("tableauComp")),
               align = "center"),
      tabPanel(
        "Advanced Stats",
        fluidRow(
          column(
            2,
            selectInput(
              "position_ps",
              "Position",
              choices = c("F", "D", "All"),
              multiple = FALSE,
              selected = "All"
            )
          ),
          column(
            2,
            selectInput(
              "team_table_ps",
              "Team",
              choices = c(team_names, "All"),
              multiple = F,
              selected = "All"
            )
          ),
          column(
            2,
            sliderInput(
              "season_ps",
              "Season",
              min = min(player_data$Season),
              max = max(player_data$Season),
              sep = "",
              step = 10001,
              value = c(min(player_data$Season), max(player_data$Season))
            )
          ),
          column(3,
                 radioButtons(
                   "pergame_ps",
                   "Select Rate",
                   c("Totals", "per Game", "per 60")
                 )),
          column(3,
                 actionButton("submit_button_ps",
                              "Submit",
                              width = "60%"))
        ),
        div(dataTableOutput("pointShares"), style = "font-size: 80%; width: 100%")
      )
    ),
    #Teams
    # Teams ----
    navbarMenu(
      "Teams",
      tabPanel(
        "Team Stats",
        fluidRow(
          column(
            2,
            selectInput(
              "team_selection",
              "Team",
              choices = c(team_names, "All"),
              multiple = F,
              selected = "All"
            )
          ),
          column(
            2,
            sliderInput(
              "team_season",
              "Season",
              min = min(player_data$Season),
              max = max(player_data$Season),
              sep = "",
              step = 10001,
              value = c(min(player_data$Season), max(player_data$Season))
            )
          ),
          column(
            4,
            radioButtons(
              "team_aggregate",
              "Choose View",
              c("Game", "Season", "Aggregate Seasons"),
              selected = "Season"
            )
          ),
          column(
            3,
            actionButton("submit_button_team",
                         "Submit",
                         width = "50%"),
            conditionalPanel(
              condition = "input.team_aggregate  == 'Season' || input.team_aggregate == 'Aggregate Seasons'",
              checkboxInput("per_game_team",
                            "Per Game Rates",
                            value = F)
            )
          )
        ),
        div(dataTableOutput("teamStats"), style = "font-size: 90%; width: 95%")
      ),
      tabPanel("Projected Points",
               div(htmlOutput("tableauPoints"),
                   align = "center"))
    ),
    #Shot Chart
    # Charts ----
    navbarMenu(
      "Location Charts",
      tabPanel(
        "Simple Shot Chart",
        fluidRow(
          h6("note: coordinates were not recorded for 15-16"),
          column(
            3,
            selectizeInput(
              "player_sc",
              "Player",
              choices = c("All", player_names),
              multiple = F,
              selected = "All"
            )
          ),
          column(
            3,
            selectInput(
              "team_sc",
              "Team",
              choices = c(team_names, "All"),
              selected = "All",
              multiple = F
            )
          ),
          column(
            3,
            selectizeInput(
              "state_sc",
              "Strength State",
              choices = c("ALL", "EVEN", "5v5", "PP", "PK"),
              selected = "ALL",
              multiple = T
            )
          ),
          column(
            3,
            sliderInput(
              "shot_chart_season",
              "Season",
              min = 20162017,
              max = max(player_data$Season),
              step = 10001,
              sep = "",
              value = c(20162017, max(player_data$Season))
            )
          )
        ),
        fluidRow(column(
          3,
          actionButton("submit_button_shotchart",
                       "Submit",
                       width = "60%")
        )),
        plotOutput("shotChart")
      ),
      tabPanel("Tableau Game Events",
               div(htmlOutput("tableauShotChart"), align = "center"))
    ),
    #Games ----
    navbarMenu(
      "Games",
      tabPanel(
        "Play by Play Viewer",
        fluidRow(column(
          4,
          selectInput(
            "game_code",
            "Game",
            choices = game_codes,
            selected = last(game_codes)
          )
        )),
        div(dataTableOutput("pbpViewer"), style = "font-size: 75%; width: 90%")
      ),
      tabPanel(
        "Flow Chart",
        fluidRow(column(
          4,
          selectInput(
            "game_id2",
            "Game",
            choices = game_codes,
            selected = last(game_codes)
          )
        ),
        column(4, h6(
          HTML("vertical line denotes a penalty</br>")
        ))),
        plotOutput("shotFlow")
      )
    ),
    # Standings ----
    tabPanel(
      "Standings",
      fluidRow(h4("Season Standings"),
               column(
                 3,
                 selectInput(
                   "season_standings",
                   "Season",
                   choices = seq(min(player_data$Season), 20182019, by = 10001),
                   selected = 20182019
                 )
               )),
      dataTableOutput("standing")
    ),
    # Glossary ----
    navbarMenu(
      "More",
      tabPanel("Sources",
               h2("Sources"),
               h6(
                 HTML(
                   "Play by play data acquired from the NWHL (nwhl.zone) collected via <a href='https://github.com/jflancer/nwhl-scraper'>NWHL Scraper</a> by Jake Flancer
                   <br> All images, player data, and standings data from <a href = 'https://www.nwhl.zone/'> nwhl.zone </a>
                   <br> Website Created and Designed by Jake Flancer
                   <br> Server and Backend Managed by Alyssa Longmuir
                   <br> Tableau creations by Alyssa Longmuir and Carlie (@quarkyhockey)
                   <br> eTOI data via CJ Turtoro
                   <br> xG data via Alyssa Longmuir
                   <br> Game Score via Shawn Ferris adapted from Dom Luszczyszyn
                   <br> Point Shares via Jake Flancer adapted from hockey-reference
                   <br> Rink code via Prashanth Iyer
                   <br> Additionally, thank you to all that provided help and feedback during the design process
                   "
                 )
                 )),
      tabPanel(
        "Glossary",
        h2("Glossary"),
        h4("Explanations of Abbreviations/Stats"),
        h6(
          "GP- Games Played | G- Goals | A1- Primary Assists | A2- Secondary Assists"
        ),
        h6("PTS- Points = G + A1 + A2 | PrPTS- Primary Points"),
        h6("SOG- Shots on Goal | FOW/FOL- Face-off Win/Loss"),
        h6(
          "PIM- Penalty Minute | Blk- Blocks | TO- Turnovers | Sh% or Sh.- Shooting Percentage"
        ),
        h6("SV- Saves | GA- Goals Allowed | Sv.- Save Percentage"),
        h6("PDO- Sh% + Sv% | GF/GA + SF/SA- Goals/Shots + For/Against"),
        h6(
          "Plus/Minus- Conventional Plus/Minus Calculation | GF%- GF/(GF+GA)"
        ),
        h6("PP/pp + Sh/sh + 5v5 - Denotes a powerplay / shorthanded / 5v5 event"),
        h6(
          "GS- Game Score created by Shawn Ferris at https://hockey-graphs.com/2018/03/22/an-introduction-to-nwhl-game-score/"
        ),
        h6(
          "PS- Point Shares created by Jake Flancer adapted from https://www.hockey-reference.com/about/point_shares.html"
        ),
        h6(
          "OPS- Offensive Point Shares- measures offensive production | DPS- Defensive Point Shares- measures defensive production"
        ),
        h6(
          "ixG- Number of goals a player is expected to score given the quality of their shot attempts, created by Alyssa Longmuir"
        ),
        h6(
          "eTOI- A player's expected TOI given their usage relative to their team, created by CJ Turturo"
        )
      )
                 )
    #----
      )
    )