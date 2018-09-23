#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI( 
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
                   step = 1,
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
           )))