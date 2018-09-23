#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(reshape2)
library(DT)
library(XML)
library(RCurl)
library(ggplot2)
library(dplyr)
player_data <- read.csv('playerdata.csv')
all_players <- read.csv('allplayers.csv')
all_positions <- read.csv('allpositions.csv')
team_names <- read.csv('teamnames.csv')
shinyServer(
  
  function(input, output, session) {
    #Players ----
    # Player Table
    tableData <-
      eventReactive(eventExpr = input$submit_button_player,
                    ignoreNULL = F,
                    {
                      selected_position <- if (input$position == "All") {
                        all_positions
                      } else if (input$position == "Skaters") {
                        c("F", "D")
                      } else {
                        input$position
                      }
                      selected_team <-
                        if (input$team_table == "All") {
                          team_names
                        } else {
                          input$team_table
                        }
                      
                      data <- player_data %>%
                        filter(
                          Position %in% selected_position,
                          Team %in% selected_team,
                          Season %in% min(input$season):max(input$season)
                        )
                      
                      aggregate_option <-
                        if (input$aggregate == "Season") {
                          data1 <- data %>%
                            group_by(Player, Position, Team, Season) %>%
                            summarise_if(is.numeric, sum, na.rm = T) %>%
                            mutate(GF. = ifelse((eGF + eGA) > 0, round(eGF / (eGF + eGA), 2), NA_integer_)) %>%
                            left_join(toi_data, by = c("Player", "Team", "Season"))
                          data2 <- data %>%
                            group_by(Player, Position, Team, Season) %>%
                            summarise(GP = n())
                          inner_join(data1, data2, by = c("Player", "Position", "Team", "Season")) %>%
                            select(Player:Season, GP, eTOI, G:GA) %>%
                            filter(GP %in% min(input$games_played):max(input$games_played))
                        } else if (input$aggregate == "Career") {
                          data1 <- data %>%
                            group_by(Player, Position, Team, Season) %>%
                            summarise_if(is.numeric, sum, na.rm = T) %>%
                            left_join(toi_data, by = c("Player", "Team", "Season")) %>%
                            ungroup() %>%
                            select(-Season) %>%
                            group_by(Player, Position, Team) %>%
                            summarise_if(is.numeric, sum, na.rm = T) %>%
                            mutate(GF. = ifelse((eGF + eGA) > 0, round(eGF / (eGF + eGA), 2), NA_integer_))
                          data2 <- data %>%
                            group_by(Player, Position, Team) %>%
                            summarise(GP = n())
                          inner_join(data1, data2, by = c("Player", "Position", "Team")) %>%
                            select(Player:Team, GP, eTOI, G:GA) %>%
                            filter(GP %in% min(input$games_played):max(input$games_played))
                        } else{
                          select(data, Player:Team, Season, everything())
                        }
                      aggregate_option <- aggregate_option %>%
                        rename(Plus = eGF,
                               Minus = eGA) %>%
                        mutate('Sh%' = round(G / SOG, 2)) %>%
                        select(Player:TO, 'Sh%', SV, GA)
                      
                      new_data <-
                        if (input$pergame == "per Game" &
                            input$aggregate == "Season") {
                          aggregate_option %>%
                            mutate(Season = as.character(Season),
                                   GP = as.character(GP)) %>%
                            mutate_if(is.numeric, funs(round(. / as.numeric(GP), 2))) %>%
                            mutate(Season = as.numeric(Season),
                                   'Sh%' = round(G / SOG, 2))
                        } else if (input$pergame == "per Game" &
                                   input$aggregate == "Career") {
                          aggregate_option %>%
                            mutate(GP = as.character(GP)) %>%
                            mutate_if(is.numeric, funs(round(. / as.numeric(GP), 2))) %>%
                            mutate('Sh%' = round(G / SOG, 2))
                        } else if (input$pergame == "per 60" &
                                   input$aggregate == "Season") {
                          aggregate_option %>%
                            filter(eTOI > 0, Position != "G") %>%
                            mutate(
                              Season = as.character(Season),
                              GP = as.character(GP),
                              eTOI = as.character(eTOI)
                            ) %>%
                            mutate_if(is.numeric, funs(round((
                              . / as.numeric(eTOI)
                            ) * 60, 2))) %>%
                            mutate(
                              Season = as.numeric(Season),
                              eTOI = as.numeric(eTOI),
                              'Sh%' = round(G / SOG, 2)
                            )
                        } else if (input$pergame == "per 60" &
                                   input$aggregate == "Career") {
                          aggregate_option %>%
                            filter(eTOI > 0, Position != "G") %>%
                            mutate(GP = as.character(GP),
                                   eTOI = as.character(eTOI)) %>%
                            mutate_if(is.numeric, funs(round((
                              . / as.numeric(eTOI)
                            ) * 60, 2))) %>%
                            mutate('Sh%' = round(G / SOG, 2))
                        } else{
                          aggregate_option
                        }
                      new_data$eTOI <- ifelse(new_data$Position == "G", 0, new_data$eTOI)
                      return(new_data)
                    })
    output$gameStats <- renderDataTable({
      start_index <- ifelse("Game.ID" %in% colnames(tableData()), 9, 5)
      sort_index <-
        ifelse("Game.ID" %in% colnames(tableData()), 13, 11)
      
      b <- datatable(
        tableData(),
        class = 'cell-border stripe',
        style = 'bootstrap',
        selection = "single",
        extensions = c('FixedColumns', "FixedHeader"),
        options = list(
          pageLength = 25,
          searchHighlight = TRUE,
          scrollX = T,
          order = list(list(sort_index, 'desc')),
          columnDefs = list(list(
            orderSequence = c('desc', 'asc'),
            targets = "_all"
          )),
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        ),
        rownames = F
      )
      # for(i in start_index:length(colnames(tableData()))){
      #   b <- b %>%
      #     formatStyle(colnames(tableData())[i],
      #                 background = styleColorBar(range(tableData()[,i], na.rm = T),
      #                                            'lightblue',
      #                                            angle = -90),
      #                 backgroundPosition = 'left',
      #                 backgroundRepeat = 'no-repeat',
      #                 textAlign = "left")
      # }
      b
    })})