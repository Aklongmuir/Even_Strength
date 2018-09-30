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
                      dplyr::rename("Plus" = "eGF",
                             "Minus" = "eGA") %>%
                      mutate('Sh%' = round((G / SOG) * 100, 2)) %>%
                      select(Player:TO, 'Sh%', SV, GA) %>%
                      dplyr::rename("GF." = "GF%")
                    
                    new_data <-
                      if (input$pergame == "per Game" &
                          input$aggregate == "Season") {
                        aggregate_option %>%
                          mutate(
                            Season = as.character(Season),
                            GP = as.character(GP),
                            GF. = as.character(GF.)
                          ) %>%
                          mutate_if(is.numeric, funs(round(. / as.numeric(GP), 2))) %>%
                          mutate(Season = as.numeric(Season),
                                 'Sh%' = round((G / SOG) * 100, 2))
                      } else if (input$pergame == "per Game" &
                                 input$aggregate == "Career") {
                        aggregate_option %>%
                          mutate(GP = as.character(GP),
                                 GF. = as.character(GF.)) %>%
                          mutate_if(is.numeric, funs(round(. / as.numeric(GP), 2))) %>%
                          mutate('Sh%' = round((G / SOG) * 100, 2))
                      } else if (input$pergame == "per 60" &
                                 input$aggregate == "Season") {
                        aggregate_option %>%
                          filter(eTOI > 0, Position != "G") %>%
                          mutate(
                            Season = as.character(Season),
                            GP = as.character(GP),
                            eTOI = as.character(eTOI),
                            GF. = as.character(GF.)
                          ) %>%
                          mutate_if(is.numeric, funs(round((
                            . / as.numeric(eTOI)
                          ) * 60, 2))) %>%
                          mutate(
                            Season = as.numeric(Season),
                            eTOI = as.numeric(eTOI),
                            'Sh%' = round((G / SOG) * 100, 2)
                          )
                      } else if (input$pergame == "per 60" &
                                 input$aggregate == "Career") {
                        aggregate_option %>%
                          filter(eTOI > 0, Position != "G") %>%
                          mutate(GP = as.character(GP),
                                 eTOI = as.character(eTOI),
                                 GF. = as.character(GF.)) %>%
                          mutate_if(is.numeric, funs(round((
                            . / as.numeric(eTOI)
                          ) * 60, 2))) %>%
                          mutate('Sh%' = round((G / SOG) * 100, 2))
                      } else{
                        aggregate_option
                      }
                    new_data <- rename(new_data, "GF%" = "GF.")
                    if (input$aggregate != "Game") {
                      new_data$eTOI <- ifelse(new_data$Position == "G", 0, new_data$eTOI)
                    }
                    #removes values of SOG and sh% for the 2016 season because
                    #SOG were not recorded
                    #Changed: Matt Barlowe 9-29-2018
                    new_data[['Sh%']][which(new_data$Season == 20152016)] <- NA_integer_
                    new_data$SOG[which(new_data$Season == 20152016)] <- NA_integer_
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
          "$(this.api().table().header()).css({'background-color': '#0FD', 'color': '#000'});",
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
  })
  
  # League Leaders
  leadersData <-
    eventReactive(eventExpr = input$submit_buttom_leader,
                  ignoreNULL = F,
                  {
                    selected_position <- if (input$leader_position == "All") {
                      all_positions
                    } else {
                      input$leader_position
                    }
                    selected_team <-
                      if (input$leader_team == "All") {
                        team_names
                      } else {
                        input$leader_team
                      }
                    
                    data_set <- player_data %>%
                      filter(
                        Position %in% selected_position,
                        Team %in% selected_team,
                        Season %in% min(input$season_leader):max(input$season_leader)
                      ) %>%
                      group_by(Player, Position, Team, Season) %>%
                      group_by(GP = n(), add = TRUE) %>%
                      summarise_if(is.numeric, sum, na.rm = T) %>%
                      mutate(GF. = ifelse((eGF + eGA) > 0, round(eGF / (eGF + eGA), 2), NA_integer_)) %>%
                      select(Player, Position, Team, Season, PrPTS, G, A1, A2, GP) %>%
                      filter(GP > input$GP_filter) %>%
                      mutate_if(funs(is.numeric(.) &
                                       input$leader_pergame == T), funs(round(. / GP, 2))) %>%
                      arrange(desc(PrPTS)) %>%
                      head(20) %>%
                      ungroup() %>%
                      mutate(
                        isSingle = min(input$season_leader) == max(input$season_leader),
                        Player = ifelse(isSingle, Player, paste(Player, Season))
                      ) %>%
                      melt(
                        id.vars = c("Player", "Position", "Team", "PrPTS"),
                        measure.vars = c("G", "A1", "A2")
                      )
                    
                    data_set$Player <-
                      factor(data_set$Player,
                             levels = unique(data_set$Player[order(data_set$PrPTS)]),
                             ordered = T)
                    
                    data_set
                    
                  })
  output$leagueChart <- renderPlot({
    ggplot(leadersData()) +
      geom_col(aes(x = Player, y = value, fill = variable),
               position = position_stack(reverse = T)) +
      scale_fill_manual(
        "",
        values = c("indianred", "navyblue", "skyblue"),
        labels = c("G", "A1", "A2"),
        drop = F
      ) +
      coord_flip() +
      theme_classic() +
      xlab('') +
      ylab("Points") +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  })
  
  # Player Profile
  player_info <- reactive({
    test1 <- player_data %>%
      filter(Season == as.numeric(input$season_profile)) %>%
      select(-Season) %>%
      group_by(Player, Position) %>%
      summarise_if(is.numeric, sum, na.rm = T) %>%
      mutate(GF. = ifelse((eGF + eGA) > 0, round(eGF / (eGF + eGA), 2), NA_integer_)) %>%
      ungroup()
    test2 <- player_data %>%
      filter(Season == as.numeric(input$season_profile)) %>%
      select(-Season) %>%
      group_by(Player, Position) %>%
      summarise(GP = n())
    test <- if (input$isGP == T) {
      inner_join(test1, test2, by = c("Player", "Position")) %>%
        group_by(Position) %>%
        mutate_at(vars(GS, SV, GA, G, PTS, PrPTS, SOG, eGF, eGA),
                  funs("PG" = round(. / GP, 2))) %>%
        mutate_if(is.numeric, funs("01" = standardize)) %>%
        filter(Player == input$player_select)
    } else{
      inner_join(test1, test2, by = c("Player", "Position")) %>%
        group_by(Position) %>%
        mutate_if(is.numeric, funs("01" = standardize)) %>%
        filter(Player == input$player_select)
    }
    if (nrow(test) == 0) {
      return(NULL)
    }
    
    if (test$Position == "G" & input$isGP == T) {
      melted <- melt(test, id.vars = c(1:2))
      melted_data <-
        melted %>% filter(variable %in% c("GS_PG", "SV_PG", "GA_PG"))
      melted_label <-
        melted %>% filter(variable %in% c("GS_PG_01", "SV_PG_01", "GA_PG_01"))
      melted <- cbind(melted_data, melted_label[, 4])
      colnames(melted)[5] <- "percentile"
      #Renames everything
      melted2 <- melted %>%
        mutate(
          variable = case_when(
            variable == "SV_PG" ~ "Saves per game",
            variable == "GA_PG" ~ "Goals Allowed per game",
            variable == "GS_PG" ~ "Game Score per game"
          )
        )
    } else if (test$Position != "G" & input$isGP == T) {
      melted <- melt(test, id.vars = c(1:2))
      melted_data <-
        melted %>% filter(
          variable %in% c(
            "G_PG",
            "PTS_PG",
            "PrPTS_PG",
            "GS_PG",
            "SOG_PG",
            "eGF_PG",
            "eGA_PG"
          )
        )
      melted_label <-
        melted %>% filter(variable %in% paste(
          c("G", "PTS", "PrPTS", "GS", "SOG", "eGF", "eGA"),
          "_PG_01",
          sep = ''
        ))
      melted <- cbind(melted_data, melted_label[, 4])
      colnames(melted)[5] <- "percentile"
      #Renames everything
      melted2 <- melted %>%
        mutate(
          variable = case_when(
            variable == "G_PG" ~ "Goals per game",
            variable == "PTS_PG" ~ "Points per game",
            variable == "PrPTS_PG" ~ "Primary Points per game",
            variable == "GS_PG" ~ "Game Score per game",
            variable == "SOG_PG" ~ "Shots on Goal per game",
            variable == "eGF_PG" ~ "5v5 Goals For per game",
            variable == "eGA_PG" ~ "5v5 Goals Against per game"
          )
        )
    } else if (test$Position == "G" & input$isGP == F) {
      melted <- melt(test, id.vars = c(1:2))
      melted_data <-
        melted %>% filter(variable %in% c("GS", "SV", "GA"))
      melted_label <-
        melted %>% filter(variable %in% c("GS_01", "SV_01", "GA_01"))
      melted <- cbind(melted_data, melted_label[, 4])
      colnames(melted)[5] <- "percentile"
      #Renames everything
      melted2 <- melted %>%
        mutate(
          variable = case_when(
            variable == "SV" ~ "Saves",
            variable == "GA" ~ "Goals Allowed",
            variable == "GS" ~ "Game Score"
          )
        )
    } else{
      melted <- melt(test, id.vars = c(1:2))
      melted_data <-
        melted %>% filter(variable %in% c("G", "PTS", "PrPTS", "GS", "SOG", "eGF", "eGA"))
      melted_label <-
        melted %>% filter(variable %in% paste(
          c("G", "PTS", "PrPTS", "GS", "SOG", "eGF", "eGA"),
          "_01",
          sep = ''
        ))
      melted <- cbind(melted_data, melted_label[, 4])
      colnames(melted)[5] <- "percentile"
      #Renames everything
      melted2 <- melted %>%
        mutate(
          variable = case_when(
            variable == "G" ~ "Goals",
            variable == "PTS" ~ "Points",
            variable == "PrPTS" ~ "Primary Points",
            variable == "GS" ~ "Game Score",
            variable == "SOG" ~ "Shots on Goal",
            variable == "eGF" ~ "5v5 Goals For",
            variable == "eGA" ~ "5v5 Goals Against"
          )
        )
    }
    
    melted2
  })
  output$playerProfs <- renderPlot({
    data <- player_info()
    
    if (!is.null(data)) {
      ggplot(data) +
        geom_col(aes(x = variable,
                     y = percentile,
                     fill = percentile), color = "black") +
        geom_hline(yintercept = 1,
                   col = "black",
                   size = 2) +
        geom_text(aes(
          x = variable,
          y = (percentile - 0.1),
          label = value
        ), size = 12) +
        scale_fill_gradientn(
          colors = c("red", "white", "blue"),
          values = c(0, 0.5, 1),
          limits = c(0, 1)
        ) +
        scale_y_continuous(limits = c(0, 1),
                           breaks = seq(0.2, 0.8, 0.2)) +
        ylab("Position Percentile") +
        xlab("") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16)) +
        theme_bw() +
        coord_flip()
    } else{
      ggplot(data.frame())
    }
  })
  output$picture <- renderText({
    roster_data <- filter(roster_data, Season == input$season_profile)
    if (input$season_profile == 2015) {
      "no image available"
    } else {
      c('<img src="',
        roster_data$thumbnail[roster_data$Player == input$player_select],
        '">')
    }
    
    
  })
  output$playerInfo <- renderText({
    roster_data <- filter(roster_data, Season == input$season_profile)
    a <-
      paste("Position", roster_data$position[roster_data$Player == input$player_select])
    player_url <-
      roster_data$url[roster_data$Player == input$player_select]
    if (length(player_url) != 0) {
      table <- readHTMLTable(getURL(player_url), header = F)[[1]]
      if (!is.null(table)) {
        tabletext <- apply(table, 1, paste, collapse = ": ")
        paste(c(a, tabletext), collapse = "<br>")
      } else{
        "           bio not available"
      }
    } else {
      "           bio not available"
    }
  })
  observe({
    players <- filter(roster_data, Season == input$season_profile)
    names <- paste(players$first_name, players$last_name)
    
    names <- names[order(players$last_name)]
    
    updateSelectizeInput(
      session,
      "player_select",
      label = "Select Player",
      choices = names,
      selected = input$player_select
    )
  })
  
  # Player Comparison Tableau
  output$tableauComp <- renderUI({
    HTML(
      "<div class='tableauPlaceholder' id='viz1536291250502' style='position: relative'><noscript><a href='#'><img alt='Comparison Dashboard ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;NW&#47;NWHLTwo-PlayerComparisonTool&#47;ComparisonDashboard&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='NWHLTwo-PlayerComparisonTool&#47;ComparisonDashboard' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;NW&#47;NWHLTwo-PlayerComparisonTool&#47;ComparisonDashboard&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1536291250502');                    var vizElement = divElement.getElementsByTagName('object')[0];                    vizElement.style.width='820px';vizElement.style.height='527px';                    var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>"
    )
  })
  
  # Point Shares
  ps <-
    eventReactive(eventExpr = input$submit_button_ps,
                  ignoreNULL = F,
                  {
                    selected_position <- if (input$position_ps == "All") {
                      all_positions
                    } else if (input$position_ps == "Skaters") {
                      c("F", "D")
                    } else {
                      input$position_ps
                    }
                    selected_team <-
                      if (input$team_table_ps == "All") {
                        team_names
                      } else {
                        input$team_table_ps
                      }
                    
                    a <- pointshare_data %>%
                      filter(
                        Team %in% selected_team,
                        Pos %in% selected_position,
                        Season %in% min(input$season_ps):max(input$season_ps)
                      ) %>%
                      left_join(toi_data, by = c("Player", "Team", "Season")) %>%
                      select(-G:-A2) %>%
                      select(Season, Player, Team, Pos, eTOI, everything()) %>%
                      mutate(
                        Season = as.character(Season),
                        GP = as.character(GP),
                        eTOI = as.character(eTOI)
                      ) #%>%
                      #left_join(xg_data, by = c("Player", "Team", "Season"))
                    
                    b <- if (input$pergame_ps == "per Game") {
                      a %>%
                        mutate(eTOI = as.numeric(eTOI)) %>%
                        mutate_if(is.numeric, funs(round(. / as.numeric(GP), 2)))
                    } else if (input$pergame_ps == "per 60") {
                      a %>%
                        mutate_if(is.numeric, funs(round(. / as.numeric(eTOI) * 60, 2)))
                    } else{
                      a
                    }
                    b %>% mutate(GP = as.numeric(GP), eTOI = as.numeric(eTOI))
                  })
  output$pointShares <- renderDataTable({
    datatable(
      ps(),
      class = 'cell-border stripe',
      style = 'bootstrap',
      selection = "single",
      options = list(
        pageLength = 25,
        searchHighlight = TRUE,
        scrollX = T,
        scrollY = '500px',
        order = list(list(8, 'desc')),
        columnDefs = list(list(
          orderSequence = c('desc', 'asc'),
          targets = "_all"
        )),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#0FD', 'color': '#000'});",
          "}"
        )
      ),
      rownames = F
    )
  })
  
  #Teams ----
  
  # Team Table
  teamData <-
    eventReactive(eventExpr = input$submit_button_team,
                  ignoreNULL = F,
                  {
                    selected_team <- if (input$team_selection == "All") {
                      team_names
                    } else {
                      input$team_selection
                    }
                    team_data <- filter(
                      team_data,
                      Team %in% selected_team,
                      Season %in% min(input$team_season):max(input$team_season)
                    )
                    if (input$team_aggregate == "Season") {
                      data1 <- team_data %>%
                        group_by(Team, Season) %>%
                        summarise_if(is.numeric, sum, na.rm = T) %>%
                        select(-game_id) %>%
                        dplyr::rename("SF_5v5." = "SF%_5v5",
                               "SF." = "SF%") %>%
                        mutate(
                          SF. = round((SF / (SF + SA)) * 100, 2),
                          SF_5v5. = round((SF_5v5 / (SF_5v5 + SA_5v5)) * 100, 2),
                          Sh. = round((GF / SF) * 100, 2),
                          Sv. = round((1 - GA / SA) * 100, 2),
                          PDO = Sh. + Sv.
                        )
                      data2 <- team_data %>%
                        group_by(Team, Season) %>%
                        summarise(GP = n())
                      inner_join(data1, data2, by = c("Team", "Season")) %>%
                        select(Team, Season, GP, everything()) %>%
                        mutate(Season = as.character(Season),
                               GP = as.character(GP)) %>%
                        mutate_if(funs(is.numeric(.) &
                                         input$per_game_team == T),
                                  funs(round(. / as.numeric(GP), 2))) %>%
                        mutate(
                            SF. = round((SF / (SF + SA)) * 100, 2),
                            SF_5v5. = round((SF_5v5 / (SF_5v5 + SA_5v5)) * 100, 2),
                            Sh. = round((GF / SF) * 100, 2),
                            Sv. = round((1 - GA / SA) * 100, 2),
                            PDO = Sh. + Sv.
                        ) %>%
                        dplyr::rename("SF%_5v5" = "SF_5v5.",
                               "SF%" = "SF.", 'SH%'="Sh.", "SV%"="Sv.")
                    } else if (input$team_aggregate == "Aggregate Seasons") {
                      data1 <- team_data %>%
                        group_by(Team) %>%
                        summarise_if(is.numeric, sum, na.rm = T) %>%
                        select(-game_id, -Season) %>%
                        dplyr::rename("SF_5v5." = "SF%_5v5",
                               "SF." = "SF%") %>%
                        mutate(
                            SF. = round((SF / (SF + SA)) * 100, 2),
                            SF_5v5. = round((SF_5v5 / (SF_5v5 + SA_5v5)) * 100, 2),
                            Sh. = round((GF / SF) * 100, 2),
                            Sv. = round((1 - GA / SA) * 100, 2),
                            PDO = Sh. + Sv.
                        )
                      data2 <- team_data %>%
                        group_by(Team) %>%
                        summarise(GP = n())
                      inner_join(data1, data2, by = c("Team")) %>%
                        select(Team, GP, everything()) %>%
                        mutate(GP = as.character(GP)) %>%
                        mutate_if(funs(is.numeric(.) &
                                         input$per_game_team == T),
                                  funs(round(. / as.numeric(GP), 2))) %>%
                        mutate(
                            SF. = round((SF / (SF + SA)) * 100, 2),
                            SF_5v5. = round((SF_5v5 / (SF_5v5 + SA_5v5)) * 100, 2),
                            Sh. = round((GF / SF) * 100, 2),
                            Sv. = round((1 - GA / SA) * 100, 2),
                            PDO = Sh. + Sv.
                        ) %>%
                        dplyr::rename("SF%_5v5" = "SF_5v5.",
                               "SF%" = "SF.", 'SH%'="Sh.", "SV%"="Sv.")
                    } else{
                      select(team_data, Season, everything())
                    }
                  })
  output$teamStats <- renderDataTable({
    b <- datatable(
      teamData(),
      class = 'cell-border stripe',
      style = 'bootstrap',
      options = list(
        searchHighlight = TRUE,
        scrollX = T,
        columnDefs = list(list(
          orderSequence = c('desc', 'asc'),
          targets = "_all"
        )),
        #order = list(list(sort_index, 'desc')),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#0FD', 'color': '#000'});",
          "}"
        )
      ),
      rownames = F
    )
    # for(i in start_index:length(colnames(teamData()))){
    #   b <- b %>%
    #     formatStyle(colnames(teamData())[i],
    #                 background = styleColorBar(range(teamData()[,i], na.rm = T),
    #                                            'lightblue',
    #                                            angle = -90),
    #                 backgroundPosition = 'left',
    #                 backgroundRepeat = 'no-repeat',
    #                 textAlign = "left")
    # }
    b
  })
  
  # Projected Points Tableau
  output$tableauPoints <- renderUI({
    HTML(
      "<div class='tableauPlaceholder' id='viz1536290907106' style='position: relative'><noscript><a href='#'><img alt='Dashboard 1 ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;NW&#47;NWHL2019ProjectedGoals&#47;Dashboard1&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='NWHL2019ProjectedGoals&#47;Dashboard1' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;NW&#47;NWHL2019ProjectedGoals&#47;Dashboard1&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1536290907106');                    var vizElement = divElement.getElementsByTagName('object')[0];                    if ( divElement.offsetWidth > 800 ) { vizElement.style.minWidth='420px';vizElement.style.maxWidth='650px';vizElement.style.width='100%';vizElement.style.minHeight='827px';vizElement.style.maxHeight='887px';vizElement.style.height=(divElement.offsetWidth*0.75)+'px';} else if ( divElement.offsetWidth > 500 ) { vizElement.style.minWidth='420px';vizElement.style.maxWidth='650px';vizElement.style.width='100%';vizElement.style.minHeight='827px';vizElement.style.maxHeight='887px';vizElement.style.height=(divElement.offsetWidth*0.75)+'px';} else { vizElement.style.width='100%';vizElement.style.height='727px';}                     var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>"
    )
  })
  
  #Charts ----
  
  # Simple Shot Chart
  shotData <-
    eventReactive(eventExpr = input$submit_button_shotchart,
                  ignoreNULL = F,
                  {
                    selected_states <-
                      if (input$state_sc == "ALL") {
                        states
                      } else if (input$state_sc == "EVEN") {
                        c("5v5", "6v6", "4v4", "3v3")
                      } else if (input$state_sc == "5v5") {
                        "5v5"
                      } else if (input$state_sc == "PP") {
                        c("5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
                      } else if (input$state_sc == "PK") {
                        c("4v5", "3v5", "3v4", "5v6", "4v6", "3v6")
                      }
                    selected_team <-
                      if (input$team_sc == "All") {
                        team_names
                      } else{
                        input$team_sc
                      }
                    selected_player <-
                      if (input$player_sc == "All") {
                        player_names
                      } else{
                        input$player_sc
                      }
                    
                    b <- pbp_data %>%
                      filter(
                        event_player_1 %in% selected_player,
                        event_team %in% selected_team,
                        state %in% selected_states,
                        Season %in% min(input$shot_chart_season):max(input$shot_chart_season),
                        event_type %in% c("Shot", "Goal")
                      ) %>%
                      select(event_type, x_coord_1, y_coord_1) %>%
                      bind_rows(a)
                  })
  output$shotChart <- renderPlot({
    if (nrow(shotData()) == 2) {
      options(warn = -1)
      plot <- rink
      options(warn = 0)
    } else{
      shot_count <- shotData() %>% summarise(n())
      options(warn = -1)
      plot <- rink +
        geom_point(
          data = shotData(),
          aes(x = x_coord_1,
              y = y_coord_1,
              color = event_type),
          size = 5,
          alpha = 0.5
        ) +
        scale_color_manual(
          values = c("darkgreen", "navyblue"),
          labels = c("Goal", "Shot on Goal"),
          drop = F
        ) +
        labs(color = "Shot Type") +
        theme(
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          plot.title = element_text(size = 18, hjust = 0.5)
        ) +
        ggtitle(paste(shot_count, "Shots on Goal"))
      options(warn = 0)
    }
    return(plot)
  })
  observe({
    selected_team <-
      if (input$team_sc == "All" | !isTruthy(input$player_sc)) {
        team_names
      } else{
        input$team_sc
      }
    
    players <- unique(
      filter(
        pbp_data,
        Season %in% min(input$shot_chart_season):max(input$shot_chart_season),
        event_team %in% selected_team,
        event_type %in% c("Shot", "Goal")
      )$event_player_1
    )
    players <- gsub("\\s+", " ", players[!is.na(players)])
    
    last_name <-
      unlist(lapply(
        strsplit(players, " "),
        FUN = function(x) {
          x[2]
        }
      ))
    
    names <- players[order(last_name)]
    
    updateSelectizeInput(
      session,
      "player_sc",
      "Player",
      choices = c("All", names),
      selected = input$player_sc
    )
  })
  observe({
    selected_player <-
      if (input$player_sc == "All" | !isTruthy(input$player_sc)) {
        player_names
      } else{
        input$player_sc
      }
    
    teams <- unique(
      filter(
        pbp_data,
        Season %in% min(input$shot_chart_season):max(input$shot_chart_season),
        event_player_1 %in% selected_player
      )$event_team
    )
    
    updateSelectInput(
      session,
      "team_sc",
      "Team",
      choices = c("All", teams),
      selected = input$team_sc
    )
  })
  observe({
    selected_player <-
      if (input$player_sc == "All" | !isTruthy(input$player_sc)) {
        player_names
      } else{
        input$player_sc
      }
    
    selected_team <-
      if (input$team_sc == "All" | !isTruthy(input$team_sc)) {
        team_names
      } else{
        input$team_sc
      }
    
    seasons <-
      unique(pbp_data$Season[which(
        pbp_data$event_player_1 %in% selected_player &
          pbp_data$event_team %in% selected_team &
          !is.na(pbp_data$x_coord) &
          pbp_data$event_type %in% c("Shot", "Goal")
      )])
    updateSliderInput(
      session,
      "shot_chart_season",
      "Season",
      min = min(seasons),
      max = max(seasons),
      #changed step
      #Changed: Matt Barlowe 9-29-2018
      step = 10001,
      value = c(min(seasons), max(seasons))
                
    )
  })
  
  # Tableau Shot Chart
  output$tableauShotChart <- renderUI({
    HTML(
      "<div class='tableauPlaceholder' id='viz1536299668550' style='position: relative'><noscript><a href='#'><img alt='17-18 NWHL Game Events ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;NW&#47;NWHLGameEvents&#47;17-18NWHLGameEvents&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='NWHLGameEvents&#47;17-18NWHLGameEvents' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;NW&#47;NWHLGameEvents&#47;17-18NWHLGameEvents&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1536299668550');                    var vizElement = divElement.getElementsByTagName('object')[0];                    vizElement.style.minWidth='420px';vizElement.style.maxWidth='1050px';vizElement.style.width='100%';vizElement.style.minHeight='587px';vizElement.style.maxHeight='887px';vizElement.style.height=(divElement.offsetWidth*0.75)+'px';                    var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>"
    )
  })
  
  #Games ----
  
  #Text Viewer
  pbp_game <- reactive({
    pbp_data %>%
      mutate(game = paste(game_date, home_team, away_team)) %>%
      filter(game == input$game_code) %>%
      select(period,
             min,
             sec,
             event_team,
             event_type,
             event_player_1,
             home_score,
             away_score) %>%
      dplyr::rename("Event" = "event_type",
             "Player" = "event_player_1")
  })
  output$pbpViewer <- renderDataTable({
    datatable(
      pbp_game(),
      #style = 'compact',
      options = list(
        scrollX = F,
        scrollY = '400px',
        scrollCollapse = T,
        searching = F,
        ordering = F,
        lengthChange = F,
        paging = F,
        info = F,
        filter = 'none',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#0FD', 'color': '#000'});",
          "}"
        )
      ),
      rownames = F
    )
  })
  
  # Flow Chart
  pbp_flow <- reactive({
    test <- pbp_data %>%
      mutate(game = paste(game_date, home_team, away_team)) %>%
      filter(game == input$game_id2) %>%
      group_by(event_team) %>%
      mutate(
        isShot = ifelse(event_type %in% c("Shot", "Goal"), 1, 0),
        isGoal = ifelse(event_type == "Goal", 1, 0),
        isPenalty = ifelse(event_type == "Penalty", 1, 0),
        Shots = cumsum(isShot)
      ) %>%
      filter(isShot == 1 | isPenalty == 1) %>%
      select(
        game_seconds,
        event_team,
        home_team,
        away_team,
        home_skaters,
        away_skaters,
        isShot,
        isGoal,
        isPenalty,
        Shots,
        period
      )
    test <- bind_rows(
      test,
      data.frame(
        game_seconds = c(0, 0, 1200 * max(test$period), 1200 * max(test$period)),
        Shots = c(0, 0,
                  last(test$Shots[which(test$event_team == test$home_team)]),
                  last(test$Shots[which(test$event_team != test$home_team)])),
        event_team = c(
          test$home_team[1],
          test$away_team[1],
          test$home_team[1],
          test$away_team[1]
        ),
        home_team = rep(test$home_team[1], 4),
        away_team = rep(test$away_team[1], 4)
      )
    )
  })
  output$shotFlow <- renderPlot({
    test <- pbp_flow()
    ggplot(test) +
      geom_step(aes(game_seconds / 60, Shots, color = event_team == home_team)) +
      geom_point(
        data = filter(test, isGoal == 1),
        aes(game_seconds / 60, Shots, color = event_team == home_team),
        size = 3
      ) +
      labs(x = "Minutes", y = "Shots For") +
      scale_color_manual(
        name = "Team",
        breaks = c("TRUE", "FALSE"),
        labels = c(test$home_team[1], test$away_team[1]),
        values = c("indianred", "navyblue")
      ) +
      geom_vline(xintercept = test$game_seconds[which(test$isPenalty == 1 &
                                                        test$event_team != test$home_team)] / 60,
                 color = "lightsalmon") +
      geom_vline(xintercept = test$game_seconds[which(test$isPenalty == 1 &
                                                        test$event_team == test$home_team)] / 60,
                 color = "skyblue") +
      ggtitle(paste(
        first(test$home_team),
        sum(test$isGoal[which(test$event_team == test$home_team)], na.rm = T),
        first(test$away_team),
        sum(test$isGoal[which(test$event_team == test$away_team)], na.rm = T)
      )) +
      theme_classic()
  }, height = 400, width = 1000)
  
  #Standings ----
  
  #Standings Page
  output$standing <- renderDataTable({
    season <- substr(as.character(input$season_standings), 1, 4)
    seasonid <- seasons$id[which(seasons$Season == season)]
    otherid <- seasons$otherid[which(seasons$Season == season)]
    url <-
      getURL(
        paste(
          "https://www.nwhl.zone/standings/show/",
          otherid,
          "?subseason=",
          seasonid,
          sep = ""
        )
      )
    ####----
    table <- readHTMLTable(url)[[1]]
    datatable(
      table,
      options = list(
        scrollX = F,
        scrollY = F,
        scrollCollapse = F,
        searching = F,
        ordering = F,
        lengthChange = F,
        paging = F,
        info = F,
        filter = 'none',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#0FD', 'color': '#000'});",
          "}"
        )
      ),
      rownames = F
    )
  })
  # ----
}