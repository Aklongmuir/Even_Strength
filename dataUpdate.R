# Jake Flancer
# Instructions to update data sources

#1) Run all code in nwhl_scraper.R up to line 728 (or until the line that begins pbp_ids <-...) 

#2) Get relevant game ids

# Option 1- Pulls game ids for current date
  curDate <- strsplit(as.character(Sys.Date()), split = "-")[[1]]
  pbp_ids <- day_scrape(Season = 20182019, Year = curDate[1], Month = curDate[2], Day = curDate[3])
# Option 2- Pulls game ids for custom date
  #pbp_ids <- day_scrape(Season = 20182019, Year = 2018, Month = 09, Day = 29)
  
#3) Get pbp data
  pbp_full <- compile_games(pbp_ids)
  
#4) Get summary data
  pbp_full_summary <- compile_player_summary(pbp_full)
  pbp_full_team_summary <- compile_team_summary(pbp_full)
  
#5) Update current files with new data
  add_data <- function(new_data, filepath){
    old_data <- read_csv(filepath)
    updated_data <- bind_rows(old_data, new_data)
    write_csv(updated_data, filepath)
  }
  add_data(pbp_full, "data/nwhl_pbp_all.csv")
  add_data(pbp_full_summary, "data/nwhl_games_all.csv")
  add_data(pbp_full_team_summary, "data/nwhl_team_games_all.csv")
  
#6) Update the player rosters
  roster_data <- lapply(pbp_ids, roster_info)
  roster_data <- roster_data[which(!is.na(roster_data))]
  roster_data <- do.call("bind_rows", roster_data)
  roster_data <- roster_data %>%
    filter(status == "active", roster_type == "player") %>%
    distinct(id, .keep_all = T) %>%
    mutate(Season = 2018)
  old_data <- read_csv("data/rostersall.csv")
  new_data <- rbind(old_data,roster_data) %>% distinct(id, .keep_all = T)
  write_csv(roster_data, "data/rostersall.csv")