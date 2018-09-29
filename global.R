library(dplyr)
library(shiny)
library(ggplot2)
library(reshape2)
library(DT)
library(XML)
library(RCurl)
source("RinkFunction.R")
#forcechange

#https://shiny.rstudio.com/articles/pool-dplyr.html

seasons <- data.frame(
  id = c("512423","407749", "327125", "327151"),
  Season = c("2018","2017", "2016", "2015"),
  otherid = c(4170045,3422136, 2758654, 2758856),
  stringsAsFactors = F
)

pbp_data <- 
  read.csv("data/nwhl_pbp_all.csv", stringsAsFactors = F) %>%
  mutate(Season = Season*10000 + Season + 1)
player_data <-
  read.csv("data/nwhl_games_all.csv", stringsAsFactors = F) %>%
  mutate(Season = Season*10000 + Season + 1)
roster_data <- 
  read.csv("data/rostersall.csv", stringsAsFactors = F) %>%
  mutate(Season = Season*10000 + Season + 1)
pointshare_data <-
  read.csv("data/pointshares.csv", stringsAsFactors = F) %>% rename(Pos = position) %>%
  mutate(Season = Season*10000 + Season + 1)
team_data <-
  read.csv("data/nwhl_team_games_all.csv", stringsAsFactors = F) %>%
  mutate(Season = Season*10000 + Season + 1)
toi_data <-
  read.csv("data/eTOI.csv", stringsAsFactors = F) %>%
  mutate(Season = Season*10000 + Season + 1)
xg_data <- read.csv("data/NWHLxG.csv", stringsAsFactors = F) %>%
  mutate(Season = as.character(season*10000 + season + 1),
        ixG = round(xG,2)) %>%
  select(-season, -Goals,-Difference,-xG) %>%
  rename(Player = event_player_1,Team = event_team)

toi_data$eTOI <- round(toi_data$eTOI)

dates <- pbp_data %>%
  group_by(game_id, home_team, away_team) %>%
  summarise(game_date = first(game_date))

date_parse <-
  data.frame(matrix(unlist(strsplit(
    dates$game_date, "-"
  )), ncol = 3, byrow = T))
date_order <- order(date_parse$X1, date_parse$X2, date_parse$X3)

dates <- dates[date_order,]

game_codes <-
  paste(dates$game_date, dates$home_team, dates$away_team)

roster_data$Player <-
  paste(roster_data$first_name, roster_data$last_name)

player_data <- player_data %>%
  mutate(game_id = as.character(game_id)) %>%
  rename(
    'Game.ID' = game_id,
    Date = game_date,
    Home = home_team,
    Away = away_team,
    Position = position
  ) %>%
  mutate(GF. = round(GF., 2),
         Position = ifelse(Position %in% c("G", "D"), Position, "F")) %>%
  mutate_if(is.numeric, round, 2)

rink <- fun.draw_rink() + coord_fixed() +
  xlim(20, 100) +
  ylim(-43, 43) +
  annotate(
    "segment",
    x = 20,
    xend = 89,
    y = 42.5,
    yend = 42.5
  ) +
  annotate(
    "segment",
    x = 20,
    xend = 89,
    y = -42.5,
    yend = -42.5
  )

ordering <- strsplit(unique(player_data$Player), " ")
ordering <-
  lapply(ordering, function(x) {
    a = unlist(x)
    a[nchar(a) > 1]
  })
ordering <- order(sapply(ordering, function(x)
  x[2]))

all_players <- unique(player_data$Player)[ordering]
team_names <- unique(pbp_data$event_team)

pbp_playernames <- pbp_data$event_player_1
players <- gsub("\\s+", " ", pbp_playernames)

pbp_data$event_player_1 <- players

last_name <-
  unlist(lapply(
    strsplit(players, " "),
    FUN = function(x) {
      x[2]
    }
  ))

player_names <- players[order(last_name)]

a <- as.data.frame(matrix(
  c("Goal", NA, NA,
    "Shot", NA, NA),
  ncol = 3,
  byrow = T
),
stringsAsFactors = F)
colnames(a) <- c("event_type", "x_coord_1", "y_coord_1")

pbp_data <- pbp_data %>%
  mutate(state = ifelse(
    event_team == home_team,
    paste(home_skaters, "v", away_skaters, sep = ''),
    paste(away_skaters, "v", home_skaters, sep = '')
  ))
states <- unique(pbp_data$state)
all_positions <- unique(player_data$Position)

standardize <- function(x) {
  x <- as.numeric(x)
  y <- rank(x) / length(x)
}

player_data <- rename(player_data, "GF%" = GF.)
team_data <-rename(team_data, "SF%" = SF.,"SF%_5v5" =  SF_5v5.)