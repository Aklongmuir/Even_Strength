library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(pROC)
library(googlesheets)
xgpbp <-  read_csv("data/nwhl_pbp_all.csv")


Train_PbP_Data <- xgpbp
#whats a fenwick

fenwick_events <- c('Shot', 'Goal')

#time differential
Train_PbP_Data <- Train_PbP_Data %>% group_by(game_id) %>%
  arrange(play_index, .by_group = TRUE) %>%
  mutate(time_diff = game_seconds - lag(game_seconds))
Train_PbP_Data$time_diff[is.na(Train_PbP_Data$time_diff)] <- 0

#diditgoping
Train_PbP_Data$is_rebound <- ifelse(Train_PbP_Data$time_diff < 3 & 
                                      Train_PbP_Data$event_type %in% fenwick_events &
                                      Train_PbP_Data$event_team == 
                                      lag(Train_PbP_Data$event_team),
                                    1, 0)

Train_PbP_Data$is_rebound[is.na(Train_PbP_Data$is_rebound)] <- 0

#RUSH
#Train_PbP_Data$is_rush <- ifelse(Train_PbP_Data$time_diff < 4 &
#                lag(abs(Train_PbP_Data$event_distance)) < 25 &
#                Train_PbP_Data$event_type %in% fenwick_events,
#            1, 0)
Train_PbP_Data$is_rebound[is.na(Train_PbP_Data$time_diff)] <- 0
#Train_PbP_Data$is_rush[is.na(Train_PbP_Data$is_rush)] <- 0

#faceoffs are for loosers
Train_Fenwick_Data<- filter(Train_PbP_Data, event_type %in% c("Shot", "Goal"))



#toronto ask themselves this every night and they still dont know
is_goal <- function(dataframe){
  dataframe$is_goal <- ifelse(dataframe$event_type == "Goal", 1, 0)
  return(dataframe)
}

#i think i only need to do one of these but at this point i dont know which one and im too afraid to ask
Train_PbP_Data <- is_goal(Train_PbP_Data)
Train_Fenwick_Data <- is_goal(Train_Fenwick_Data)


#Get rid of the NA
Train_Fenwick_Data <- filter(Train_Fenwick_Data, !is.na(event_distance))
#head(Train_Fenwick_Data[Train_Fenwick_Data$event_distance == 'NA',])

Train_Fenwick_Data <- filter(Train_Fenwick_Data, !is.na(event_angle))
#head(Train_Fenwick_Data[Train_Fenwick_Data$event_angle == 'NA',])

Train_Fenwick_Data <- filter(Train_Fenwick_Data, !is.na(event_type))
#head(Train_Fenwick_Data[Train_Fenwick_Data$event_type == 'NA',])

Train_Fenwick_Data <- filter(Train_Fenwick_Data, !is.na(event_player_1))
#head(Train_Fenwick_Data[Train_Fenwick_Data$event_player_1 == 'NA',])

Train_Fenwick_Data <- filter(Train_Fenwick_Data, !is.na(is_rebound))
#head(Train_Fenwick_Data[Train_Fenwick_Data$is_rebound == 'NA',])

Train_Fenwick_Data <- filter(Train_Fenwick_Data, !is.na(is_rush))
#head(Train_Fenwick_Data[Train_Fenwick_Data$is_rush == 'NA',])

Train_Fenwick_Data <- filter(Train_Fenwick_Data, !is.na(is_goal))
#head(Train_Fenwick_Data[Train_Fenwick_Data$is_goal == 'NA',])


#CHOOCHOOTRAINTHEMODEL
xGmodel <-glm (is_goal ~ poly(event_distance, 3, raw = TRUE) +
                 poly(event_angle, 3, raw = TRUE) + is_rebound +
               data = Train_Fenwick_Data,
               family = binomial(link = 'logit')

save(xGmodel, file = "xGmodelver2.rda")


testdata <- read_csv("data/nwhl_pbp_1819.csv")
xGtestData<- filter(testdata, event_type %in% c("Shot", "Goal"))

#doing all of the hocus pocus to the test data

#time differential
xGtestData <- xGtestData %>% group_by(game_id) %>%
  arrange(play_index, .by_group = TRUE) %>%
  mutate(time_diff = game_seconds - lag(game_seconds))
xGtestData$time_diff[is.na(xGtestData$time_diff)] <- 0

#diditgoping
xGtestData$is_rebound <- ifelse(xGtestData$time_diff < 3 & 
                                  xGtestData$event_type %in% fenwick_events &
                                  xGtestData$event_team == 
                                  lag(xGtestData$event_team),
                                1, 0)

xGtestData$is_rebound[is.na(xGtestData$is_rebound)] <- 0

#rushrushbaby

xGtestData$is_rush <- ifelse(xGtestData$time_diff < 4 &
                               lag(abs(xGtestData$event_distance)) < 25 &
                               xGtestData$event_type %in% fenwick_events,
                             1, 0)
#isitagoal
xGtestData <- is_goal(xGtestData)

#Get rid of the NAs

xGtestData <- filter(xGtestData, !is.na(event_distance))
#head(xGtestData[xGtestData$event_distance == 'NA',])

xGtestData <- filter(xGtestData, !is.na(event_angle))
#head(xGtestData[xGtestData$event_angle == 'NA',])

xGtestData <- filter(xGtestData, !is.na(event_type))
#head(xGtestData[xGtestData$event_type == 'NA',])

xGtestData <- filter(xGtestData, !is.na(event_player_1))
#head(xGtestData[xGtestData$event_player_1 == 'NA',])

xGtestData <- filter(xGtestData, !is.na(is_rebound))
#head(xGtestData[xGtestData$is_rebound == 'NA',])

xGtestData <- filter(xGtestData, !is.na(is_rush))
#head(xGtestData[xGtestData$is_rush == 'NA',])

xGtestData <- filter(xGtestData, !is.na(is_goal))
#head(xGtestData[xGtestData$is_goal == 'NA',])


#RUNTHISBABY

xGtestData$xG <- predict(xGmodel, xGtestData, type = "response")

#WHERE ARE THE GOOD PLACES TO SHOOT FROM?? TELL ME. 
avg_xG_by_coord <- xGtestData %>% group_by(x_coord_1, y_coord_1) %>%
  summarise(xg = mean(xG))


#Pretty curve which keeps telling me im mediocre, like bitch i know
g <- roc(is_goal ~ xG, data = xGtestData)
plot(g)

#xGoalsby player
xg_player <- xGtestData %>%
  group_by(event_player_1, event_team) %>%
  summarise( xG = sum(xG), Goals = sum(is_goal), Difference = sum(xG) - sum(is_goal))
head(xg_player)
arrange(xg_player, desc(xG))

#xGoals by player graph
ggplot(aes(x = xG, y = Goals), data = xg_player) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  labs(title = 'Expected Goals vs Goals by Player')

#goalsbyteam
xg_team <- xGtestData %>%
  group_by(event_team) %>%
  summarise( xG = sum(xG), Goals = sum(is_goal), Difference = sum(xG) - sum(is_goal))

arrange(xg_team, desc(xG))

#goalsbygame
xg_game_team <- xGtestData %>%
  group_by(game_date, event_team) %>%
  summarise(xG = sum(xG), Goals = sum(is_goal), Difference = sum(xG) - sum(is_goal))
arrange(xg_game_team, desc(xG))

#goalsbyplayerbygame

xg_game_player <- xGtestData %>%
  group_by(game_date, event_player_1, event_team) %>%
  summarise(xG = sum(xG), Goals = sum(is_goal), Difference = sum(xG) - sum(is_goal))
arrange(xg_game_player, desc(xG))

#test
