#Load the dataset
filename <- file.choose()
AllDt <- readRDS(filename)

#Let's take a look at our dataset
head(AllDt)

#Check for missing value
sum(is.na(AllDt))

str(AllDt)

# GET TO KNOW YOUR DATA BETTER
# LET's ACCESS SOME IMPORTANT INFORMATION


#-------------------------------------------
# Which team is the winner?
#--------------------------------------------
match_outcome <- function(Team.Goals, Opponent.Goals) {
  m_outcome <- "Draw"
  if (Team.Goals > Opponent.Goals) {m_outcome <- "Home"}
  if (Team.Goals < Opponent.Goals) {m_outcome <- "Away"}
  return(m_outcome)
}

winning_team <- function(Team.Goals, Opponent.Goals, Team, Opponent) {
  winning_team <- NA
  if (Team.Goals > Opponent.Goals) {winning_team <- Team}
  if (Team.Goals < Opponent.Goals) {winning_team <- Opponent}
  return(winning_team)
}

losing_team <- function(Team.Goals, Opponent.Goals, Team, Opponent) {
  losing_team <- NA
  if (Team.Goals < Opponent.Goals) {losing_team <- Team}
  if (Team.Goals > Opponent.Goals) {losing_team <- Opponent}
  return(losing_team)
}

AllDt <- AllDt %>%
  mutate(year = format(Date, "%Y"),
         month = format(Date, "%b"),
         dayofweek = weekdays(Date)) %>%
  rowwise() %>%
  mutate(m_outcome = match_outcome(Team.Goals, Opponent.Goals),
         winning_team = winning_team(Team.Goals, Opponent.Goals, Team, Opponent),
         losing_team = losing_team(Team.Goals, Opponent.Goals, Team, Opponent)) %>%
  ungroup()

head(AllDt)

#-------------------------------------------
# Which team play the most?
#--------------------------------------------
all_teams <- data.frame(teams = c(AllDt$Team, AllDt$Opponent), year=as.numeric(c(AllDt$year, AllDt$year)))
all_teams_count <- all_teams %>%
  group_by(teams) %>%
  summarise(number_matches = length(teams)) %>%
  arrange(desc(number_matches))
head(all_teams_count, 15)

# Looklike Arsenal team play the most matches

#-------------------------------------------
# How many matches per month?
#--------------------------------------------
matches_per_month <- AllDt %>%
  mutate(month<-as.integer(factor(month, levels = month.name))) %>%
  group_by(month) %>%
  summarise(nb_games = length(Date))  %>%
  ungroup()

matches_per_month
# Seems like in 2020, most matches happen in July

#-------------------------------------------
# When do matches occur?
#-------------------------------------------
AllDt_games_per_dayofweek <- AllDt %>%
  group_by(dayofweek) %>%
  summarise(n = length(Date)) %>%
  mutate(perc = n / sum(n) * 100) %>%
  mutate(dayofweek = factor(dayofweek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

AllDt_games_per_dayofweek
# 32 percent of matches occur in Sunday 

#-------------------------------------------
# Which team has the best win ratio?
#-------------------------------------------
AllDt_teams_goals_overall <- AllDt %>%
  group_by(Team) %>%
  summarise(gf_per_game = sum(Team.Goals) / length(Date),
            total_games = length(Date)
  )

top_attack <- head(AllDt_teams_goals_overall %>% arrange(desc(gf_per_game)))
top_attack
#Seems like MU is the top attack in 2020 matches

#-------------------------------------------
# Which are team teams relying the most on crosses??
#-------------------------------------------
AllDt_teams_crosses_overall <- AllDt %>%
  group_by(Team) %>%
  summarise(crosses_per_match = sum(Crosses) / length(Date),
  )

top_cross_team <- head(AllDt_teams_crosses_overall %>% arrange(desc(crosses_per_match)))
top_cross_team
#Leeds produce 1.22 crosses per matches in 2020

#-------------------------------------------
#ENOUGH FOR DATA EXPLORATION
#-------------------------------------------

#-------------------------------------------
# Let's group player position 'PlyPosition' into three categories
# Group all positions into three categories
#-------------------------------------------
library(tidyverse)
library(dplyr)

AllDt$position_cat <- fct_collapse(
  AllDt$PlyPosition,
  Def = c("DR", "DC", "DL", "GK"),
  Mid = c( "AMC", "AML", "AMR", "DMC", "DML", "DMR", "MC", "ML", "MR"),
  Att = c("FWR", "FWL", "FW"),
  Sub = c("Sub")
)

AllDt$position_cat<-
  factor(AllDt$position_cat, levels = c("Def", "Mid", "Att", "Sub"))

#-------------------------------------------
# Number of players by position after grouping in the dataset
#-------------------------------------------
library(kableExtra) #simple table generator

AllDt%>% group_by(position_cat) %>% summarise(count = n()) %>%
  kable(caption = "Number of Players by Position") %>%
  kable_styling(
    latex_options = c("striped", "bordered", "hover", "responsive", "repeat_header"),
    full_width = FALSE,
    position = "center",
    font_size = 10
  )

#-------------------------------------------
# Let's group Crosses habit into categories, I'm gonna use Mean as split point here
# Group all positions into three categories
#-------------------------------------------
AllDt <- AllDt %>% 
  mutate(cross_hab = case_when(Crosses < 10 ~ 1,
                               10 <= Crosses & Crosses < 14 ~ 2,
                               14 <= Crosses ~ 3))

# I intend to generate new variable of 'Player Value' but seems like I need some supportive data
# I'm gonna leave it here

#-------------------------------------------
# Selecting Feature
#-------------------------------------------
library(dummies)
AllDt_new <- dummy.data.frame(select(AllDt, 'Date', 'Crosses', 'WinProb', 'position_cat', 'cross_hab', 'Team.Possession'), sep='_')

#-------------------------------------------
#Correlation Matrix
#-------------------------------------------
library(caret)
library(mlbench)
# ensure the results are repeatable
set.seed(42)
AllDt_new$position_cat<- as.numeric(AllDt_new$position_cat)
correlationMatrix <- cor(AllDt_new[2:6])
# summarize the correlation matrix
print(correlationMatrix)

#-------------------------------------------
# Split the dataset
#-------------------------------------------
TrainDate <- AllDt_new$Date < "2020-12-01"
TrainDt <- AllDt_new[TrainDate,]
TestDt <- AllDt_new[!TrainDate,]

#-------------------------------------------
# Dropping Date from both Train and Test set
#-------------------------------------------
TrainDt<- TrainDt[,2:6]
TestDt<- TestDt[,2:6]

#-------------------------------------------
# Hyperparameter Tuning, I'm gonna skip this step since it'll take so long 
#-------------------------------------------
#library(superml)
#nrounds = 1000
#tune_grid <- expand.grid(
#nrounds = seq(from = 200, to = nrounds, by = 50),
#eta = c(0.025, 0.05, 0.1, 0.3),
#max_depth = c(2, 3, 4, 5, 6),
#gamma = 0,
#colsample_bytree = 1,
#min_child_weight = c(1, 2, 3),,
#subsample = 1
#)
#set.seed(42)
#modelXgb <- train(Crosses~., data=TrainDt, 
#method="xgbTree", 
#trControl=control, 
#tuneGrid = tune_grid,
#verbose=FALSE)

#-------------------------------------------
# I'm gonna use XGoost and LightGBM as ensemble learning that known as the-state-of-art
# 1. XGBoost
#-------------------------------------------
library(xgboost)
xgb_train = xgb.DMatrix(data = data.matrix(TrainDt[,-1]), label = TrainDt[,1])
xgbc = xgboost(data = xgb_train, max.depth = 5, nrounds = 100, booster='gbtree', modelEval='mse')

#-------------------------------------------
#Predicts the test_set using XGBoost
#-------------------------------------------
xgb_test = xgb.DMatrix(data = data.matrix(TestDt[,-1]), label = TestDt[,1])
pred_y_xgb <- round(predict(xgbc, xgb_test))

#-------------------------------------------
#Measure MSE, MAE & RMSE for XGBoost
#-------------------------------------------
mse <- mean((TestDt$Crosses - pred_y_xgb)^2)
mae<- mean(abs(TestDt$Crosses - pred_y_xgb))
rmse <-sqrt(mse)
mse
mae
rmse

#-------------------------------------------
# 2. LightGBM
#-------------------------------------------
library(lightgbm)
lgbm_train = lgb.Dataset(data = as.matrix(TrainDt), label = TrainDt[,1])
lgbm = lgb.train(data = lgbm_train, max_depth = 5, nrounds = 100, boosting ='gbdt', eval ='mse', obj='regression')
