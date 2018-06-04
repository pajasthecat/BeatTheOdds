install.packages("devtools")
devtools::install_github("dashee87/footballR")
devtools::install_github("phillc73/abettor")
install.packages("dplyr")
install.packages("skellam")
install.packages("ggplot2")
install.packages("purrr")
install.packages("tidyr")
install.packages("RCurl")

library(footballR)
library(dplyr)
library(skellam)
library(ggplot2)
library(purrr)
library(tidyr)
library(abettor)
library(RCurl)



football_data_id <- fdo_listComps(season = 2017, response = "minified") %>% filter(league == "SA") %>% .$id

football_data <- fdo_listCompFixtures(id = football_data_id, response = "minified")$fixtures %>%
  jsonlite::flatten() %>% filter(status == "FINISHED") %>% 
  rename(home = homeTeamName, away = awayTeamName, homeGoals = result.goalsHomeTeam, 
           awayGoals = result.goalsAwayTeam) %>% select(home, away, homeGoals, awayGoals)

head(football_data)

#Predict result of final round
football_data_minus_ten <- head(football_data, -10)

#Probabilty draw between home and away. First parameter : Amount of goals 
dskellam(0, mean(football_data_minus_ten$homeGoals), mean(football_data_minus_ten$awayGoals))

#Prob home team winning by one 
dskellam(1, mean(football_data_minus_ten$homeGoals), mean(football_data_minus_ten$awayGoals))

#Prob away team winning by one (i think)
dskellam(-1, mean(football_data_minus_ten$homeGoals), mean(football_data_minus_ten$awayGoals))

#Poisson-model
poisson_model <- rbind(
  data.frame(goals = football_data_minus_ten$homeGoals,
             team = football_data_minus_ten$home,
             opponent = football_data_minus_ten$away,
             home = 1),
  data.frame(goals = football_data_minus_ten$awayGoals,
             team = football_data_minus_ten$away,
             opponent = football_data_minus_ten$home,
             home = 0)) %>%
  glm( goals ~ home + team +opponent, family = poisson(link = log),
       data =.)

summary(poisson_model)

#Predict using model. Must do twice, one for each team

#First Napoli
predict(poisson_model,
        data.frame(home = 1,
                   team = "SSC Napoli",
                   opponent = "FC Crotone"), type = "response")

#Second Crotone
predict(poisson_model,
        data.frame(home = 0,
                   team = "FC Crotone",
                   opponent = "SSC Napoli"), type = "response")

#Simulation of matches
sim_match <- function(model, homeTeam, awayTeam, max_goals = 10)
{
  home_goals_avg <- predict(poisson_model,
                            data.frame(home = 1,
                                       team = homeTeam,
                                       opponent = awayTeam), type = "response")
  away_goals_avg <- predict(poisson_model,
                            data.frame(home = 0,
                                       team = awayTeam,
                                       opponent = homeTeam), type = "response")
  
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
  
}

#Shows matrix over different results. Diagonal is tied games
nap_cro <- sim_match(poisson_model, "SSC Napoli", "FC Crotone", max_goals = 10)

#Probability Napoli wins
sum(nap_cro[lower.tri(nap_cro)])

#Prob draw
sum(diag(nap_cro))

#Prob Crotone wins
sum(nap_cro[upper.tri(nap_cro)])

