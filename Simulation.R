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

#Creates Model
create_model <- function(complete_data)
{
  poisson_model <- rbind(
    data.frame(goals = complete_data$homeGoals,
               team = complete_data$home,
               opponent = complete_data$away,
               home = 1),
    data.frame(goals = complete_data$awayGoals,
               team = complete_data$away,
               opponent = complete_data$home,
               home = 0)) %>%
    glm( goals ~ home + team +opponent, family = poisson(link = log),
         data =.)
  
  summary(poisson_model)
  
  return(poisson_model)
}


#Do simulation
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

create_result <- function(poisson_model)
{
  #Shows matrix over different results. Diagonal is tied games
  create_matrix <- sim_match(poisson_model, "SSC Napoli", "FC Crotone", max_goals = 10)
  
  #Probability Napoli wins
  home <- sum(nap_cro[lower.tri(nap_cro)])
  
  #Prob draw
  draw <- sum(diag(nap_cro))
  
  #Prob Crotone wins
  away <- sum(nap_cro[upper.tri(nap_cro)])
  
  return(c(home, draw, away))
}


run_sim <- function(complete_dataset)
{
  model <- create_model(complete_dataset)
  result <- create_result(poisson_model)
  return(result)
}

