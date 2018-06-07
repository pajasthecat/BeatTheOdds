install.packages('rvest')

library('rvest')

#Metod för att generera url beroende på månad
generate_url <- function(month){
  splitMonths <- unlist(strsplit(month, " "))
  baseUri <-'http://www.fifa.com/worldcup/preliminaries/matches/year='
  month_string <- '/month='
  uri_with_year <- paste(baseUri, splitMonths[2], sep = "")
  uri_with_month <- paste(uri_with_year, month_string, sep = "")
  uri_with_months_complete <- paste(uri_with_month, splitMonths[1], sep = "")
  uri_end <- '/index.htmx'
  url <- paste(uri_with_months_complete, uri_end, sep = "")
  return(url)
}

#Method for getting raw data for teams
get_raw_data_teams <- function(url)
{
  webpage <- read_html(url)
  teams_raw <- html_nodes(webpage, '.t-nText') 
  teams <- html_text(teams_raw)
  return(teams)
}

#Met
get_raw_data_scores <- function(url){
  webpage <- read_html(url)
  score_raw <- html_nodes(webpage, '.s-scoreText') 
  scores<- html_text(score_raw)
  return(scores)
}

#Make scores into data.frame
scores_to_data_frame<- function(scores){
  
  scores_data <- structure(data.frame( 
    matrix(unlist(strsplit(scores,"-")),length(scores),2,T)), 
    names=c("homeGoal","awayGoal")) 
  return(scores_data)
}


#Make scores into numerics
scores_values_to_numeric <- function(scores_data){
  
  scores_data$homeGoal <- as.numeric(scores_data$homeGoal)
  scores_data$awayGoal <- as.numeric(scores_data$awayGoal)
  return(scores_data)
}

generate_data <- function(url)
{
  #Reading the HTML code from the website
  teams <- get_raw_data_teams(url)
  scores <- get_raw_data_scores(url)
  
  #Seperate into home and away teams
  index <- length(teams)
  home <- teams[seq(1, index, 2)]
  
  away <- teams[seq(2, index, 2)]
  
  #Make teams to data.frame
  teams_data <- structure(data.frame( home, away))
  
  #Make scores into data.frame
  scores_data <- scores_to_data_frame(scores = scores)
  
  #Make scores into numeric
  scores_data <- scores_values_to_numeric(scores_data = scores_data)
  
  #Add ID to data.frames
  scores_data$Id <- seq.int(nrow(scores_data))
  teams_data$Id <- seq.int(nrow(teams_data))
  
  #Merge datasets
  merged_set <- merge(teams_data, scores_data, by = "Id")
  
  return(merged_set)
}


get_complete_data <- function(months, complete_data)
{
  for(month in months){
    url <- generate_url(month)
    generated_data <- generate_data(url)
    new_part <- generated_data
    complete_data <- rbind(complete_data, generated_data)
  }
  complete_data$Id <- seq.int(nrow(complete_data))
  
  return(complete_data)
}

months <- c('3 2015', '5 2015', '6 2015', '8 2015', 
            '9 2015', '10 2015', '11 2015', '3 2016', 
            '5 2016', '6 2016', '9 2016', '10 2016', 
            '11 2016', '3 2017', '6 2017', '8 2015', 
            '9 2015', '10 2017', '11 2015')


complete_data <- data.frame(home = character(),
                            away = character(),
                            homeGoals = numeric(),
                            awayGoals = numeric()) 

complete_data <- get_complete_data(months = months, complete_data = complete_data)


