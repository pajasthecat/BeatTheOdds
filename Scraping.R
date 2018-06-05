install.packages('rvest')

library('rvest')

generate_url <- function(month){
  u <-'http://www.fifa.com/worldcup/preliminaries/matches/year=2015/month='
  r <- '/index.htmx'
  l <- paste(u, month, sep = "")
  url <- paste(l, r, sep = "")
  return(url)
}


#url <- 'http://www.fifa.com/worldcup/preliminaries/matches/year=2015/month=5/index.htmx'

generate_data <- function(url)
{
  #Reading the HTML code from the website
  webpage <- read_html(url)
  
  teams_raw <- html_nodes(webpage, '.t-nText') 
  
  score_raw <- html_nodes(webpage, '.s-scoreText') 
  
  #Converting the ranking data to text
  teams <- html_text(teams_raw)
  scores<- html_text(score_raw)
  
  #Seperate into home and away teams
  index <- length(teams)
  home <- teams[seq(1, index, 2)]
  
  away <- teams[seq(2, index, 2)]
  
  #Make teams to data.frame
  teams_data <- structure(data.frame( home, away))
  
  #Make scores into data.frame
  scores_data <- structure(data.frame( 
    matrix(unlist(strsplit(scores,"-")),length(scores),2,T)), 
    names=c("homeGoal","awayGoal")) 
  
  #Make scores into numeric
  scores_data$homeGoal <- as.numeric(scores_data$homeGoal)
  
  scores_data$awayGoal <- as.numeric(scores_data$awayGoal)
  
  #Add ID to data.frames
  scores_data$Id <- seq.int(nrow(scores_data))
  teams_data$Id <- seq.int(nrow(teams_data))
  
  #Merge datasets
  new_part <- merge(teams_data, scores_data, by = "Id")
  
  return(new_part)
  old_part <-new_part
  
  complete_data <- rbind(old_part, new_part)
  
  complete_data$Id <- seq.int(nrow(complete_data))
}

months <- c('3', '5', '6', '8', '9', '10', '11')

for(month in months){
  url <- generate_url(month)
  generated_data <- generate_data(url)
  if(month != "3")
  {
    new_part <- generated_data
    complete_data <- rbind(old_part, new_part)
    complete_data$Id <- seq.int(nrow(complete_data))
  }
  if(month == 3)
  {
    old_part <- generated_data
  }
}
