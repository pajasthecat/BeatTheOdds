
#install.packages('rvest')
#install.packages('httr')
#install.packages('jsonlite')

library('rvest')
library('httr')
library('jsonlite')
library(stringr)


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


get_teams <- function(url){
  #Reading the HTML code from the website
  teams <- get_raw_data_teams(url)
  
  #Seperate into home and away teams
  index <- length(teams)
  home <- teams[seq(1, index, 2)]
  away <- teams[seq(2, index, 2)]
  
  #Make teams to data.frame
  teams_data <- structure(data.frame( home, away))
  
  #Add ID to data.frames
  teams_data$Id <- seq.int(nrow(teams_data))
  return(teams_data)
}


#Get raw data for teams
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


get_scores <- function(url){
  #Reading the HTML code from the website
  scores <- get_raw_data_scores(url)
  
  #Make scores into data.frame
  scores_data <- scores_to_data_frame(scores = scores)
  
  #Make scores into numeric
  scores_data <- scores_values_to_numeric(scores_data = scores_data)
  
  #Add ID to data.frames
  scores_data$Id <- seq.int(nrow(scores_data))
  return(scores_data)
}

get_raw_date <- function(url){
  webpage <- read_html(url)
  dates_raw <- html_nodes(webpage, '.mu-i-datetime')
  dates <- html_text(dates_raw)
  return(dates)
}

convert_dates_to_readable <- function(dates, month)
{
  dates_clean <- sapply(strsplit(dates,"-"), `[`, 1)
  
  day <- sapply(strsplit(dates_clean, " "), `[`, 1)
  year <- sapply(strsplit(dates_clean, " "), `[`, 3)
  
  date_input <- paste(year, '-', month, "-", day, sep = "")
}

get_raw_city <- function(url){
  webpage <- read_html(url)
  city_raw <- html_nodes(webpage, '.mu-i-venue')
  city <-  html_text(city_raw)
  return(city)
}

#Metod för att hämta tempratur
get_weather_from_api <- function(dates_clean, citys){
  temp_vector <- c()
  
  for(i in 0:length(citys)){
    basePath <- 'https://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=b9894c56b5914eb2876112655180906&'
    date <- dates_clean[i]
    city <- citys[i]
    url <- paste(basePath, 'q=', str_trim(city), sep = "")
    url <- paste(url,'&format=json&date=', sep = "" )
    url <- paste(url, date, sep = "")
    raw_response <- GET(URLencode(url))
    response <- content(raw_response, encoding = "application/x-www-form-urlencoded")
    temprature <- as.numeric(response$data$weather[[1]]$maxtempC)
    temp_vector <- append(temp_vector, temprature)
  }
  
  return(temp_vector)
}

#Turn temprature into dummies
get_temprature_dummies <- function(temprature){
  under10 <- as.numeric(temprature <= 10)
  over10under20 <- as.numeric(temprature >10 & temprature <=20)
  over20under25 <- as.numeric(temprature>20 & temprature<=30)
  over30 <- as.numeric(temprature > 30)
  
  temprature_data <- structure(data.frame( cbind(under10), cbind(over10under20, cbind(over20under25), cbind(over30))))
  
  return(temprature_data)
}

get_weather <- function(url, month){
  dates <- get_raw_date(url)
  clean_dates <- convert_dates_to_readable(url, month)
  city <- get_raw_city(url)
  temprature <- get_weather_from_api(dates_clean = clean_dates, citys = city)
  temprature_data <- get_temprature_dummies(temprature = temprature)
  temprature_data$Id <- seq.int(nrow(temprature_data))
  return(temprature_data)
}

generate_data <- function(url, month)
{

  teams_data <- get_teams(url)
  scores_data <- get_scores(url)
  temprature_data <- get_weather(url, month)
  #Merge datasets
  merged_set<- Reduce(function(x, y) merge(x, y, all=TRUE), list(teams_data, scores_data, temprature_data))
  #merged_set <- merge(teams_data, scores_data, temprature_data, by = "Id")
  
  return(merged_set)
}


get_complete_data <- function(months, complete_data)
{
  for(month in months){
    url <- generate_url(month)
    generated_data <- generate_data(url, month)
    new_part <- generated_data
    complete_data <- rbind(complete_data, generated_data)
  }
  complete_data$Id <- seq.int(nrow(complete_data))
  
  return(complete_data)
}

"months <- c('3 2015', '5 2015', '6 2015', '8 2015', 
            '9 2015', '10 2015', '11 2015', '3 2016', 
            '5 2016', '6 2016', '9 2016', '10 2016', 
            '11 2016', '3 2017', '6 2017', '8 2017', 
            '9 2017', '10 2017', '11 2017')"

months <- c('3 2015')


complete_data <- data.frame(home = character(),
                            away = character(),
                            homeGoals = numeric(),
                            awayGoals = numeric(),
                            under10 = numeric(),
                            over10under20 = numeric(),
                            over20under25 = numeric(),
                            over30 = numeric()) 

complete_data <- get_complete_data(months = months, complete_data = complete_data)


write.csv(complete_data, file = 'complete_data.csv', row.names = FALSE)



