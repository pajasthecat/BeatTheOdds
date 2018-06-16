
#install.packages('rvest')
#install.packages('httr')
#install.packages('jsonlite')

library('rvest')
library('httr')
library('jsonlite')
library(stringr)


complete_data_test$rankingHome <- ifelse(complete_data_test$home)


rankings 

team_names <- scrape("https://www.fifa.com/fifa-world-ranking/ranking-table/men/index.html", '.tbl-teamname')

rankings <- scrape("https://www.fifa.com/fifa-world-ranking/ranking-table/men/index.html", '.tbl-rank')

t <- structure(data.frame(team_names, rankings))
t <- t[-1,]
t


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

########TEAMS##########

#Metod för att scrapa
scrape <- function(url, nodes){
  webpage <- read_html(url)
  content_raw <- html_nodes(webpage, nodes) 
  content <- html_text(content_raw)
  return(content)
}


get_teams <- function(url){
  #Reading the HTML code from the website
  teams <- scrape(url, '.t-nText')

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

########Scores##########

#Make scores into data.frame
scores_to_data_frame<- function(scores){
  
  scores_data <- structure(data.frame( 
    matrix(unlist(strsplit(scores,"-")),length(scores),2,T)), 
    names=c("homeGoal","awayGoal")) 
  return(scores_data)
}


#Make scores into numerics
scores_values_to_numeric <- function(scores_data){
  
  scores_data$homeGoal <- as.numeric(as.character(scores_data$homeGoal))
  scores_data$awayGoal <- as.numeric(as.character(scores_data$awayGoal))
  
  return(scores_data)
}


get_scores <- function(url){
  #Reading the HTML code from the website
  scores <- scrape(url, '.s-scoreText')
  
  #Make scores into data.frame
  scores_data <- scores_to_data_frame(scores = scores)
  
  
  #Add ID to data.frames
  scores_data$Id <- seq.int(nrow(scores_data))
  
  #Make scores into numeric
  scores_data <- scores_values_to_numeric(scores_data = scores_data)
  return(scores_data)
}


########WEATHER##########

convert_dates_to_readable <- function(dates, month)
{
  dates_clean <- sapply(strsplit(dates,"-"), `[`, 1)
  
  day <- sapply(strsplit(dates_clean, " "), `[`, 1)
  year <- sapply(strsplit(dates_clean, " "), `[`, 3)
  
  date_input <- paste(year, '-', month, "-", day, sep = "")
  return(date_input)
}


#Metod för att hämta tempratur
get_weather_from_api <- function(dates_clean, citys){
  temp_vector <- c()
  test <- length(citys)
  for(i in 1:test){
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
  under10 <- as.numeric(10 >=temprature)
  over10under20 <- as.numeric(temprature >10 & 20 >= temprature)
  over20under30 <- as.numeric(temprature>20 & 30 >= temprature)
  over30 <- as.numeric(temprature > 30)
  
  temprature_data <- structure(data.frame( cbind(under10), cbind(over10under20, cbind(over20under30), cbind(over30))))
  
  return(temprature_data)
}

get_weather <- function(url, month){
  dates <- scrape(url, '.mu-i-datetime')
  clean_dates <- convert_dates_to_readable(dates, month)
  city <- scrape(url, '.mu-i-venue')
  temprature <- get_weather_from_api(dates_clean = clean_dates, citys = city)
  temprature_data <- get_temprature_dummies(temprature = temprature)
  temprature_data$Id <- seq.int(nrow(temprature_data))
  return(temprature_data)
}

get_month_number <- function(month){
  month_list <- strsplit(month, " ")
  month_number <- unlist(month_list)[1]
  return(month_number)
}

generate_data <- function(url, month)
{

  teams_data <- get_teams(url)
  
  scores_data <- get_scores(url)
  
  month_clean <- get_month_number(month)
  
  temprature_data <- get_weather(url, month_clean)
  #Merge datasets
  merged_set<- Reduce(function(x, y) merge(x, y, all=TRUE), list(teams_data, scores_data, temprature_data))
  #merged_set <- merge(teams_data, scores_data, by = "Id")
  
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
                            over20under30 = numeric(),
                            over30 = numeric()) 

complete_data <- get_complete_data(months = months, complete_data = complete_data)


write.csv(complete_data, file = 'complete_data2.csv', row.names = FALSE)



