library(tidyverse)
library(rvest)
library(ggimage)
library(lubridate)

#get first 25 leagues in Europe ----
url <- "https://www.transfermarkt.de/wettbewerbe/europa"
doc <- read_html(url)

leagues <- doc %>% html_nodes(".hauptlink a") %>% html_attr("href")
leagues <- leagues[seq(2,length(leagues),2)]

year_league_url <- function(league_url, year) 
  paste0(league_url, "/plus/?saison_id=", year)

#function to scrape 
get_data <- function(team_url, year){
  
  base.url <- "https://www.transfermarkt.de"
  
  doc <- read_html(paste0(base.url,
                          year_league_url(team_url, year)))

  teams <- doc %>% 
    html_nodes(".items") %>% 
    html_table(fill=TRUE) %>% 
    .[[1]] %>% 
    .[-1,]
  
  teams <- janitor::clean_names(teams)
  
  #check if market value is in millions or hundred thousands
  teams$val <- teams$gesamtmarktwert %>% str_extract("[a-zA-Z]+")
  
  #league name
  league <- doc %>% html_nodes(".spielername-profil") %>% html_text()
  
  #league country
  country <- doc %>% 
    html_table(fill=TRUE) %>% 
    .[[1]] %>% 
    .$X2 %>% 
    .[1] %>% 
    word(2,sep="-") %>% 
    str_trim()
  
  #get team name, mean age and mean value
  teams <- teams %>% 
    select(name,kader,gesamtmarktwert,val) %>% 
    mutate(age=as.numeric(str_replace(kader,",","."))) %>% 
    mutate(mw=str_replace(gesamtmarktwert,"[a-zA-Z]+\\. €","")) %>% 
    mutate(mw=as.numeric(str_replace(mw,",","."))) %>%
    mutate(mw=ifelse(val=="Mio", mw, mw/1000)) %>% 
    select(name,age,mw)
  
  teams$league <- league
  teams$year <- year
  
  return(teams)
}

grid_leagues_year <- expand.grid(league_url = leagues[1:5], year = 2009:2019)
res <- vector("list", length = nrow(grid_leagues_year))
for(i in 1:nrow(grid_leagues_year)){
  print(i)
  res[[i]] <- get_data(grid_leagues_year$league_url[i],
                       grid_leagues_year$year[i])
}

salary_data <- bind_rows(res)
