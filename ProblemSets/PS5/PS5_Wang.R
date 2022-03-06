
library(rvest) ## Already loaded
library(tidyverse)
library(jsonlite)
library(fredr)
library(sqldf)


BYU = read_html("https://www.byuaccounting.net/rankings/univrank/rank_university.php?qurank=Finan_Archi&sortorder=ranking6") %>%
  html_nodes("#content > form") %>%
  '[['(1)%>% # get the first item of a list
  html_table(fill=TRUE)

Tesla_Concept = 
  fromJSON("https://data.sec.gov/api/xbrl/companyconcept/CIK0001318605/us-gaap/AccountsPayableCurrent.json") %>%
  '[['(7)%>%
  '[['(1)%>%
  as_tibble()
Tesla_Concept # why there is only 1 variable and 1 colume to r. It appears to have 8 columes.

temp<-Tesla_Concept[Tesla_Concept$frame !=NA, ]

Tesla_Facts = 
  fromJSON("https://data.sec.gov/api/xbrl/companyfacts/CIK0001318605.json") %>%
  '[['(3) %>%
  '[['(2) %>%
  '[['(1)%>%
  '[['(3)%>%
  as_tibble()
Tesla_Facts # How to create table efficiently to hold all the data?



Frame = 
  fromJSON("https://data.sec.gov/api/xbrl/frames/us-gaap/AccountsPayableCurrent/USD/CY2019Q1I.json") %>%
  as_tibble()
Frame

save.image("C:/Jwang/00-OKU/Coursework/2022H1-ECON 5253/DScourseS22-MyFork/test.RData")