## Packages ----
library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)

## Data ----
## https://github.com/wcota/covid19br - https://wcota.me/covid19br
	
raw <- read.csv("https://github.com/wcota/covid19br/blob/master/cases-brazil-cities-time.csv.gz")
# write.csv(raw, "raw.csv")
# raw <- read.csv("raw.csv", h = T) # for tests

covid <- raw %>% 
	mutate(date = as.Date(date, format = "%Y-%m-%d"),
		   city = str_replace(city, "\\/[A-Z]+", "")) %>% 
	filter(state != "TOTAL") %>% 
	droplevels()

brazil <- covid %>% 
	group_by(date) %>% 
	summarize(newC = sum(newCases),
			  totalC = sum(totalCases),
			  totalD = sum(deaths))

states <- covid %>% 
	group_by(date, state) %>% 
	summarise(newC = sum(newCases),
			  totalC = sum(totalCases),
			  totalD = sum(deaths))

cities <- covid %>% 
	group_by(date, state, city, ibgeID) %>% 
	summarise(newC = sum(newCases),
			  totalC = sum(totalCases),
			  totalD = sum(deaths))

ibge <- read_csv("https://github.com/wcota/covid19br/blob/master/gps_cities.csv")	%>% 
	select(ibgeID, lat, lon)

# write.csv(ibge, "ibge.csv")
# ibge <- read.csv("ibge.csv", h = T) %>% #for tests
# 	select(ibgeID, lat, lon)

cities <- inner_join(cities, ibge, by = "ibgeID")

# write.csv(brazil, "data/processed/brazil.csv")
# write.csv(states, "data/processed/states.csv")
# write.csv(cities, "data/processed/cities.csv")

rm(covid, ibge, raw)
