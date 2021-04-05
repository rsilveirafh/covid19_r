## Data ----
## https://github.com/wcota/covid19br - https://wcota.me/covid19br

library(tidyverse)

# raw <- read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv", h = T)

# write.csv(raw, "raw.csv")

raw <- read.csv("raw.csv", h = T)

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

states.list <- data.frame(acr = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", 
								  "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", 
								  "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
						  st = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", 
						  	   "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", 
						  	   "Minas Gerais", "Mato Grosso do Sul", "Mato Grosso", "Pará", 
						  	   "Paraíba", "Pernambuco", "Piauí", "Paraná", "Rio de Janeiro", 
						  	   "Rio Grande do Norte", "Rondônia", "Roraima", "Rio Grande do Sul", 
						  	   "Santa Catarina", "Sergipe", "São Paulo", "Tocantins"))

st_choice <- as.list(states.list$acr)
names(st_choice) <- states.list$st

o <- as.vector(st_choice[[1]])

filter(states, state == o)

states <- covid %>% 
	group_by(date, state) %>% 
	summarise(newC = sum(newCases),
			  totalC = sum(totalCases),
			  totalD = sum(deaths))

cities <- covid %>% 
	group_by(date, city, ibgeID) %>% 
	summarise(newC = sum(newCases),
			  totalC = sum(totalCases),
			  totalD = sum(deaths))

ibge <- read.csv("data/raw/gps_cities.csv", h = T)	%>% 
	select(ibgeID, lat, lon)

cities <- inner_join(cities, ibge, by = "ibgeID")

# write.csv(brazil, "data/processed/brazil.csv")
# write.csv(states, "data/processed/states.csv")
# write.csv(cities, "data/processed/cities.csv")

rm(covid, ibge, raw)
