## Packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

## Data ----
source("data_acquisition.R")

## UI ----
fluidPage(
	theme = shinytheme("superhero"),
	headerPanel("Brasil -- COVID-19"),
	navlistPanel(widths = c(2, 5),
				 tabPanel("Brasil",
				 	h3("Dados do COVID-19 para todo o país"),
				 	h5("Fonte dos dados: https://github.com/wcota/covid19br"),
				 	sidebarLayout(
				 		sidebarPanel(
				 			sliderInput(inputID = "data_1", 
				 						label = "Selecione as Datas:",
				 						min = min(brazil$date),
				 						max = max(brazil$date)),
				 			checkboxInput(inputID = "log",
				 						  label = "Escala Logarítmica")
				 		),
				 		mainPanel(
				 			h3("Brasil"),
				 			plotOutput("brasilPlot"),
				 			h6("Criado por Ricardo da Silveira Filho"),
				 			h6("Qualquer sugestão, entre em contato por ricardodasilveira at gmail.com"),
				 			uiOutput("git"),
				 			uiOutput("database")
				 		)
				 	)
				 ),
				 tabPanel("Estados",
				 	h3("Dados do COVID-19 para os estados do Brasil"),
				 	h5("Fonte dos dados: https://github.com/wcota/covid19br"),
				 	sidebarLayout(
				 		sidebarPanel(
				 			checkboxGroupInput("estados_grupos", ""
				 				choices = list(s)
				 			)
				 		)
				 	)
				 )
				 
	)
)




