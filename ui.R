## UI ----

# Header ----
header <- dashboardHeader(title = "Brasil ─ COVID-19")

# Sidebar ----
sidebar <- dashboardSidebar(
	sidebarMenu(
		menuItem("Brasil", tabName = "br_dashboard", icon = icon("globe-americas")),
		menuItem("Estados", tabName = "st_dashboard", icon = icon("globe")),
		menuItem("Municípios", tabName = "ct_dashboard", icon = icon("map-marker-alt")),
		menuItem("Código Fonte", icon = icon("file-code-o"), href = "https://github.com/rsilveirafh/covid19_r")
	)
)

# Body ----
body <- dashboardBody(
	tabItems(
		# Brazil Tab ----
		tabItem(tabName = "br_dashboard",
				fluidRow(
					h3("# Dados do COVID-19 para todo o país"),
					h5("# Passe o ponteiro do mouse nos gráficos para mais informações")
				),
				fluidRow(
					valueBox(max(brazil$totalC), "Casos confirmados", icon = icon("check-circle"), 
							 width = 3, color = "blue"),
					valueBox(max(brazil$newC), 
							 paste0("Novos casos, ", format.Date(max(brazil$date), "%d/%m/%Y")), 
							 icon = icon("plus"), width = 3, color = "teal"),
					valueBox(max(brazil$totalD), "Mortes", icon = icon("times-circle"), 
							 width = 3, color = "olive"),
				 	valueBox(paste0(round(max(brazil$totalD)*100/max(brazil$totalC), 2), "%"), 
							 "Taxa de mortes", icon = icon("percentage"), 
							 width = 3, color = "maroon")
				),
				fluidRow(
				 	box(title = "Modificações dos gráficos:",
				 		width = 2,
				 		status = "primary",
				 		sliderInput("br_date",
				 					label = "Selecione as datas:",
				 					min = as.Date(min(brazil$date), "%d/%m"),
				 					max = as.Date(max(brazil$date), "%d/%m"),
				 					value = c(as.Date(min(brazil$date), "%d/%m"), as.Date(max(brazil$date), "%d/%m"))),
				 		radioButtons("br_scale", 
				 					 label = "Selecione a escala:",
				 					 choices = list("Linear" = "br_linear", 
				 					 			   "Logarítmica" = "br_log"))
				 	),
				 	box(title = "Brasil",
				 		status = "primary",
				 		plotlyOutput("br_plot")
					),
					box(title = "Evolução da taxa de mortes",
						status = "danger",
						plotlyOutput("br_rate"),
						width = 4
					)
				),
		 		fluidRow(
		 			h6("# Criado por Ricardo da Silveira Filho"),
		 			h6("# Qualquer sugestão, por favor entre em contato por", tags$b("ricardodasilveira at gmail.com")),
		 			h5(uiOutput("git1")),
		 			h5(uiOutput("database1"))
		 		)
		),
		# States Tab ----
		tabItem(tabName = "st_dashboard",
				fluidRow(
					h3("# Dados do COVID-19 para cada estado brasileiro"),
					h5("# Passe o ponteiro do mouse nos gráficos para mais informações")
				),
				fluidRow(
					valueBoxOutput("st_totalC", width = 3),
					valueBoxOutput("st_newC", width = 3),
					valueBoxOutput("st_totalD", width = 3),
					valueBoxOutput("st_r", width = 3)
				),
				fluidRow(
					box(title = "Modificações dos gráficos:",
						width = 2,
						status = "primary",
						uiOutput("st_select"),
						sliderInput("st_date",
									label = "Selecione as datas:",
									min = as.Date(min(states$date), "%d/%m"),
									max = as.Date(max(states$date), "%d/%m"),
									value = c(as.Date(min(states$date), "%d/%m"), as.Date(max(states$date), "%d/%m"))),
						radioButtons("st_scale", 
									 label = "Selecione a escala:",
									 choices = list("Linear" = "st_linear", 
									 			   "Logarítmica" = "st_log"))
					),
					box(title = "Estado:",
						status = "primary",
						plotlyOutput("st_plot")
					),
					box(title = "Evolução da taxa de mortes",
						status = "danger",
						plotlyOutput("st_rate"),
						width = 4
					)
				),
				fluidRow(
					h6("# Criado por Ricardo da Silveira Filho"),
					h6("# Qualquer sugestão, por favor entre em contato por", tags$b("ricardodasilveira at gmail.com")),
					h5(uiOutput("git2")),
					h5(uiOutput("database2"))
				)
		),
		# Cities Tab ----
		tabItem(tabName = "ct_dashboard",
				fluidRow(
					h3("# Dados do COVID-19 para os municípios brasileiros"),
					h5("# Passe o ponteiro do mouse nos gráficos para mais informações")
				),
				fluidRow(
					valueBoxOutput("ct_totalC", width = 3),
					valueBoxOutput("ct_newC", width = 3),
					valueBoxOutput("ct_totalD", width = 3),
					valueBoxOutput("ct_r", width = 3)
				),
				fluidRow(
					box(title = "Modificações dos gráficos:",
						width = 2,
						status = "primary",
						uiOutput("ct_select_state"),
						uiOutput("ct_select_city"),
						sliderInput("ct_date",
									label = "Selecione as datas:",
									min = as.Date(min(cities$date), "%d/%m"),
									max = as.Date(max(cities$date), "%d/%m"),
									value = c(as.Date(min(cities$date), "%d/%m"), as.Date(max(cities$date), "%d/%m"))),
						radioButtons("ct_scale", 
									 label = "Selecione a escala:",
									 choices = list("Linear" = "ct_linear", 
									 			   "Logarítmica" = "ct_log"))
					),
					box(title = "Estado:",
						status = "primary",
						plotlyOutput("ct_plot")
					),
					box(title = "Evolução da taxa de mortes",
						status = "danger",
						plotlyOutput("ct_rate"),
						width = 4
					)
				),
				fluidRow(
					h6("# Criado por Ricardo da Silveira Filho"),
					h6("# Qualquer sugestão, por favor entre em contato por", tags$b("ricardodasilveira at gmail.com")),
					h5(uiOutput("git2")),
					h5(uiOutput("database2"))
				)
		)
	)
)

ui <- dashboardPage(header, sidebar, body)