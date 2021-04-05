# server.R
library(shiny)
library(tidyverse)
library(ggplot2)

# read in global data
data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_full.csv")
data.100 <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_100-cases.csv")
data.deaths10 <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/JHU_5-deaths.csv")

# UK data
UK.data <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/UK_total.csv")

# UK breakdown data
UK_by_country <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/UK_by_country.csv")

# UK county data
# read in UK county data
data.county <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/england_UTLA.csv")

# get list of counties
data.county$county_UA <- as.character(data.county$county_UA)
county_LA.list <- c(unique(data.county$county_UA))
list.county <- list()
for (i in 1:length(county_LA.list)){
	list.county[i] <- county_LA.list[i]
}
names(list.county) <- county_LA.list

# read in England region data
data.region <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/NHS_england_regions.csv")
data.region.pop <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/NHS_england_regions_pop.csv")

# Testing data
data.test <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/Data%20visualisation/UK%20data/UK_testing.csv")
data.test <- data.test %>%
	select(date, total_tested = tested)
data.test$date = as.Date(data.test$date, "%d/%m/%Y")
data.test$new_tested <- c(NA,diff(data.test$total_tested))

# code to deal with mismatch in lengths for testing and UK data
if(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) < nrow(data.test)){
	data.test <- data.test[1:(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"])),]
}

if(length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) > nrow(data.test)){
	x <- length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]) - nrow(data.test)
	data.test[((nrow(data.test)+1):length(UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"])),] <- NA
	for (i in (nrow(data.test)-x+1):nrow(data.test)){
		data.test[i,1]<- data.test[(i-1),1] + 1
	}
}

data.test$total_cases <- UK.data$number[UK.data$type=="total_cases" & UK.data$date>="2020-03-17"]
data.test$new_cases <- UK.data$number[UK.data$type=="new_cases" & UK.data$date>="2020-03-17"]

data.test$total_prop_pos <- 100*data.test$total_cases/data.test$total_tested
data.test$new_prop_pos <- 100*data.test$new_cases/data.test$new_tested

data.test <- data.test %>%
	gather(key="type", value="number",-date)

# Brazil data
data.brazil <- read_csv("https://raw.githubusercontent.com/maxeyre/COVID-19/master/data_scraper/data/processed/brazil_full.csv")

data.brazil$date <- as.Date(data.brazil$date, "%Y-%m-%d")
# get list of states
data.brazil$state_name <- as.character(data.brazil$state_name)
state.list <- c(unique(data.brazil$state_name))
state.list <- state.list[order(state.list)]
list.state <- list()
for (i in 1:length(state.list)){
	list.state[i] <- state.list[i]
}
names(list.state) <- state.list


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
	
	# Change date range for by country UK graphs
	
	observe({
		val <- input$checkGroup_UK
		if(length(val)<3 & input$tabs_UK==2){
			x <- sum("new_deaths" %in% val, "total_deaths" %in% val)
			if(x==length(val)) {
				updateDateRangeInput(session, "dateRange_UK", 
									 start  = as.Date("27/03/2020", "%d/%m/%Y"),
									 end    = max(UK.data$date), 
									 min    = as.Date("27/03/2020", "%d/%m/%Y"),
									 max    = max(UK.data$date))
			} else{
				updateDateRangeInput(session, "dateRange_UK", 
									 start  = as.Date("09/03/2020", "%d/%m/%Y"),
									 end    = max(UK.data$date), 
									 min    = as.Date("09/03/2020", "%d/%m/%Y"),
									 max    = max(UK.data$date))
				
			}
		}
	})
	
	formulaText <- reactive({
		paste(input$country)
	})
	
	formulaText_county <- reactive({
		paste(input$county)
	})
	
	output$startdate <- renderText({
		paste("Date range: ",as.character(input$dateRange[1])," to ",as.character(input$dateRange[2]),sep="")
	})
	
	# Return the formula text for printing as a caption
	output$caption <- renderText({
		formulaText()
	})
	
	output$caption_county <- renderText({
		formulaText_county()
	})
	
	red <- data.county[data.county$date == max(data.county$date) & data.county$type == "new_cases",]
	red <- red[order(red$number,decreasing=TRUE),]
	
	red2 <- data.county[data.county$date == max(data.county$date) & data.county$type == "total_cases",]
	red2 <- red2[order(red2$number,decreasing=TRUE),]
	
	output$county_newcase_update <- renderText({
		paste("Top 5 highest new daily cases: ", as.character(red$county_UA[1])," (", red$number[1],"), ",
			  as.character(red$county_UA[2])," (", red$number[2],"), ",
			  as.character(red$county_UA[3])," (", red$number[3],"), ",
			  as.character(red$county_UA[4])," (", red$number[4],"), ",
			  as.character(red$county_UA[5])," (", red$number[5],"), ", sep="")
	})
	
	output$county_totalcase_update <- renderText({
		paste("Top 5 highest total cases: ", as.character(red2$county_UA[1])," (", red2$number[1],"), ",
			  as.character(red2$county_UA[2])," (", red2$number[2],"), ",
			  as.character(red2$county_UA[3])," (", red2$number[3],"), ",
			  as.character(red2$county_UA[4])," (", red2$number[4],"), ",
			  as.character(red2$county_UA[5])," (", red2$number[5],"), ", sep="")
	})
	
	
	url <- a("Twitter", href="https://twitter.com/maxeyre3")
	
	output$twitter <- renderUI({
		tagList(url)
	})
	
	output$twitter2 <- renderUI({
		tagList(url)
	})
	output$twitter_comp <- renderUI({
		tagList(url)
	})
	output$twitter3 <- renderUI({
		tagList(url)
	})
	output$twitter4 <- renderUI({
		tagList(url)
	})
	
	output$twitter_UK <- renderUI({
		tagList(url)
	})
	
	output$twitter_br <- renderUI({
		tagList(url)
	})
	
	url_data <- a("JHU CSSE Data sources", href="https://github.com/CSSEGISandData/COVID-19")
	url_github <- a("GitHub", href="https://github.com/maxeyre/COVID-19")
	url_data2 <- a("Data source", href="https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public")
	
	url_data_br <- a("Data source", href="https://github.com/wcota/covid19br")
	
	output$data_source <- renderUI({
		tagList(url_data)
	})
	output$data_source_comp <- renderUI({
		tagList(url_data)
	})
	
	output$data_source2 <- renderUI({
		tagList(url_data2)
	})
	output$data_source_UK <- renderUI({
		tagList(url_data2)
	})
	
	output$data_source3 <- renderUI({
		tagList(url_data2)
	})
	output$data_source4 <- renderUI({
		tagList(url_data2)
	})
	
	output$data_source_br <- renderUI({
		tagList(url_data_br)
	})
	
	output$git1 <- renderUI({
		tagList(url_github)
	})
	output$git2 <- renderUI({
		tagList(url_github)
	})
	output$git3 <- renderUI({
		tagList(url_github)
	})
	output$git4 <- renderUI({
		tagList(url_github)
	})
	output$git5 <- renderUI({
		tagList(url_github)
	})
	output$git6 <- renderUI({
		tagList(url_github)
	})
	output$git_br <- renderUI({
		tagList(url_github)
	})
	
	output$checkGroup <- renderText({
		paste(as.character(length(input$checkGroup)))
	})
	
	output$checkGroup_county <- renderText({
		paste(as.character(c(input$checkGroup_county)))
	})
	output$checkGroup_region <- renderText({
		paste(as.character(c(input$checkGroup_region)))
	})
	
	output$dateRange.100 <- renderPrint({ input$dateRange.100 })  
	
	output$counter <- renderText({
		library(rdrop2)
		token <- readRDS("token.rds")
		counter <- drop_read_csv("counter.csv",dtoken = token)
		counter$count <- counter$count + 1
		counter <- counter%>%
			select(count)
		write.csv(counter, file = "counter.csv")
		drop_upload("counter.csv",dtoken = token)
		paste0(counter$count," site visits", sep="")
	})
	
	# Single country plots
	output$countryPlot <- renderPlot({
		lines <- c(as.character(input$checkGroup))
		
		data<- data[data$type %in% lines, ]
		if(input$pop_country=="pop_yes"){
			p <- ggplot(data[data$country==paste(formulaText(),sep=""),]) + geom_point(aes(x=date, y=number_pop, col=type),size=1.5) +
				geom_line(aes(x=date, y=number_pop, col=type),size=1) +
				scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Number (per 100,000)") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-1.5),
					  axis.title.y = element_text(vjust=2),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
													   "new_deaths"="#a65628","total_recoveries"="#377eb8", "new_recoveries"="#4daf4a"),
									breaks=c("new_cases","total_cases","new_deaths","total_deaths","new_recoveries","total_recoveries"),
									labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)","Recoveries (daily)","Recoveries (total)")) +
				guides(linetype = guide_legend(override.aes = list(size = 20))) +
				theme(legend.direction = "horizontal",legend.box = "vertical")
			if(input$log=='log_yes'){
				p <- p + scale_y_log10()
			}
		}
		else{
			p <- ggplot(data[data$country==paste(formulaText(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
				geom_line(aes(x=date, y=number, col=type),size=1) +
				scale_x_date(limits=c(input$dateRange[1],input$dateRange[2])) + xlab(label = "") +ylab(label="Number") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-1.5),
					  axis.title.y = element_text(vjust=2),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
													   "new_deaths"="#a65628","total_recoveries"="#377eb8", "new_recoveries"="#4daf4a"),
									breaks=c("new_cases","total_cases","new_deaths","total_deaths","new_recoveries","total_recoveries"),
									labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)","Recoveries (daily)","Recoveries (total)")) +
				guides(linetype = guide_legend(override.aes = list(size = 20)))
			if(input$log=='log_yes'){
				p <- p + scale_y_log10(labels = scales::comma) 
			}
		}
		p
	})
	
	# country comparisons
	output$countryPlot_compare <- renderPlot({
		lines2 <- c(as.character(input$checkGroup_countryCompare))
		
		if(input$compare_by=="cases"){
			data.100<- data.100[data.100$country %in% lines2, ]
			lab_y <- "Cases"
			
		}else{
			data.100<- data.deaths10[data.deaths10$country %in% lines2, ]
			lab_y <- "Deaths"
		}
		
		if(input$compare_pop=="pop_no"){
			y_min <- min(data.100$number[data.100$date_rel==0], na.rm=TRUE)
			y_max <- max(data.100$number, na.rm=TRUE)
			
			p2 <- ggplot(data.100) + geom_point(aes(x=date_rel, y=number, col=country),size=1.5) +
				geom_line(aes(x=date_rel, y=number, col=country),size=1) +
				scale_x_continuous(limits=c(input$dateRange.100[1],input$dateRange.100[2])) + scale_y_continuous(limits=c(y_min,y_max), labels= scales::comma) + 
				xlab(label = "Days") +
				ylab(label=paste(lab_y)) +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-0.5),
					  axis.title.y = element_text(vjust=2),
					  legend.title = element_blank(),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				guides(linetype = guide_legend(override.aes = list(size = 20))) 
			if(input$log_compare=='log_yes'){
				p2 <- p2 + scale_y_log10(limits=c(y_min,y_max), labels = scales::comma)
			}
		} else{
			y_min <- min(data.100$number_pop[data.100$date_rel_pop==0], na.rm=TRUE)
			y_max <- max(data.100$number_pop, na.rm=TRUE)
			
			p2 <- ggplot(data.100) + geom_point(aes(x=date_rel_pop, y=number_pop, col=country),size=1.5) +
				geom_line(aes(x=date_rel_pop, y=number_pop, col=country),size=1) +
				scale_x_continuous(limits=c(input$dateRange.100[1],input$dateRange.100[2])) + scale_y_continuous(limits=c(y_min,y_max)) + xlab(label = "Days") +
				ylab(label=paste(lab_y," (per 100,000)",sep="")) +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-0.5),
					  axis.title.y = element_text(vjust=2),
					  legend.title = element_blank(),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				guides(linetype = guide_legend(override.aes = list(size = 20))) 
			if(input$log_compare=='log_yes'){
				p2 <- p2 + scale_y_log10(limits=c(y_min,y_max))
			}
		}
		
		p2
	})
	
	
	# UK plot
	output$UKPlot <- renderPlot({
		lines <- c(as.character(input$checkGroup_UK))
		
		UK.data<- UK.data[UK.data$type %in% lines, ]
		if (input$pop_UK=="pop_yes"){
			p <- ggplot(UK.data) + geom_point(aes(x=date, y=100000*number/66440000, col=type),size=1.5) +
				geom_line(aes(x=date, y=100000*number/66440000, col=type),size=1) +
				scale_x_date(limits=c(input$dateRange_UK[1],input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Number (per 100,000)") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-1.5),
					  axis.title.y = element_text(vjust=2),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
													   "new_deaths"="#a65628"),
									breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
									labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
				guides(linetype = guide_legend(override.aes = list(size = 20)))
			if(input$log_UK=='log_yes'){
				p <- p + scale_y_log10()
			}
		} else{
			p <- ggplot(UK.data) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
				geom_line(aes(x=date, y=number, col=type),size=1) +
				scale_x_date(limits=c(input$dateRange_UK[1],input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Number") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-1.5),
					  axis.title.y = element_text(vjust=2),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c", "total_deaths"="#ff7f00", 
													   "new_deaths"="#a65628"),
									breaks=c("new_cases","total_cases","new_deaths","total_deaths"),
									labels=c("Cases (daily)", "Cases (total)", "Deaths (daily)","Deaths (total)")) +
				guides(linetype = guide_legend(override.aes = list(size = 20)))
			if(input$log_UK=='log_yes'){
				p <- p + scale_y_log10(labels = scales::comma)
			}
		}
		p
	})
	
	# UK plot
	output$UKPlot_by_country <- renderPlot({
		lines <- c(as.character(input$checkGroup_UK))
		
		UK_by_country<- UK_by_country[UK_by_country$type %in% lines, ]
		
		val <- input$checkGroup_UK
		if(length(val)<3 & input$tabs_UK==2){
			x <- sum("new_deaths" %in% val, "total_deaths" %in% val)
			if(x==length(val)) {
				date.min <- as.Date("27/03/2020", "%d/%m/%Y")
			} else {
				date.min <- as.Date("09/03/2020", "%d/%m/%Y")
			}
		} else {
			date.min <- as.Date("09/03/2020", "%d/%m/%Y")
		}
		
		if (input$pop_UK=="pop_yes"){
			p <- ggplot(UK_by_country) + geom_point(aes(x=date, y=100000*number/pop, col=country),size=1.5) +
				geom_line(aes(x=date, y=100000*number/pop, col=country, linetype=type),size=1) +
				scale_x_date(limits=c(date.min,input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Number (per 100,000)") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-1.5),
					  axis.title.y = element_text(vjust=2),
					  legend.title = element_blank(),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +  scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
					  																	 breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
					  																	 labels=c("Cases (total)","Cases (daily)","Deaths (total)","Deaths (daily)")) +
				guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
				theme(legend.direction = "horizontal",legend.box = "vertical")
			
			if(input$log_UK=='log_yes'){
				p <- p + scale_y_log10()
			}
		}else{
			p <- ggplot(UK_by_country) + geom_point(aes(x=date, y=number, col=country),size=1.5) +
				geom_line(aes(x=date, y=number, col=country, linetype=type),size=1) +
				scale_x_date(limits=c(date.min,input$dateRange_UK[2])) + xlab(label = "") +ylab(label="Number") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-1.5),
					  axis.title.y = element_text(vjust=2),
					  legend.title = element_blank(),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +  scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
					  																	 breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
					  																	 labels=c("Cases (total)","Cases (daily)","Deaths (total)","Deaths (daily)")) +
				guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
				theme(legend.direction = "horizontal",legend.box = "vertical")
			
			if(input$log_UK=='log_yes'){
				p <- p + scale_y_log10(labels = scales::comma)
			}
		}
		
		p
	})
	
	# England NHS regions plots
	output$EnglandRegionPlot <- renderPlot({
		
		lines <- c(as.character(input$checkGroup_region))
		
		if (input$pop=="pop_yes"){
			data.region <- data.region.pop
		}
		data.region<- data.region[data.region$type %in% lines, ]
		
		p.pop <- ggplot(data.region) + geom_point(aes(x=date, y=number, col=region),size=1.5) +
			geom_line(aes(x=date, y=number, col=region, linetype=type),size=1) +
			scale_x_date(limits=c(input$dateRange_region[1],input$dateRange_region[2])) + xlab(label = "") +ylab(label="Cases") +
			theme_classic()+
			theme(axis.text=element_text(size=13),
				  axis.title=element_text(size=16), 
				  axis.title.x = element_text(vjust=-1.5),
				  axis.title.y = element_text(vjust=2),
				  legend.title = element_blank(),
				  legend.text = element_text(size=13),
				  legend.position = 'top', 
				  legend.spacing.x = unit(0.4, 'cm'),
				  panel.grid.major.y=element_line(size=0.05)) + scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2),
				  																	breaks=c("total_cases","new_cases"),
				  																	labels=c("Cases (total)","Cases (daily)")) +
			guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
			theme(legend.direction = "horizontal",legend.box = "vertical")
		if (input$pop=="pop_yes"){
			p.pop <- p.pop +  ylab(label="Cases (per 100,000)")
		}
		
		if(input$log_region=='log_yes'){
			p.pop <- p.pop + scale_y_log10(labels = scales::comma)
		}
		p.pop
	})
	
	# England county plots
	output$englandcountyPlot <- renderPlot({
		lines <- c(as.character(input$checkGroup_county))
		
		data.county <- data.county[data.county$type %in% lines, ]
		
		ggplot(data.county[data.county$county_UA==paste(formulaText_county(),sep=""),]) + geom_point(aes(x=date, y=number, col=type),size=1.5) +
			geom_line(aes(x=date, y=number, col=type),size=1) +
			scale_x_date(limits=c(input$dateRange_county[1],input$dateRange_county[2])) + xlab(label = "") +ylab(label="Cases") +
			theme_classic()+
			theme(axis.text=element_text(size=13),
				  axis.title=element_text(size=16), 
				  axis.title.x = element_text(vjust=-1.5),
				  axis.title.y = element_text(vjust=2),
				  legend.text = element_text(size=13),
				  legend.position = 'top', 
				  legend.spacing.x = unit(0.4, 'cm'),
				  panel.grid.major.y=element_line(size=0.05)) +
			scale_colour_manual(name="",values = c("total_cases" = "#000000", "new_cases" = "#e41a1c"),
								breaks=c("new_cases","total_cases"),
								labels=c("Cases (daily)", "Cases (total)")) +
			guides(linetype = guide_legend(override.aes = list(size = 20)))
	})
	
	# UK testing plot
	output$UKtestingPlot <- renderPlot({
		
		lines <- c(as.character(input$checkGroup_test))
		
		data.test <- data.test[data.test$type %in% lines, ]
		
		p.test <- ggplot(data.test) + geom_point(aes(x=date, y=number, col=type),size=1.5)+ 
			geom_line(aes(x=date, y=number, col=type, group=type),size=1) +
			scale_x_date(limits=c(input$dateRange_test[1],input$dateRange_test[2])) + xlab(label = "") +ylab(label="Number tested") +
			theme_classic()+
			theme(axis.text=element_text(size=13),
				  axis.title=element_text(size=16), 
				  axis.title.x = element_text(vjust=-1.5),
				  axis.title.y = element_text(vjust=2),
				  legend.text = element_text(size=13),
				  legend.position = 'top', 
				  legend.spacing.x = unit(0.4, 'cm'),
				  panel.grid.major.y=element_line(size=0.05)) +
			scale_colour_manual(name="",values = c("total_tested" = "#000000", "new_tested" = "#e41a1c"),
								breaks=c("new_tested","total_tested"),
								labels=c("Daily", "Total"))
		if(input$log_test=='log_yes'){
			p.test <- p.test + scale_y_log10(labels = scales::comma)
		}
		p.test
	})
	
	output$UKtestingPlot2 <- renderPlot({
		
		lines <- c(as.character(input$checkGroup_test2))
		
		data.test <- data.test[data.test$type %in% lines, ]
		
		p.test <- ggplot(data.test) + geom_point(aes(x=date, y=number, col=type),size=1.5)+ 
			geom_line(aes(x=date, y=number, col=type, group=type),size=1) +
			scale_x_date(limits=c(input$dateRange_test2[1],input$dateRange_test2[2])) + xlab(label = "") +ylab(label="Prop. positive (%)") +
			theme_classic()+
			theme(axis.text=element_text(size=13),
				  axis.title=element_text(size=16), 
				  axis.title.x = element_text(vjust=-1.5),
				  axis.title.y = element_text(vjust=2),
				  legend.text = element_text(size=13),
				  legend.position = 'top', 
				  legend.spacing.x = unit(0.4, 'cm'),
				  panel.grid.major.y=element_line(size=0.05)) +
			scale_colour_manual(name="",values = c("total_prop_pos" = "#000000", "new_prop_pos" = "#e41a1c"),
								breaks=c("new_prop_pos","total_prop_pos"),
								labels=c("Daily", "Total"))
		p.test
		
	})
	
	# Brazil state comparisons
	output$statePlot_compare_br <- renderPlot({
		lines_type <- c(as.character(input$checkGroup_br))
		
		data.br <- data.brazil[data.brazil$type %in% lines_type, ]
		
		lines2 <- c(as.character(input$checkGroup_stateCompare_br))
		
		data.br <- data.br[data.br$state_name %in% lines2, ]
		
		lab_y <- "Casos/óbitos relatados"
		
		
		if(input$compare_pop_br=="pop_no"){
			
			p2 <- ggplot(data.br) + geom_point(aes(x=date, y=number, col=state_name),size=1.5) +
				geom_line(aes(x=date, y=number, col=state_name,linetype=type),size=1) +
				scale_x_date(limits=c(input$dateRange_br[1],input$dateRange_br[2])) + scale_y_continuous(labels= scales::comma) + 
				ylab(label=paste(lab_y)) + xlab(label = "") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-0.5),
					  axis.title.y = element_text(vjust=2),
					  legend.title = element_blank(),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
									  breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
									  labels=c("Casos (total)","Casos (diários)","Óbitos (total)","Óbitos (diários)")) +
				guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
				theme(legend.direction = "horizontal",legend.box = "vertical") 
			if(input$log_compare_br=='log_yes'){
				p2 <- p2 + scale_y_log10()
			}
		} else{
			
			p2 <- ggplot(data.br) + geom_point(aes(x=date, y=number_pop, col=state_name),size=1.5) +
				geom_line(aes(x=date, y=number_pop, col=state_name,linetype=type),size=1) +
				scale_x_date(limits=c(input$dateRange_br[1],input$dateRange_br[2])) +
				ylab(label=paste(lab_y," (per 100,000)",sep="")) + xlab(label = "") +
				theme_classic()+
				theme(axis.text=element_text(size=13),
					  axis.title=element_text(size=16), 
					  axis.title.x = element_text(vjust=-0.5),
					  axis.title.y = element_text(vjust=2),
					  legend.title = element_blank(),
					  legend.text = element_text(size=13),
					  legend.position = 'top', 
					  legend.spacing.x = unit(0.4, 'cm'),
					  panel.grid.major.y=element_line(size=0.05)) +
				scale_linetype_manual(name="", values=c("total_cases"=1, "new_cases" = 2, "total_deaths" =3, "new_deaths"=4),
									  breaks=c("total_cases","new_cases","total_deaths","new_deaths"),
									  labels=c("Casos (total)","Casos (diários)","Óbitos (total)","Óbitos (diários)")) +
				guides(linetype = guide_legend(label.position = "top", keywidth = 2)) +
				theme(legend.direction = "horizontal",legend.box = "vertical")  
			if(input$log_compare_br=='log_yes'){
				p2 <- p2 + scale_y_log10()
			}
		}
		
		p2
	})
	
	
})