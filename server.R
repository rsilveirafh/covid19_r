## Server ----
server <- function(input, output) {
	
	# Brasil ----
	output$br_plot <- renderPlotly({
		br_graph <- brazil %>% 
			pivot_longer(newC:totalD) %>% 
			mutate(name = str_replace(name, "newC", "Novos casos no dia"),
				   name = str_replace(name, "totalC", "Total de casos"),
				   name = str_replace(name, "totalD", "Total de mortes"))
		
		if (input$br_scale == "br_linear") {
			br_p <- ggplot(br_graph, aes(x = date, y = value, group = name, colour = name)) +
				geom_line(size = 1.3, alpha = 0.7) +
				geom_point(size = 2, alpha = 0.7) +
				scale_x_date(limits = c(input$br_date[1], input$br_date[2]),
							 name = "Datas", breaks = "1 week", date_labels = "%d/%m") +
				scale_y_continuous(name = "Quantidade de pessoas", n.breaks = 10) +
				scale_colour_manual(name = NULL, 
									values = c("#39CCCC", "#0072B6", "#3D9970")) +
				theme_minimal() +
				theme(legend.text = element_text(size = 10),
					  axis.title = element_text(size = 14),
					  axis.text = element_text(size = 12))
		} else {
			br_p <- ggplot(br_graph, aes(x = date, y = value, group = name, colour = name)) +
				geom_line(size = 1.3, alpha = 0.7) +
				geom_point(size = 2, alpha = 0.7) +
				scale_x_date(limits = c(input$br_date[1], input$br_date[2]),
							 name = "Datas", breaks = "1 week", date_labels = "%d/%m") +
				scale_y_log10(name = "Quantidade de pessoas", n.breaks = 10) +
				scale_colour_manual(name = NULL, 
									values = c("#39CCCC", "#0072B6", "#3D9970")) +
				theme_minimal() +
				theme(legend.text = element_text(size = 10),
					  axis.title = element_text(size = 14),
					  axis.text = element_text(size = 12))
		}
		ggplotly(br_p) %>% layout(legend = list(orientation = "v", x = 0.05, y = 0.95))
	 })
	
	output$br_rate <- renderPlotly({
		br_rate_g <- brazil %>% 
			mutate(rate = round(totalD * 100 / totalC, 2))
			
		br_r <- ggplot(br_rate_g, aes(x = date, y = rate)) +
			geom_bar(stat = "identity", alpha = 0.7, fill = "#D81B60") +
			scale_x_date(limits = c(input$br_date[1], input$br_date[2]),
						 name = "Datas", breaks = "1 week", date_labels = "%d/%m") +
			scale_y_continuous(name = "Taxa de mortes (%)", n.breaks = 10) +
			theme_minimal() +
			theme(legend.text = element_text(size = 10),
				  axis.title = element_text(size = 14),
				  axis.text = element_text(size = 12))
		ggplotly(br_r)
	})
	
	url_git <- a("GitHub", href = "https://github.com/rsilveirafh")
	url_data <- a("Fonte dos dados", href = "https://github.com/wcota/covid19br")
	
	output$git1 <- renderUI({
	 	tagList(url_git)
	})
	 
	output$database1 <- renderUI({
	 	tagList(url_data)
	})

	# Estados ----
	output$st_select <- renderUI({
		selectInput("st_select", 
					label = "Selecione o Estado:",
					choices = list(`Centro-Oeste` = list("Distrito Federal" = "DF", "Goiás" = "GO", 
														 "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS"),
								   `Nordeste` = list("Alagoas" = "AL", "Bahia" = "BA", "Ceará" = "CE",
								   				  "Maranhão" = "MA", "Paraíba" = "PB", "Pernambuco" = "PE", 
								   				  "Piauí" = "PI", "Rio Grande do Norte" = "RN", "Sergipe" = "SE"),
								   `Norte` = list("Acre" = "AC", "Amapá" = "AP", "Amazonas" = "AM", "Pará" = "PA",
								   			   "Rondônia" = "RO", "Roraima" = "RR", "Tocantins" = "TO"),
								   `Sul` = list("Paraná" = "PR", "Rio Grande do Sul" = "RS", "Santa Catarina" = "SC"),
								   `Suldeste` = list("Espírito Santo" = "ES", "Minas Gerais" = "MG", 
								   				  "Rio de Janeiro" = "RJ", "São Paulo" = "SP")))
	})
	
	st_filter <- reactive ({
		if (is.null(input$st_select)) {
			return(NULL)
		}  
		states %>%
			filter(state == input$st_select)
	})
	
	output$st_totalC <- renderValueBox({
		if (is.null(st_filter())) {
			return()
		}
		valueBox(max(st_filter()$totalC), "Casos confirmados", 
				 icon = icon("check-circle"), color = "blue")
	})
	
	output$st_newC <- renderValueBox({
		if (is.null(st_filter())) {
			return()
		}
		valueBox(last(st_filter()$newC), 
				 paste0("Novos casos, ", format.Date(max(st_filter()$date), "%d/%m/%Y")), 
				 icon = icon("plus"), color = "teal")
	})
	
	output$st_totalD <- renderValueBox({
		if (is.null(st_filter())) {
			return()
		}
		valueBox(max(st_filter()$totalD), "Mortes", icon = icon("times-circle"), color = "olive")
	})
	
	output$st_r <- renderValueBox({
		if (is.null(st_filter())) {
			return()
		}
		valueBox(paste0(round(max(st_filter()$totalD)*100/max(st_filter()$totalC), 2), "%"), 
				 "Taxa de mortes", icon = icon("percentage"), color = "maroon")
	})
	
	output$st_plot <- renderPlotly ({
		if (is.null(st_filter())) {
			return()
		}
		
		st_graph <- st_filter() %>% 
			pivot_longer(newC:totalD) %>% 
			mutate(name = str_replace(name, "newC", "Novos casos no dia"),
				   name = str_replace(name, "totalC", "Total de casos"),
				   name = str_replace(name, "totalD", "Total de mortes"))
		
		if (input$st_scale == "st_linear") {
			st_p <- ggplot(st_graph, aes(x = date, y = value, group = name, colour = name)) +
				geom_line(size = 1.3, alpha = 0.7) +
				geom_point(size = 2, alpha = 0.7) +
				scale_x_date(limits = c(input$st_date[1], input$st_date[2]),
							 name = "Datas", breaks = "1 week", date_labels = "%d/%m") +
				scale_y_continuous(name = "Quantidade de pessoas", n.breaks = 10) +
				scale_colour_manual(name = NULL, 
									values = c("#39CCCC", "#0072B6", "#3D9970")) +
				theme_minimal() +
				theme(legend.text = element_text(size = 10),
					  axis.title = element_text(size = 14),
					  axis.text = element_text(size = 12))
		} else {
			st_p <- ggplot(st_graph, aes(x = date, y = value, group = name, colour = name)) +
				geom_line(size = 1.3, alpha = 0.7) +
				geom_point(size = 2, alpha = 0.7) +
				scale_x_date(limits = c(input$st_date[1], input$st_date[2]),
							 name = "Datas", breaks = "1 week", date_labels = "%d/%m") +
				scale_y_log10(name = "Quantidade de pessoas", n.breaks = 10) +
				scale_colour_manual(name = NULL, 
									values = c("#39CCCC", "#0072B6", "#3D9970")) +
				theme_minimal() +
				theme(legend.text = element_text(size = 10),
					  axis.title = element_text(size = 14),
					  axis.text = element_text(size = 12))
		}
		ggplotly(st_p) %>% layout(legend = list(orientation = "v", x = 0.05, y = 0.95))
	})
	
	output$st_rate <- renderPlotly({
		if (is.null(st_filter())) {
			return()
		}
		st_rate_g <- st_filter() %>% 
			mutate(rate = round(totalD * 100 / totalC, 2))
		
		st_r <- ggplot(st_rate_g, aes(x = date, y = rate)) +
			geom_bar(stat = "identity", alpha = 0.7, fill = "#D81B60") +
			scale_x_date(limits = c(input$st_date[1], input$st_date[2]),
						 name = "Datas", breaks = "1 week", date_labels = "%d/%m") +
			scale_y_continuous(name = "Taxa de mortes (%)", n.breaks = 10) +
			theme_minimal() +
			theme(legend.text = element_text(size = 10),
				  axis.title = element_text(size = 14),
				  axis.text = element_text(size = 12))
		ggplotly(st_r)
	})

	output$git2 <- renderUI({
		tagList(url_git)
	})
	
	output$database2 <- renderUI({
		tagList(url_data)
	})
	
	
	# Municípios ----
}


