## server ----
# Contains the instructions to build the App

server <- function(input, output) {
	histdata <- rnorm(500)
	output$plot1 <- renderPlot({
		data <- histdata[seq_len(input$slider)]
		hist(data)
	})
}