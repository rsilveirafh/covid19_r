library(shiny)

ui<-
	fluidPage(
		fluidRow(
			column(4,
				   h2("Reactive Test"),
				   textInput("Test_R","Test_R"),
				   textInput("Test_R2","Test_R2"),
				   textInput("Test_R3","Test_R3"),
				   tableOutput("React_Out")
			),
			column(4,
				   h2("Observe Test"),
				   textInput("Test","Test"),
				   textInput("Test2","Test2"),
				   textInput("Test3","Test3"),
				   tableOutput("Observe_Out")
			),
			column(4,
				   h2("Observe Event Test"),
				   textInput("Test_OE","Test_OE"),
				   textInput("Test_OE2","Test_OE2"),
				   textInput("Test_OE3","Test_OE3"),
				   tableOutput("Observe_Out_E"),
				   actionButton("Go","Test")
			),
			column(4,
				   h2("Eita"),
				   uiOutput("st_select")
			)
			
		),
		fluidRow(
			column(8,
				   h4("Note that observe and reactive work very much the same on the surface,
       it is when we get into the server where we see the differences, and how those
       can be exploited for diffrent uses.")
			))
		
	)

server<-function(input,output,session){
	
	# Create a reactive Evironment. Note that we can call the varaible outside same place
	# where it was created by calling Reactive_Var(). When the varaible is called by
	# renderTable is when it is evaluated. No real diffrence on the surface, all in the server.
	
	Reactive_Var<-reactive({c(input$Test_R, input$Test_R2, input$Test_R3)})
	
	output$React_Out<-renderTable({
		Reactive_Var()
	})
	
	# Create an observe Evironment. Note that we cannot access the created "df" outside 
	# of the env. A, B,and C will update with any input into any of the three Text Feilds.
	observe({
		A<-input$Test
		B<-input$Test2
		C<-input$Test3
		df<-c(A,B,C)
		output$Observe_Out<-renderTable({df})
	})
	
	#We can change any input as much as we want, but the code wont run until the trigger
	# input$Go is pressed.
	observeEvent(input$Go, {
		A<-input$Test_OE
		B<-input$Test_OE2
		C<-input$Test_OE3
		df<-c(A,B,C)
		output$Observe_Out_E<-renderTable({df})
	})
	
	output$st_select <- renderUI({
		selectInput("st_select", 
					label = "Selecione o Estado:",
					choices = list(`Centro-Oeste` = list("Distrito Federal" = "DF", "Goiás", "Mato Grosso", "Mato Grosso do Sul"),
								   `Nordeste` = list("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", 
								   				  "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe"),
								   `Norte` = list("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins"),
								   `Sul` = list("Paraná", "Rio Grande do Sul", "Santa Catarina"),
								   `Suldeste` = list("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo")))
	})
	observe({
		print(input$st_select)
	})
	
	
	
}
shinyApp(ui, server)