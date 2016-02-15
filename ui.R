
shinyUI(
	navbarPage(
		"Regression Model Builder", id = "navbar",
		
		
		tabPanel("Interactive Plot", value = "plot",
						 sidebarLayout(
						 	sidebarPanel(
						 		
						 		# CSS code for website aesthetics:
						 		tags$head(
						 			tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
						 		),
						 		
						 		
						 		### Response variable ###
						 		selectInput("response", "Response variable (y)",
						 								choices = initialNumericVariables, selected = "RottenTomatoesScore"),
						 		
						 		### Explanatory variable ###
						 		selectInput("predictor", "Explanatory variable of interest (x)",
						 								choices = initialNumericVariables, selected = "Budget"),
						 		
						 		### List of controls ###
						 		span("Control variables", class="control-label"),
						 		tags$div(class = 'multicol',
							 		checkboxGroupInput("controls", "",
							 											 choices = initialNumericVariables,
							 											 inline = FALSE)
						 		), br(),
						 		
						 		### List of all variables ###
						 		span("Information displayed in tooltip", class="control-label"),
						 		tags$div(class = 'multicol',
							 		checkboxGroupInput("tooltip_vars", "Tooltip information",
							 											 choices = initialVariables,
							 											 selected = previousInputVars,
							 											 inline = FALSE)
						 		)
						 	),
						 	
						 	# Show a plot of the generated distribution
						 	mainPanel(
						 		htmlOutput("y_label"),
						 		ggvisOutput("ggvis_plot"),
						 		htmlOutput("x_label"),
						 		tabsetPanel(
						 			tabPanel(title = "Model summary",
						 				htmlOutput("model.summary")
						 			),
						 			tabPanel(title = "Model calculations",
						 							 "R output:",
						 				verbatimTextOutput("model.output")	
						 			)
						 		)
						 	)
						 )
		),
		
		tabPanel("Import Dataset", value = "import",
						 sidebarLayout(
						 	# Input in sidepanel:
						 	sidebarPanel(
						 		
						 		# Upload data:
						 		fileInput("file", "Upload data-file:"),
						 		# 						 		
						 								 		
						 		# 						 		
						 		# 						 		selectInput("dec", "Decimal",
						 		# 						 								choices = c("Period" = ".", "Comma" = ","),
						 		# 						 								selected = "."),
						 		# 						 		
						 		# 						 		selectInput("quote", "Quote",
						 		# 						 								choices = c('Double Quote (\")' = '\"',
						 		# 						 														"Single Quote (')" = "'"),
						 		# 						 								selected = '\"'),
						 		# 						 		
						 		
						 		
						 		
						 		
						 		htmlOutput("specifications"),
						 		
						 		# Specific CSV attributes
						 		actionLink("additionalspec", "Specify additional file parameters"),
						 		conditionalPanel(condition = "input.additionalspec %2 == 1" ,
						 										 selectInput("sep", "Field separator:", 
						 										 						choices = c("Whitespace" = "",
						 										 												"Comma" = ",",
						 										 												"Semicolon" = ";"),
						 										 						selected = ","),
						 										 textInput("na.strings", "Missing values coded as:", value = "NA")
						 										 
						 										 ),
						 										 
						 										 # Variable selection:
						 		htmlOutput("varselect"),
						 		
						 		# ID column
						 		htmlOutput("idselect"),
						 		
						 		htmlOutput("errormessage", class = "shiny-output-error"),
						 		
						 		# Commit dataset
						 		actionButton("commitdataset", "Use this dataset")
						 		
						 	),
						 	
						 	# Display table of dataset to import
						 	mainPanel(	
						 		dataTableOutput("table")
						 	)
						 )
		)
		
	)
)
