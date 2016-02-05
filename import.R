

### Dataset specifications:
output$specifications <- renderText({
	readChar("import_specs.html", file.info("import_specs.html")$size)
})

### Import data file:
uploadedDataset <- reactive({
	if (is.null(input$file)) { # User has not uploaded a file yet
		return(data.frame())
	}
	
	file.extension <- substr(input$file$name, 
													 nchar(input$file$name)-3, nchar(input$file$name))
	
	validate(
		need( file.extension == ".csv",
					message = "Dataset must be in a .csv format.")
	)
	
	if ( file.extension == ".csv") {
		read.csv(input$file$datapath,
						 stringsAsFactors = FALSE,
						 header = TRUE,
						 sep = input$sep,
						 na.strings = input$na.strings %>% strsplit(", ") %>% unlist)
	}
})

################################################################################
########################## Render UI Elements on page ##########################
################################################################################

### Additional formatting specifications
output$csvformat <- renderUI({
	
	if (identical(uploadedDataset(),data.frame())) {
		return(NULL)
	}
	
	selectInput("sep", "Separator", 
							choices = c("Whitespace" = "",
													"Comma" = ",",
													"Semicolon" = ";"),
							selected = ",")
	textInput("na.strings", "na.strings", value = "NA")
})

### Select variables:
output$varselect <- renderUI({
	
	if (identical(uploadedDataset(),data.frame())) {
		return(NULL)
	}
	
	div(class = "select", selectInput("vars", "Regression Variables",
																		choices = names(uploadedDataset()), 
																		selected = names(uploadedDataset()), 
																		multiple = TRUE)  )  
	
})

# Select identifier column:
output$idselect <- renderUI({
	
	if (identical(uploadedDataset(),data.frame())) return(NULL)
	
	selectInput("identifier", "Identifier (optional)",
							choices = names(uploadedDataset()) %>% c("None", .))            
})


################################################################################
# Render 
# Display table of dataset which will be used in the regression:
output$table <- renderDataTable({
	if (is.null(input$vars) || length(input$vars)==0) {
		return(NULL)
	}
	
	return((uploadedDataset()[,input$vars]))	
}, options = list(pageLength = 10))

# Renders error message to be displayed when there is a problem with the dataset
output$errormessage <- renderText({
	
	if(uploadedDataset() %>% identical(data.frame()) ){
		return("Please select a dataset.")
 	} else if( sum( sapply( uploadedDataset()[,input$vars], is.numeric)) < 2) {
 		return("You must have at least two numeric columns. Make sure that they are formatted correctly.")
	} else if( any (uploadedDataset()[[input$identifier]] %>% duplicated ) ) {
		return("Values in the identifying column must be unique!")
	}	else {
		return("")
	}
})

################################################################################

# When user clicks "Use this dataset" button,
observeEvent(input$commitdataset, {
	
	# checks if the selected dataset is not empty, the identifiers are unique, and
	# there are at least two numeric columns. If they are, updates the UI.
	if( !identical( uploadedDataset(), data.frame()) && 
			!any( uploadedDataset()[[input$identifier]] %>% duplicated ) &&
			sum( sapply( uploadedDataset()[,input$vars], is.numeric)) > 1) {

		updateSelectInput(session, "response",
											choices = numericVariables())
		updateSelectInput(session, "predictor",
											choices = numericVariables(), 
											selected = numericVariables()[2])
		updateCheckboxGroupInput(session, "controls",
														 choices = numericVariables()[-2] ,
														 inline = TRUE)
		updateCheckboxGroupInput(session, "tooltip_vars",
														 choices = datasetVariables(),
														 inline = TRUE)
		
		updateTabsetPanel(session, "navbar", selected = "plot")
	}
})
