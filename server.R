
### TO DO: IMPORTING DATA AFTER DELETING COLUMNS CRASHES THE APP

# Adjust max file upload size if necessary: default is 5MB
# megabytes <- 5
# options(shiny.maxRequestSize=megabytes*1024^2)

shinyServer(function(input, output, session) {
	
	# Helper function specifying hover tooltip format
	createTooltip <- function (hover) {
		if (hover$id %>% is.null) return(NULL)
		
		if(input$tooltip_vars %>% is.null ) { 
			
			paste0("<strong>", hover$id, "</strong>")
			
		} else {
			
			row <- Dataset()[ which(idColumn() == hover$id), ]
			
			paste0("<strong>", hover$id, "</strong>", "<br/>",
						 paste0("<strong>", input$tooltip_vars , ":</strong> ", 
						 			 row[, input$tooltip_vars], 
						 			 collapse = "</br>"))
			
		}
		
	}
	
	
	### Initializes dataset and variables in separate reactives
#		The first the app is run, these reactives take on the default values specified 
#		in global.R. To change the values, the user can import their own dataset. 

#			***********************************************************************
#			 These values change when the button is pressed, BEFORE conditions and 
#			 specifications are checked. If a user presses "Commit Dataset" while 
#			 an error message is displayed and tries to go back to the plot, 
#			 the app will crash.
#			***********************************************************************

	
	idColumn <- reactive({
		
		if (!input$commitdataset) {
			initialIdColumn
		} else if (input$identifier == "None") {
			paste(rep("Row", nrow(uploadedDataset())), seq(1,nrow(uploadedDataset())))
		} else {
			isolate(uploadedDataset()[, input$identifier ])
		}
		
	})
	
	Dataset <- reactive({
		
		if (!input$commitdataset) {
			initialDataset
		} else {
			isolate(uploadedDataset()[, input$vars ])
		}
		
	})
	
	datasetVariables <- reactive({
		
		if (!input$commitdataset) {
			initialVariables
		} else {
			isolate(input$vars)
		}
		
	})
	
	numericVariables <- reactive({
		if (!input$commitdataset) {
			initialNumericVariables
		} else {
			isolate(input$vars[ sapply(uploadedDataset()[,input$vars], class) == "numeric" |
														sapply(uploadedDataset()[,input$vars], class) == "integer"])
		}
	})
	
	
################################################################################
########################## MODEL CALCULATIONS ##################################
################################################################################
	
# 	The user inputs a response variable Y, a explanatory variable of interest X1,
# 	and a list of control variables {X2, X3, ...}. While the model equation shown
# 	below the plot is Y = b0 + b1X1 + b2X2 + b3X3 + ..., the actual model that is
# 	used to create the plot itself is Y - b2X2 - b2X3 - ... = b0 + b1X1 in order
# 	to preserve the locations of each point on the x-axis, lessening confusion.

#		The code in the adjusted.outcome() block below takes the data matrix [Y X1 X2
#		X3 ...] and multiplies it with the coefficient vector [1 0 -b2 -b3 ...],
#		giving us the left hand side, Y' = Y - b2X2 - b3X3 - ....
	

	# Creates model
	currentModel <- reactive({
		
		input$response %>% paste("~", input$predictor, 
														 paste(c("",input$controls), collapse=" + ")) %>%
			as.formula %>% lm(data = Dataset())
	})
	
	# Creates new values for the y axis (Response - Controls)
	adjusted.outcome <- reactive({
		
		# Create a vector of zeroes, one element for each variable. This will be the
		# base for the coefficient vector
		coefficientVector <- numeric( length(numericVariables()) ) %>%
			set_names( numericVariables() )
		
		# Creates the coefficient vector by reading coefficients from the model	
		#### 	TO DO: FIND SOME WAY TO VECTORIZE THIS STEP
		for( variable in input$controls %>% setdiff(input$response) ) {
			coefficientVector[[variable]] <- currentModel()$coefficients[[variable]]
		}
		
		# Leaves the response variable unchanged.
		coefficientVector[[input$response]] <- 1
		
		# Creates the data matrix
		dataMatrix <- Dataset() %>%
			extract(,numericVariables()) %>%
			inset(is.na(.), value = 0) %>%
			as.matrix
		
		# Multiplies the data matrix with the coefficient vector.
		dataMatrix %*% coefficientVector %>% drop
	})

################################################################################
############################# PLOTTING DATA ####################################
################################################################################
	
# This code uses ggvis to plot the adjusted.outcome() vector Y' against the
# explanatory variable X1. Unfortunately, due to limitations involving
# reactivity in ggvis, it is impossible to create axis labels which will

	# Specifies the data to be put into ggvis
	plottingData <- reactive ({
		data.frame(id = idColumn(),
							 x = Dataset()[[input$predictor]],
							 y = adjusted.outcome())
	}) 
	
	### Plotting function: this should run every time plottingData() changes
	plottingData %>%
		ggvis( x = ~x, y = ~y,
					 opacity := 0.5, opacity.hover := 0.9) %>%
		layer_points(key := ~id) %>%
		add_tooltip(createTooltip, "hover") %>%
		layer_model_predictions(model = "lm", formula = y~x) %>%
		
		# Makes ggvis's axis labels invisible -- we will add our own reactive labels
		add_axis("x", title = "", properties = axis_props(
			labels = list(opacity = 0),
			ticks = list(strokeOpacity = 0),
			title = list(fontSize = 12))) %>%
		add_axis("y", title = "", properties = axis_props(
			labels = list(opacity = 0),
			ticks = list(strokeOpacity = 0),
			title = list(fontSize = 12))) %>%
		
		bind_shiny("ggvis_plot")
	
	# Creates reactive axis labels, which are not possible with ggvis
	output$y_label <- renderText({
		paste0("<div class = 'axis-label' id='y-label'><div class='axis-label' id='y-label-container'>", input$response, "</div></div>")
	})
	output$x_label <- renderText({
		paste0("<div class='axis-label' id='x-label'>", input$predictor, "</div>")
	})
	

################################################################################
########################### MODEL INFORMATION ##################################
################################################################################

	# Renders output in the "Model summary" tab
	output$model.summary <- renderText({
		
		f <- currentModel() %>% summary %>% extract2("fstatistic")
		
		# Find overall p-value of model: do the combination of variables actually affect the response?
		p.value <- pf(f[1], f[2], f[3], lower.tail = FALSE) %>%	
			format(digits = 4) %>%
			ifelse( as.numeric(.) < 0.0001, "< 0.0001", . )
		
		# Find adjusted r-squared of model: how well does the model fit our data?
		r.squared <- currentModel() %>% summary %>% extract2("adj.r.squared") %>%
			format(digits = 4) %>%
			ifelse( (as.numeric(.)) < 0.0001, "< 0.0001", . )
		
		# Create a string containing the model formula. (Sorry about the mess. 
		# 	Basically all the HTML is just formatting it so that it looks nice.)
		formula <- paste("<span class='avoidwrap'><em><strong>", input$response, "</strong></em>= ",
										 paste( currentModel()$coeff %>% format(digits = 2, scientific = FALSE),
										 			 c("",
										 			 	sprintf("<em><strong> %s </em></strong></span>", c(input$predictor, input$controls) )), 
										 			 collapse = " + <span class='avoidwrap'>" ))
		
		# Formatting all the information together
		sprintf("<div class='formula'> %s </div>
						<table style='width:50%%'><tr>
						<td><span class='pval'>
						<p class='statlabel'>p-value</p> 
						<p class='statistic'> %s </p></span></td>
						<td><span class='rsq'>
						<p class='statlabel'>adjusted r<sup>2</sup></p> 
						<p class='statistic'> %s </p></span></td></tr></table>", 
						formula, p.value, r.squared)
		
	})
	
	# Renders output in the "Model calculations" tab
	output$model.output <- renderPrint({
		summary(currentModel())
	})


################################################################################
############################ RUNTIME OVERHEAD ##################################
################################################################################
	
	# Updates list of control variables to prevent user from checking selected
	# response/explanatory variables
	observeEvent( {input$response; input$predictor}, {
		updateCheckboxGroupInput(session, inputId = "controls",
														 choices = setdiff(numericVariables(), 
														 									c(input$response, input$predictor)),
														 selected = setdiff(input$controls, input$response),
														 inline = FALSE
		)
		
	})
	
	# Updates explanatory variable to prevent response and explanatory variables
	# from being the same
	observeEvent( input$response , {
		new.predictors <- setdiff(numericVariables(), input$response)
		
		updateSelectInput(session, inputId = "predictor",
											choices = new.predictors,
											selected = ifelse(input$response == input$predictor,
																				new.predictors[1],
																				input$predictor) )
		
	})
	
	# Updates Tooltip information based on currently selected variables -- helps
	# users with selecting tooltip information they might be interested in
	observeEvent( {input$response; input$predictor; input$controls}, {
		
		selected.inputs <- c(input$response, input$predictor, input$controls)
		added.input <- setdiff(selected.inputs, previousInputVars)
		removed.input <- setdiff(previousInputVars, selected.inputs)
		
		updateSelectInput(session, inputId = "tooltip_vars",
											selected = input$tooltip_vars %>% 
												setdiff(removed.input) %>% 
												union(added.input)
		) 
		# WONT UPDATE WHEN INPUT$CONTROLS BECOMES NULL
		
		previousInputVars <<- selected.inputs
		
	})
	
	# Creates the "Import Dataset" page. See import.R for code.
	source("import.R", local = TRUE)
	
})
