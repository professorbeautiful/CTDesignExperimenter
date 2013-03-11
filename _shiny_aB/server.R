library("shiny")

options(shiny.trace=TRUE)

shinyServer(function(input, output) {

#output$aBtext = "initial value"

	output$aBtext <- reactive( {
	  # Don't do anything until after the first button push.
	  if (input$aB == 0)
	    value = "uninitialized"
    else
	    value = as.character(input$aB)
	 # output$aBtext = value
    # print("output$aBtext = " %&% output$aBtext)
	  return(value)
	})
})
