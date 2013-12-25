###  server.R
###  shiny interface for defining a variable
require(RBioinf)
require(RJSONIO)
require(shiny)
require("shinyIncubator")



shinyServer_variables = function(input, output, session) {
  print("Entered shinyServer_variables")
#  output$varNameOutput = renderText(input$varNameInput)
#  reactive(f.varSearchButtonOutput())
  output$varNames  = 
  observe({
    x = input$varNameInput
    updateTextInput(session, "varNameOutput",
                    label="newlabel",
                    value="new value")
  })
}

f.varSearchButtonOutput = function(){
  print("Value of varSearchButton is " %&% 
          input$varSearchButton %&%
          "textbox says ", input$varNameInput %&% "\n")
  ## make it reactive to the button.
}
  
shinyServer(shinyServer_variables)
