# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")
library(datasets)
options(shiny.trace=TRUE)

instanceNames = function(className) {
  names(which(sapply(.GlobalEnv, is, className )))
}

shinyServer(function(input, output) {
  output$ClassesOrObjects = reactiveText(function()
    if(is.null(input$ClassesOrObjects))
    "NULL" else input$ClassesOrObjects)
  specName = 
    reactiveText(function()input$specChoice)
  print(specName)
  output$specName = specName  
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$classes_table <- 
    reactiveTable(function() {
      theClasses = data.frame(c(input$specChoice,
                                subClassNames(
        input$specChoice)))
      names(theClasses) = "subClasses of " %&% input$specChoice
        # reactiveText(function()input$specChoice)
        #  reactiveText(function()input$specChoice) #fails
      cat("\n==theClasses==\n")
      print(str(theClasses))
      theClasses
    })
  output$objects_table <- 
    reactiveTable(function() {
      theObjects = data.frame(instanceNames(
                                  input$specChoice))
      names(theObjects) = "instance"
      theObjects$class = sapply(theObjects$instance,
                                FUN=function(obName)
                                  class(get(obName))
                                )
      cat("\n==theObjects==\n")
      print(str(theObjects))
      theObjects
    })
})