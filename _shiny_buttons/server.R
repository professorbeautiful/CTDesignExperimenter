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
    if(is.null(input$xClassesOrObjects))
    "NULL" else input$xClassesOrObjects)
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
      theClasses$slotNames = sapply(theClasses[[1]], slotNames)
      theClasses$slotTypes = sapply(theClasses[[1]], getSlots)
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
      theObjects$requirements = sapply(theObjects[[1]], function(theObject)
        paste(getRequirements(get(theObject)), collapse=","))
      theObjects$provisions = sapply(theObjects[[1]], function(theObject)
        paste(getProvisions(get(theObject)), collapse=","))
      cat("\n==theObjects==\n")
      print(str(theObjects))
      theObjects
    })
})