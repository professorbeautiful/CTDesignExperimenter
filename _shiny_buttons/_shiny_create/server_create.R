# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")
# library(datasets)
require(RBioinf)

options(shiny.trace=FALSE)


specClassNames = c(`patient attributes`="BaseCharModelSpecifier",
                   `population models`="PopModelSpecifier",
                   `outcome models`="OutcomeModelSpecifier",
                   designs="DesignSpecifier",
                   `evaluation criteria`="EvalSpecifier")

shortName = function(specifierName)
  names(specClassNames)[match(specifierName, specClassNames)]

instanceNames = function(className) {
  names(which(sapply(.GlobalEnv, is, className )))
}

shinyServer(function(input, output) {
  output$actionChoice = reactiveText(function()
    input$viewChoice)
  specName = reactiveText(function()input$specChoice)
  print(specName)
  output$specName = specName  
  output$mainPanelHeader = reactiveText(
    function() { if(input$viewChoice 
                  %in% c("View spec classes", "View spec objects"))
      input$viewChoice %&% "for  " %&% 
                   shortName(input$specChoice)  %&%
                   " (class="    %&%
                   input$specChoice %&% ")"
    else input$viewChoice %&% ": not yet implemented"
  })
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$classes_table <- 
    reactiveTable(function() {
      cat("\n==specChoice Class==\n")
      theClasses = data.frame(c(input$specChoice,
                                subClassNames(
                                  input$specChoice)))
      names(theClasses) = "subClasses of " %&% input$specChoice
      rownames(theClasses) = 
        "<input type=\"radio\" name=\"chooseOneClass\" 
        value=" %&% 1:nrow(theClasses) %&% ">"
      theClasses$slotNames = 
        sapply(theClasses[[1]], 
               FUN=function(theClass){
                 slotNames = try(slotNames(getClass(theClass)))
                 if(class(slotNames) == "try-error" | is.null(slotNames)) return ("")
                 return(paste(slotNames, collapse="\n"))}
        )
      theClasses$slotTypes = 
        sapply(theClasses[[1]], 
               FUN=function(theClass){
                 slots = try(getSlots(getClass(theClass)))
                 if(class(slots) == "try-error" | is.null(slots)) return ("")
                 return(paste(slots, collapse="\n"))}
        )
      cat("\n==theClasses==\n")
      print(str(theClasses))
      cat("radio value for chooseOneClass = ", input$chooseOneClass,"\n")
      theClasses
    })
  output$objects_table <- 
    reactiveTable(function() {
      theObjects = data.frame(instanceNames(
                                  input$specChoice))
      names(theObjects) = "instance"
      theObjects$class = 
        sapply(theObjects$instance,
                                FUN=function(obName)
                                  class(get(obName))
                                )
      theObjects$requirements = 
        sapply(theObjects[[1]], 
               FUN=function(theObject){
                 req = try(getRequirements(get(theObject)))
                 if(class(req) == "try-error") return ("")
                 return(paste(req, collapse="\n"))}
        )
      theObjects$provisions = 
        sapply(theObjects[[1]], 
               FUN=function(theObject){
                 prov = try(getProvisions(get(theObject)))
                 if(class(prov) == "try-error") return ("")
                 return(paste(prov, collapse="\n"))}
        )
      cat("\n==theObjects==\n")
      print(str(theObjects))
      theObjects
    })
})