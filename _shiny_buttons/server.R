# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")
library(datasets)
options(shiny.trace=FALSE)

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
      cat("\n==specChoice Class==\n")
      theClasses = data.frame(c(input$specChoice,
                                subClassNames(
        input$specChoice)))
      names(theClasses) = "subClasses of " %&% input$specChoice
        #  reactiveText(function()input$specChoice) #fails
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