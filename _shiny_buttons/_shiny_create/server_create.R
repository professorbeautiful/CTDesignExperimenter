# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")
# library(datasets)
require(RBioinf)

options(shiny.trace=TRUE)

specClassNames = c(`patient attributes`="BaseCharModelSpecifier",
                   `population models`="PopModelSpecifier",
                   `outcome models`="OutcomeModelSpecifier",
                   designs="DesignSpecifier",
                   `evaluation criteria`="EvalSpecifier")
# buildingModelIndices = rep(NA, 4)
# names(buildingModelIndices) = names(specClassNames)

shortName = function(specifierName)
  names(specClassNames)[match(specifierName, specClassNames)]

instanceNames = function(className) {
  names(which(sapply(.GlobalEnv, is, className )))
}

shinyServer(function(input, output) {
  output$actionChoice = reactiveText(function()
    input$viewChoice)
#  specName = reactiveText(function()input$specChoice)
  output$mainPanelHeader = reactiveText(
    function() { 
      if(input$viewChoice == "View spec classes")
        return(input$viewChoice %&% " for  " %&% 
                 shortName(input$specChoiceClasses)  %&% 
                 " (class="    %&%
                 input$specChoiceClasses %&% ")")
      else if(input$viewChoice == "View spec objects")
        return(input$viewChoice %&% " for  " %&% 
                 shortName(input$specChoiceModels)  %&% 
                 " (class="    %&%
                 input$specChoiceModels %&% ")")
      else if(input$viewChoice == "Define one clinical trial")
        return(input$viewChoice %&% ": pick an object of type " %&% 
                 shortName(input$specChoiceModels)  %&%
                 " (class="    %&%
                 input$specChoiceModels %&% ")" %&%
                 " by typing object number in this box")
      else input$viewChoice %&% ": not yet implemented"
    })
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$classes_table <- 
    reactiveTable(function() {
      cat("\n==specChoiceClasses Class==\n")
      theClasses = data.frame(c(input$specChoiceClasses,
                                subClassNames(
                                  input$specChoiceClasses)))
      names(theClasses) = "subClasses of " %&% input$specChoiceClasses
      rownames(theClasses) = NULL
#       radioButtons = sapply(1:nrow(theClasses),
#                             function(rownum)
#                               HTML("<input type=\"radio\" name=\"chooseOneClass\" 
#         value=" %&% rownum %&% ">"))
#       theClasses$O = radioButtons 
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
      #cat("radio value for chooseOneClass = ", input$chooseOneClass,"\n")
      theClasses
    })
  output$classes_table_nrows <- 
    reactiveTable(function() {
      nrow(output$classes_table)
    })
  createObjectsTable = function() {
    theSpecChoice = switch(input$viewChoice,
                           `View spec objects`=input$specChoiceModels,
                           `Define one clinical trial`=input$specChoiceOneCT
                           )    #### TODO-- handle NULL.""trying to get slot \"className\" from an object of a basic class (\"NULL\") with no slots","
    if(is.null(theSpecChoice)) theSpecChoice = "PopModelSpecifier"
    if(regexpr("\\[", theSpecChoice) > 0)  ### Remove extra characters
      theSpecChoice = substring(theSpecChoice, 1, regexpr("\\[", theSpecChoice) - 2)
    theObjects = data.frame(instanceNames(theSpecChoice))
    names(theObjects) = "instance"
    theObjects$class = 
      sapply(theObjects$instance,
             FUN=function(obName) class(get(obName)))
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
  }
#  debug(createObjectsTable)
  output$objects_table <- reactiveTable(createObjectsTable)
  output$objects_table_1 <- reactiveTable(createObjectsTable)
  output$objects_table_2 <- reactiveTable(createObjectsTable)

#   output$buildingModelIndices = reactiveUI(function() {
#     cat("buildingModelIndices=", buildingModelIndices)
#     if(!is.null(input$specChoiceModels) & !is.null(input$model_row_num))
#       buildingModelIndices[isolate(print(input$specChoiceModels))] <<- input$model_row_num
#     ## The purpose of "isolate" here is to prevent 
#     buildingModelIndices
#   })
  
  f.buildingModelMain = function() {
    list(      numericInput("model_row_num", "model row num",
                            "1", min=1, 10)# max=nrow(output$objects_table_2))
    ,    tableOutput(outputId="objects_table_2"))
  }
  output$buildingModelMain = reactiveUI(f.buildingModelMain)
  
  f.buildingModelSide = function() {
    radioButtonLabels = paste(
      specClassNames %except% "BaseCharModelSpecifier",
      "[", "index goes here", "]")
#    if(is.na(nrow_table)) nrow_table=10
    return(list(
      radioButtons("specChoiceOneCT", "One CT component type:", 
                   radioButtonLabels)
      #      textOutput("CurrentCTasText"),
    ))
  }
#  debug(f.buildingModelSide)
  output$buildingModelSide = reactiveUI(f.buildingModelSide)
  
  #   output$objects_table_nrows <- 
  #     reactiveTable(function() {
  #       nrow(output$objects_table)
  #     })
  #   output$CurrentCT <- 
  #     reactiveTable(function() {
  #       specChoiceObjects      model_row_num
  #       nrow(output$objects_table)
  #     })
  #   
})