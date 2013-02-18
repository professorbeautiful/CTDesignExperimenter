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

specClassNamesForSim1CT = specClassNames %except% "BaseCharModelSpecifier" %except% "EvalSpecifier"  ### not the "nice" names

instanceNames = function(className) {
  names(which(sapply(.GlobalEnv, is, className )))
}

shinyServer(function(input, output) {

  ### Called just once!
  buildingModelIndices = rep(NA, 3)
  names(buildingModelIndices) = specClassNamesForSim1CT
  
  output$actionChoice = reactiveText(function()
    input$viewChoice)

  f.mainPanelHeader = function() { 
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
      else if(input$viewChoice == "Define one clinical trial") { 
#         if(is.na(buildingModelIndices[f.specChoiceOneCTCleaned()]))  
#           buildingModelIndices[f.specChoiceOneCTCleaned()] == 1
              ### do we want this? maybe the user wants to leave it undecided?
        return(input$viewChoice %&% ": <br>pick a" %&% 
                 shortName(f.specChoiceOneCTCleaned())  %&%
                 " (class="    %&%
                 f.specChoiceOneCTCleaned() %&% ")" )
      }
      else return(input$viewChoice %&% ": not yet implemented")
  }
#  debug(f.mainPanelHeader)
  output$mainPanelHeader = reactiveText(f.mainPanelHeader)
  
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$classes_table <- 
    reactiveTable(function() {
    #  cat("\n==specChoiceClasses Class==\n")
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
                 slotNames = try(slotNames(getClass(theClass)), silent=TRUE)
                 if(class(slotNames) == "try-error" | is.null(slotNames)) return ("")
                 return(paste(slotNames, collapse="\n"))}
        )
      theClasses$slotTypes = 
        sapply(theClasses[[1]], 
               FUN=function(theClass){
                 slots = try(getSlots(getClass(theClass)), silent=TRUE)
                 if(class(slots) == "try-error" | is.null(slots)) return ("")
                 return(paste(slots, collapse="\n"))}
        )
#      cat("\n==theClasses==\n")
#      print(str(theClasses))
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
               req = try(getRequirements(get(theObject)), silent=TRUE)
               if(class(req) == "try-error") return ("")
               return(paste(req, collapse="\n"))}
      )
    theObjects$provisions = 
      sapply(theObjects[[1]], 
             FUN=function(theObject){
               prov = try(getProvisions(get(theObject)), silent=TRUE)
               if(class(prov) == "try-error") return ("")
               return(paste(prov, collapse="\n"))}
      )
#    cat("\n==theObjects==\n")
#    print(str(theObjects))
    theObjects
  }
#  debug(createObjectsTable)
  output$objects_table <- reactiveTable(createObjectsTable)
  output$objects_table_1 <- reactiveTable(createObjectsTable)
  output$objects_table_2 <- reactiveTable(createObjectsTable)

  f.specChoiceOneCTCleaned = function() { 
    if(is.null(input$specChoiceOneCT))
      return("")
    oneCTclassName = ifelse(is.na(input$specChoiceOneCT), "PopModelSpecification", input$specChoiceOneCT)
    if(regexpr("\\[", oneCTclassName) > 0)  ### Remove extra characters
      oneCTclassName = substring(oneCTclassName, 1, regexpr("\\[", oneCTclassName) - 2) 
    oneCTclassName
  }
  #debug(f.specChoiceOneCTCleaned)
  reactive(f.specChoiceOneCTCleaned)   #### See the help file.
    
  f.buildingModelIndices = function() {
    catn("Changing the model index for ", isolate(f.specChoiceOneCTCleaned()),
         ":  buildingModelIndices=\n", 
         paste(names(buildingModelIndices), buildingModelIndices, sep="=", collapse=", "))
    if(!is.null(isolate(f.specChoiceOneCTCleaned())) & !is.null(input$model_row_num))
      buildingModelIndices[(isolate(f.specChoiceOneCTCleaned()))] <<- input$model_row_num ## From the box.
    ## single or double headed assignment?
    ## The purpose of "isolate" here is ???
    catn("f.buildingModelIndices: exit:", paste(names(buildingModelIndices), buildingModelIndices, sep="=", collapse=", "))
    buildingModelIndices
  }
#  debug(f.buildingModelIndices)
  reactive(f.buildingModelIndices)

  f.buildingModelMain = function() {
    theTableOutput = tableOutput(outputId="objects_table_2")
    ## This should set the value of output$objects_table_2, for use in the text.
    list( HTML("Select model object (by number) <br>to build simulation."),
         " In this box, type (or arrow to) the row  number for your object.",
            numericInput("model_row_num", "model row num",
                         buildingModelIndices[f.specChoiceOneCTCleaned()], 
                         min=1, max=length(instanceNames(f.specChoiceOneCTCleaned())))
    ,  theTableOutput  )
  }
  output$buildingModelMain = reactiveUI(f.buildingModelMain)
  
  f.isModelFinished = function() {
    all(!is.na(buildingModelIndices))
  }
  reactiveUI(f.isModelFinished)
  
  f.buildingModelSide = function() {
    radioButtonLabels = paste(
      specClassNamesForSim1CT,
      "[", as.character(f.buildingModelIndices()), "]")
      radioButtons("specChoiceOneCT", "One CT component type:", 
                   radioButtonLabels)
#      textOutput("CurrentCTasText"),
  theRadioButtons = radioButtons("specChoiceOneCT", "One CT component type:", 
             radioButtonLabels)
  if(f.isModelFinished()) return(
      list(theRadioButtons
        , tags$button(type="button",
                  style="color: red",
                  onclick='sim1CT()',
                  ("Simulate one CT"))
        , tag("script ", 
                      "function sim1CT() {alert(\"Not yet ready!\");}")
      ))
  else  return(theRadioButtons)
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