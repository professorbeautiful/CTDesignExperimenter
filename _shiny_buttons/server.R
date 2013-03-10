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
  
  
  output$actionChoice = renderText({input$viewChoice})
  
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
      return(input$viewChoice %&% ": \npick a " %&% 
               shortName(input$specChoiceOneCT  %&%
                           " \n(class="    %&%
                           input$specChoiceOneCT %&% ")" ))
    }
    else return(input$viewChoice %&% ": not yet implemented")
  }
  #  debug(f.mainPanelHeader)
  output$mainPanelHeader = renderText({f.mainPanelHeader()})
  
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$classes_table <- 
    renderTable( {
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
    renderTable( {
      nrow(output$classes_table)
    })
  createObjectsTable = function() {
    theSpecChoice = switch(input$viewChoice,
                           `View spec objects`=input$specChoiceModels,
                           `Define one clinical trial`= input$specChoiceOneCT
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
  output$objects_table <- renderTable({createObjectsTable()})
  output$objects_table_1 <- renderTable({createObjectsTable()})
  output$objects_table_2 <- renderTable({createObjectsTable()})
  
  #   f.specChoiceOneCT.save = reactive(function() {
  #     catn("f.specChoiceOneCT.save: saving ", input$specChoiceOneCT)
  #     specChoiceOneCT.saved[1] <<- input$specChoiceOneCT
  #   })
  #   
  #   specChoiceOneCT.saved <<- NA
  
  #   f.specChoiceOneCTCleaned = function() { 
  #     catn("      f.specChoiceOneCTCleaned: cleaning ", f.specChoiceOneCTCleaned())
  #     if(is.null(f.specChoiceOneCTCleaned()))
  #       return("")
  #     oneCTclassName = ifelse(is.na(f.specChoiceOneCT.save()), "OutcomeModelSpecifier", f.specChoiceOneCT.save())
  #     #oneCTclassName = f.specChoiceOneCT.save()
  #     if(regexpr("\\[", oneCTclassName) > 0)  ### Remove extra characters
  #       oneCTclassName = substring(oneCTclassName, 1, regexpr("\\[", oneCTclassName) - 2) 
  #     oneCTclassName
  #   }
  #debug(f.specChoiceOneCTCleaned)  ##f.specChoiceOneCTCleaned   #### See the help file.
  
  #   f.buildingModelIndices = function() {
  #     catn("Changing the model index for ", isolate(f.specChoiceOneCTCleaned()),
  #          ":  buildingModelIndices=\n", 
  #          paste(names(buildingModelIndices), buildingModelIndices, sep="=", collapse=", "))
  #     if(!is.null(isolate(f.specChoiceOneCTCleaned())) & !is.null(input$model_row_num)
  #        & (input$model_row_num > 0))
  #       buildingModelIndices[(isolate(f.specChoiceOneCTCleaned()))] <<- input$model_row_num ## From the box.
  #     ## single or double headed assignment?
  #     ## The purpose of "isolate" here is ???
  #     catn("f.buildingModelIndices: exit:", paste(names(buildingModelIndices), buildingModelIndices, sep="=", collapse=", "))
  #     buildingModelIndices
  #   }
  #   #  debug(f.buildingModelIndices)
  #   reactive(f.buildingModelIndices)
  
  f.buildingModelMain = function() {
    theTableOutput = tableOutput(outputId="objects_table_2")
    ## This should set the value of output$objects_table_2, for use in the text.
    list( HTML("Select model object (by number) <br>to build simulation.")
          #           " In this box, type (or arrow to) the row  number for your object.",
          #           numericInput("model_row_num", "model row num",
          #                        buildingModelIndices[f.specChoiceOneCTCleaned()], 
          #                        min=1, max=length(instanceNames(f.specChoiceOneCTCleaned())))
          ,  theTableOutput  )
  }
  output$buildingModelMain = renderUI({f.buildingModelMain()})
  
  f.isModelFinished = function() {
    updateModelIndices()
    #cat("length(values): ", length(values), "\n")
    rowValues = c(values$PopRow, values$OutcomeRow, values$DesignRow)
    result = try(
        ! is.null(rowValues)
      & ! any(sapply(rowValues, is.na))
      & ! any(rowValues < 1)
    ,  silent=TRUE)
    #cat("result: ", result, "\n")
    if((length(result) == 0)
       | (is.na(result)) 
       | class(result)=="try-error"
     )
      result = FALSE
    values$isModelFinished = result
    result
  }
  
  output$isModelFinished = reactive(f.isModelFinished)
  
  output$currentModelText = renderText({
    paste(specClassNamesForSim1CT, 
          "[", c(values$PopRow,values$OutcomeRow,values$DesignRow), "]", 
          collapse="\n")
  })
  
  f.buildingModelSide = function() {
    components = list(
      radioButtons("specChoiceOneCT", "choose spec type", 
                   specClassNamesForSim1CT),
      numericInput(inputId="PopRow", label="Pop model", value=NA, min=1, max=4, step=1),
      numericInput(inputId="OutcomeRow", label="Outcome model", value=NA, min=1, max=4, step=1),
      numericInput(inputId="DesignRow", label="Design", value=NA, min=1, max=4, step=1),
      textOutput(outputId="currentModelText")
    )
    return(components)
  }
  #  debug(f.buildingModelSide)
  output$buildingModelSide = renderUI({f.buildingModelSide()})
  
  f.sim1CTbutton = function() {
    #      conditionalPanel(condition="input.isModelFinished"
    list(tags$button( type="button",
                  style="color: " %&% ifelse(f.isModelFinished(), " green", " red"),
                  onclick="sim1CT()",
                  ("Simulate one CT" %&% ifelse(f.isModelFinished(), " READY!", " (not ready)")))
    , tag("script ", 
          "function sim1CT() {alert(\"Not yet ready!\");}")
    )
  }
  
  f.buttonColor = reactive({
    ifelse(f.isModelFinished(), " green", " red")
  })
  
  f.buttonLabel = reactive({
    "Simulate one CT" %&% ifelse(f.isModelFinished(), " READY!", " (not ready)")
  })
  output$sim1CTbutton = renderUI({
    actionButton("sim1CTbutton", HTML("<div 
                 style='color: " %&% f.buttonColor() %&% "'> " %&% f.buttonLabel()
                 %&% "</div>")
    )
  })
  
  reactive({
    cat("Value of sim1CTbutton is ", input$sim1CTbutton)
  })
  
  values = reactiveValues()
  ## values  is an S3 class "reactivevalues". Put this in the eval box:
    #   methods(class="reactivevalues")
    #  and this is what you get:
    # [.reactivevalues [[.reactivevalues [[<-.reactivevalues [<-.reactivevalues $.reactivevalues $<-.reactivevalues as.list.reactivevalues names.reactivevalues names<-.reactivevalues

  updateModelIndices = reactive({
    values$PopRow <- input$PopRow
    values$OutcomeRow <- input$OutcomeRow
    values$DesignRow <- input$DesignRow
  })
  
  #   output$objects_table_nrows <- 
  #     renderTable(function() {
  #       nrow(output$objects_table)
  #     })
  #   output$CurrentCT <- 
  #     renderTable(function() {
  #       specChoiceObjects      model_row_num
  #       nrow(output$objects_table)
  #     })
  #   
  
  output$evalOutput = renderText({
    if(input$evalToggle) eval(parse(text=isolate(input$evalString)))
    })
  
  output$headerOutput = renderUI({
    #    addAttr("html", TRUE,
    list(HTML("<head>\n  <title>Clinical Trial Experiment Platform</title>
            </head>\n<div class=\"span12\" style=\"padding: 10px 0px; color:rgb(fff,300,400)\">
            <h2  > <i>Clinical Trial Experiment Platform </i> </h2>\n</div>")
         , tags$button(type="button",
                       style="color: blue",
                       onclick="\"toggleDebug()\"",
                       "Toggle debugging")
         , 
         checkboxInput(inputId="evalToggle", "evalToggle", value=FALSE)
         , textInput(inputId="evalString", label="=>", value="1+1")
         , textOutput(outputId="evalOutput")
         , tag("script ", 
               "function toggleDebug() {alert(\"Not yet ready!\");}")                             
         #headerPanel(title="Clinical Trial Experiment Platform", windowTitle="Clinical Trial Experiment Platform")
    )
  })
})
