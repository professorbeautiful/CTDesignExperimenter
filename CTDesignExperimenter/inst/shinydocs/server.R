## NOTE:  sometimes jQuery fails to load.  Just try again with runApp().

# install.packages('shiny')
# install.packages('RJSONIO')
#library("shiny")
#library("shinyIncubator")
# library(datasets)
#require(RBioinf)
#require(RJSONIO)
####  Using Imports in DESCRIPTION, these should not be required.

source("fixHtmlEscape.R")

options(shiny.trace=TRUE)

#if(length(ls(pattern="CRMSpec1")) == 0 )  RELOAD_CATALOG()

specClassNames = c(`Variables`="Variable",
                   `patient attributes`="VariableGenerator",
                   `population models`="PopulationModel",
                   `outcome models`="OutcomeModelSpecifier",
                   designs="DesignSpecifier",
                   `evaluation criteria`="EvalSpecifier")

shortName = function(specifierName)
  names(specClassNames)[match(specifierName, specClassNames)]
#debug(shortName)
specClassNamesForSim1CT = setdiff(specClassNames, c("BaseCharModelSpecifier", "EvalSpecifier"))  ### not the "nice" names


f.mainPanelHeader = function() { 
  # Error in f.mainPanelHeader() : object 'input' not found
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
    return(HTML("<strong>Define one clinical trial:</strong> <br>pick  population model, outcome model, and design."))
  }
  else if(input$viewChoice == "Run one clinical trial") { 
    #         if(is.na(buildingModelIndices[f.specChoiceOneCTCleaned()]))  
    #           buildingModelIndices[f.specChoiceOneCTCleaned()] == 1
    ### do we want this? maybe the user wants to leave it undecided?
    return("Run one clinical trial: results appear here.\n" %&% 
             h5(em(uiOutput(outputId="currentModelText_copy"))))
  }
  else return(input$viewChoice %&% ": not yet implemented")
}
# debug(f.mainPanelHeader)


myShinyServerFunction = function(input, output) {
  print("GOT THIS FAR: myShinyServerFunction 1")
  output$actionChoice = renderText({input$viewChoice})
  print("GOT THIS FAR: myShinyServerFunction 2")
  
  output$mainPanelHeader = renderText({f.mainPanelHeader()})
  
  classNames = c("BaseCharModelSpecifier","PopModelSpecifier","OutcomeModelSpecifier","DesignSpecifier","EvalSpecifier")
  output$classes_table <- 
    renderTable( {
      #  cat("\n==specChoiceClasses Class==\n")
      theClasses = data.frame(stringsAsFactors=FALSE,
                              c(input$specChoiceClasses,
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
  
  
  objects_tables = list(
    PopModelSpecifier=createObjectsTable("PopModelSpecifier"),
    OutcomeModelSpecifier=createObjectsTable("OutcomeModelSpecifier"),
    DesignSpecifier=createObjectsTable("DesignSpecifier"))
  
  
  #  debug(createObjectsTable)
  output$objects_table <- renderTable({createObjectsTable()})
  output$objects_table_1 <- renderTable({createObjectsTable()})
  output$objects_table_2 <- renderTable({createObjectsTable()})
  
  #source("tableForCreateOneCT.R")# doesn't work inside a function body?
  
  f.objects_table_for_OneCT = function(){
    f.changeSelectedRow()
    df = createObjectsTable()
    selectedRow = switch(input$specChoiceOneCT,
                         PopModelSpecifier=values$PopRow,
                         OutcomeModelSpecifier=values$OutcomeRow,
                         DesignSpecifier=values$DesignRow
    )
    header_html <- function(table_cell) paste0('<th>', table_cell, '</th>')
    cell_html <- function(table_cell) paste0('<td>', table_cell, '</td>')
    radio_html <- function(radio_name, radio_value, is_checked, radio_text) {
      paste0('<input type="radio" name="', 
             radio_name, '" value=', radio_value, 
                   ifelse(is_checked, " checked ", ""),
                   '>', radio_text)
    }    
    row_html <- function(table_row_num) {
      table_row = df[table_row_num, ]
      cells <- sapply(table_row, cell_html)
      cells <- c(cell_html(radio_html("whichRow_" %&% input$specChoiceOneCT, 
                                      table_row_num, table_row_num == selectedRow, "")), cells)
      collapse_cells <- paste0(cells, collapse='')
      selectedRowStyle = "style='color:red; font-weight:bold'"
      collapse_cells <- paste0('<tr ', 
                               ifelse(table_row_num == selectedRow, selectedRowStyle, ""),
                               '>', collapse_cells, '</tr>')
      collapse_cells 
    }
    df_rows <- sapply(1:nrow(df), row_html) 
    df_header_row <- header_html(c("CHOICE", names(df)))
    collapse_cells <- paste0(c(df_header_row, df_rows), collapse='')    
    full_table <- paste0('<table class=\"data table table-bordered table-condensed\">', 
                         collapse_cells, '</table>')
    return(full_table)
  }
  
  output$objects_table_for_OneCT = renderText({f.objects_table_for_OneCT()})
  
  f.changeSelectedRow = reactive({
    catn("In f.changeSelectedRow (enter); values are  ", values$PopRow, values$OutcomeRow, values$OutcomeRow)
    catn(input$specChoiceOneCT)
    ###  These values should work.
    if(is.null(values$PopRow)) values$PopRow = which(objects_tables[["PopModelSpecifier"]] == "doseThresholdPopModelSpec")
    if(is.null(values$OutcomeRow)) values$OutcomeRow = which(objects_tables[["OutcomeModelSpecifier"]] == "toxDoseThresholdOutcomeModel")
    if(is.null(values$DesignRow)) values$OutcomeRow = which(objects_tables[["DesignSpecifier"]] == "crm9")
    catn("whichRow_ values: ", paste(
      input$whichRow_PopModelSpecifier, input$whichRow_OutcomeModelSpecifier, input$whichRow_DesignSpecifier, sep=",")) 
    whichRow = input[["whichRow_" %&% input$specChoiceOneCT]]
    if(!is.null(whichRow)) 
      switch(input$specChoiceOneCT,
           PopModelSpecifier=
             if(whichRow != values$PopRow) values$PopRow = as.numeric(whichRow),
           OutcomeModelSpecifier=
             if(whichRow != values$OutcomeRow) values$OutcomeRow = as.numeric(whichRow),
           DesignSpecifier=
             if(whichRow != values$DesignRow) values$DesignRow = as.numeric(whichRow)
    )
  })
  
  #debug(f.changeSelectedRow)
  
  #   highlightObjectsTable = reactive({
  #         
  #   })
  
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
#   debug(f.specChoiceOneCTCleaned)  
  
  f.buildingModelMain = function() {
    #     %&% 
    #       shortName(input$specChoiceOneCT)  %&%
    #       " \n(class="    %&%
    #       input$specChoiceOneCT %&% ")" )
    theTableOutput = tableOutput(outputId="objects_table_for_OneCT")
    ## "<div id=\"objects_table_2\" class=\"shiny-html-output\"></div>"
    # print(theTableOutput)
    ## This should set the value of output$objects_table_2, for use in the text.
    list( uiOutput(outputId="currentModelText")
          , HTML("<DIV style='color:blue'> Select "
            %&% input$specChoiceOneCT %&% " object to build simulation.</DIV>")
          ###  THIS HTML LINE WORKED!!!
          #           " In this box, type (or arrow to) the row  number for your object.",
          #           numericInput("model_row_num", "model row num",
          #                        buildingModelIndices[f.specChoiceOneCTCleaned()], 
          #                        min=1, max=length(instanceNames(f.specChoiceOneCTCleaned())))
          ,  theTableOutput  )
  }
  #debug(f.buildingModelMain)
  output$buildingModelMain = renderUI({f.buildingModelMain()})
  
  f.isModelFinished = reactive( {
    #updateModelIndices()
    #cat("length(values): ", length(values), "\n") ### always == 1 !!!
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
    values$isModelFinished = result  ## removing this doesnt help LOOP PROBLEM
    result
  })
  
#  output$isModelFinished = reactive({f.isModelFinished()})
  
  output$currentModelText = output$currentModelText_copy = renderText({
    specLine = function(specName, rowNum) strong(specName)  %&% " = " %&%
      ifelse(is.null(rowNum) | is.na(rowNum), "(not chosen)", 
             objects_tables[[specName]][rowNum, "instance"]) ;
    line = specLine("PopModelSpecifier", values$PopRow) %&% "<br>" %&%
      specLine("OutcomeModelSpecifier", values$OutcomeRow) %&% "<br>" %&%
      specLine("DesignSpecifier", values$DesignRow) 
    attr(line, "html") = TRUE
    #print(line)
    HTML(line)
  })
  
  f.componentsForBuildingModel = function() {
    components = list()
      #      textOutput(outputId="currentModelText"),
    components[[1]] = radioButtons("specChoiceOneCT", "choose spec type",
                   specClassNamesForSim1CT)
    components[[2]] = checkboxInput(inputId="toggleDetailTable", "show/hide details", value=FALSE)
#     components[[3]] = conditionalPanel(condition="input.specChoiceOneCT == \"PopModelSpecifier\"",
#                                     numericInput(inputId="PopRow", label="Pop model", 
#                                                  value=values$PopRow, min=1, 
#                                                  max=nrow(objects_tables[["PopModelSpecifier"]]), step=1))
#     components[[4]] = conditionalPanel(condition="input.specChoiceOneCT == \"OutcomeModelSpecifier\"",
#                                     numericInput(inputId="OutcomeRow", label="Outcome model", 
#                                                  value=values$OutcomeRow, min=1, 
#                                                  max=nrow(objects_tables[["OutcomeModelSpecifier"]]), step=1))
#     components[[5]] = conditionalPanel(condition="input.specChoiceOneCT == \"DesignSpecifier\"",
#                                     numericInput(inputId="DesignRow", label="Design", 
#                                                  value=NA, min=1, 
#                                                  max=nrow(objects_tables[["DesignSpecifier"]]), step=1))
# # here is the cause of the error message  
    #Error in as.character(value) : 
    #cannot coerce type 'closure' to vector of type 'character'
    #     components[[6]] = conditionalPanel(condition="input.toggleDetailTable",
#                                     renderTable(objects_tables[[input$specChoiceOneCT]]))
    #print("f.componentsForBuildingModel: length(components) = " %&% length(components))
    return(components)
  }
  #debug(f.componentsForBuildingModel)
  output$componentsForBuildingModel = renderUI({f.componentsForBuildingModel()})
  
  #   f.sim1CTbutton = function() {
  #     #      conditionalPanel(condition="input.isModelFinished"
  #     list(tags$button( type="button",
  #                   style="color: " %&% ifelse(f.isModelFinished(), " green", " red"),
  #                   onclick="sim1CT()",
  #                   ("Simulate one CT" %&% ifelse(f.isModelFinished(), " READY!", " (not ready)")))
  #     , tag("script ", 
  #           "function sim1CT() {alert(\"Not yet ready!\");}")
  #     )
  #   }  ### No longer used.
  
  f.buttonColor = reactive({
    ifelse(f.isModelFinished(), " green", " red")
  })
  
  f.buttonLabel = reactive({
    "Simulate one CT" %&% ifelse(f.isModelFinished(), " READY!", " (not ready)")
  })
  
  f.sim1CTbuttonUI = function(){
    catn("Here in f.sim1CTbuttonUI")# %&% input.toggleDetailTable);
    #catn("Input is ", str(input))
    #input$PopRow ## <== this does NOT cause the infinite looping.
#    print(values$PopRow) ## <== this does NOT cause the infinite looping.
    # but it generates error message:
    #   Error in tag$name : $ operator is invalid for atomic vectors
    #f.isModelFinished() ## <== this DOES  cause the infinite looping
    #updateModelIndices
    #     f.buttonColor()  ## <== this causes the infinite looping.
#    "Here in output$sim1CTbuttonUI"
    ## Commenting out the following does NOT remove the problem of the repeated
    #==> Shiny URLs starting with /actionbutton will mapped to /Users/Roger/Library/R/2.15/library/shinyIncubator/actionbutton
#    actionButton("sim1CTbutton", HTML("<div style='color:blue'> sim1CT </div>"))
    actionButton("sim1CTbutton", HTML("<div 
                   style='color: " %&% f.buttonColor() %&% "'> " %&% f.buttonLabel()
                                       %&% "</div>")
    )
  }
 # debug(f.sim1CTbuttonUI)
  output$sim1CTbuttonUI = renderUI({f.sim1CTbuttonUI()})
  
  f.sim1CTbuttonOutput = function(){
    print("Value of sim1CTbutton is " %&% input$sim1CTbutton %&% "\n")  ## make it reactive to the button.
    if(isolate(f.isModelFinished())) {
      designSpecName        = objects_tables[["DesignSpecifier"]]$instance[isolate(values$DesignRow)]
      outcomeModelSpecName  = objects_tables[["OutcomeModelSpecifier"]]$instance[isolate(values$OutcomeRow)]
      popModelSpecName      = objects_tables[["PopModelSpecifier"]]$instance[isolate(values$PopRow)]
      catn("sim1CTbuttonAction: Running model!\n" %&% 
             "    designSpec = " %&% designSpecName %&% 
             ",   outcomeModelSpec = " %&% outcomeModelSpecName %&% 
             ",   popModelSpec = " %&% popModelSpecName
      )
      ###  TODO:  Check interoperability
      sim1CToutcome = sim1CT(
        popModelSpec=get(popModelSpecName),
        designSpec=get(designSpecName),
        outcomeModelSpec=get(outcomeModelSpecName)
      )
      assign("sim1CToutcome." %&% input$sim1CTbutton, sim1CToutcome)
      return(HTML(gsub("\n", "<br>", CTresultToString(sim1CToutcome))))
      ### This HTML(gsub()) works great!
    } else { ### 
      return("sim1CTbuttonAction: model is not ready")
    }
  }
  #debug(f.sim1CTbuttonOutput)
  
  output$resultsSim1CTModelMain = 
    renderText({f.sim1CTbuttonOutput()})
  
  
  values = reactiveValues(
    PopRow=which(objects_tables[["PopModelSpecifier"]] == "doseThresholdPopModelSpec"),
    OutcomeRow=which(objects_tables[["OutcomeModelSpecifier"]] == "toxDoseThresholdOutcomeModel"),
    DesignRow=which(objects_tables[["DesignSpecifier"]] == "crm9")
  )
  
  ## values  is an S3 class "reactivevalues". Put this in the eval box:
  #   methods(class="reactivevalues")
  #  and this is what you get:
  #    [.reactivevalues [[.reactivevalues [[<-.reactivevalues [<-.reactivevalues $.reactivevalues $<-.reactivevalues 
  #    as.list.reactivevalues names.reactivevalues names<-.reactivevalues
  
#   updateModelIndices = reactive({
#     values$PopRow <- input$PopRow
#     values$OutcomeRow <- input$OutcomeRow
#     values$DesignRow <- input$DesignRow
#     if(is.null(values$PopRow)) values$PopRow = NA
#     if(is.null(values$OutcomRow)) values$OutcomeRow = NA
#     if(is.null(values$DesignRow)) values$DesignRow = NA
#   })
  
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
    if(input$evalButton > 0) eval(parse(text=isolate(input$evalString)))
  })  ### example:   options(shiny.trace=input$traceButton)[[1]]
  ### example:   options("shiny.trace")[[1]]
  
  
  output$shiny.trace = renderText({
    options(shiny.trace=input$traceButton)
    catn("shiny.trace: ", options("shiny.trace")[[1]], 
         " should equal ", input$traceButton)
    options("shiny.trace")[[1]]
  })   #### Not called!!

  output$headerOutput = renderUI({
    #    addAttr("html", TRUE,
    list(HTML("<head>\n  <title>Clinical Trial Experiment Platform</title>
            </head>\n<div class=\"span12\" style=\"padding: 10px 0px; color:rgb(fff,300,400)\">
            <h2  > <i>Clinical Trial Experiment Platform </i> </h2>\n</div>")
         , tag("script ", 
               "function toggleTrace() {alert(\"Trace toggle is not yet ready!\");}")       
         , tag("table", list(
           tag("tr", 
               list(
                 tag("TD",
                     list(width=120, style="color: blue",
                          checkboxInput(inputId="traceButton", 
                                        value=FALSE,
                                        "trace=" %&% options("shiny.trace")[[1]]
                          ))),
                 tag("TD", 
                     list(style="color:\"red\"", actionButton("evalButton", "evalButton"))),
                 tag("TD",
                     list(width=10, textInput(inputId="evalString", label="", value="1+1"))),
                 tag("TD", list(style="color:red", HTML("&rarr;"))),
                 tag("TD",
                     list(width=800, `text-align`="right", color="red", uiOutput(outputId="evalOutput")))
               )
           )
           ))
               #          , tag("script ", 
               #                "function alertModelNotReady() {alert(\"Model is not yet fully specified!\");}")
               #          , tag("script ", 
#                "function alertModelIsReady() {alert(\"Model is running!\\n\" + 
#                output.);}"
         #)                             
    )
  })
}
#debug(myShinyServerFunction)
print("GOT THIS FAR: starting shinyServer")

shinyServer(myShinyServerFunction)
