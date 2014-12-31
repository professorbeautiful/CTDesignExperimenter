

output$insertEditorUI = renderUI({ 
  try(rm(requirementDF), silent = TRUE)
  try(rm(requirementDF_old), silent = TRUE)
  try(rm(parameterDF), silent = TRUE)
  try(rm(parameterDF_old), silent = TRUE)
  rValues$openingInsertEditor = FALSE

  theInsert = rValues$theInsert
  if(is.null(theInsert)) 
    rValues$theInsert = theInsert = makeTemplateInsert()
  catn("output$insertEditorUI: insert is ", capture.output(theInsert))

  
  pattern = "^I_"
  objectTypeName="Insert"
  source("createSwapMeetObjectTable.R", local=TRUE)  
  
  observerBtnEditOutputVariable = observe({
    if(wasClicked(input$btnEditOutputVariable)) {
      rValues$editingOutputVariable = TRUE
      rValues$theVariable = rValues$theInsert@outputVariable
      updateTabsetPanel(session, "tabsetID", selected = "Variable Editor")
    }
  })
  
  ###################  REQUIREMENTS  ##############################
  
  observerRequirements = observe({
    if(exists("requirementDF"))
      requirementDF_old <<- requirementDF
    requirementDF <<- hot.to.df(input$requirementHOT)
    print(requirementDF)
    if(exists("requirementDF_old")) {
      cat("requirement is changed: ")
      for(pName in names(requirementDF)) {
        #if( requirementDF_old[[pName]] != requirementDF[[pName]])
        cat(pName, " changed from ", requirementDF_old[[pName]], " to ", requirementDF[[pName]])
      }
      cat("\n")
    }
  }
  )
  
  output$requirementHOT <- renderHotable({
    if(is.null(rValues$theInsert))
      NULL
    else {
      df = as.data.frame(sapply(rValues$theInsert@requirements,
                                function(v) c(v@description, printFunctionBody(v@checkDataType))))
      names(df) = sapply(rValues$theInsert@requirements, slot, name="name")
      cat("requirementHOT: df is:\n")
      print(df)
      print(dim(df))
      if(dim(df)[1] > 0) rownames(df) = c("description", "checkDataType")
      df
    }
  }, readOnly = FALSE)
  
  ###################  PARAMETERS  ##############################
  observerParameterDF = observe({
    if(exists("parameterDF"))
      parameterDF_old <<- parameterDF
    parameterDF <<- hot.to.df(input$parameterHOT)
    catn("observer: parameterDF:")
    print(parameterDF)
    if(exists("parameterDF_old")) {
      cat("Parameter is changed: ")
      for(pName in names(parameterDF)) {
        #if( parameterDF_old[[pName]] != parameterDF[[pName]])
        catn(pName, " changed from ", parameterDF_old[[pName]], " to ", parameterDF[[pName]])
      }
      cat("\n")
    }
  }
  )
  
  output$parameterHOT <- renderHotable({
    df = as.data.frame(theInsert@parameters)
    cat("parameterHOT: df is:\n")
    print(df)
    print(dim(df))
    df
  }, readOnly = FALSE)
  
  
  output$insertSubTypeTOP = renderText( {  theInsert@insertSubType})
  
  output$insertBlockString = renderText( { 
    scaffoldObjectNames[which(scaffoldInsertSubTypes==input$selectedInsertSubType)] })
  ## We should probably rename scaffoldObjectNames to scaffoldBlockNames. 
  
  ############ showEditableInsertSlot, a FAILED EXPERIMENT ? ####################
  
  #   showEditableInsertSlot = function(label, slotName) {
  #     tagList(tag('table', tag('tr', 
  #                              tagList(tag('th', label), 
  #                              tag('th', 
  #                                  renderText({slot(rValues$theInsert, slot=slotName)}))
  #     ))))
  #   }
  
  textAreaResize.con = file('textAreaResize.html')
  textAreaResize.html = readLines(con = textAreaResize.con)
  close(textAreaResize.con)  ### Lovely, but it doesn't work.
  
  ### Return value for renderUI "expr" arg starts here.
  div(style="overflow:auto;",
    ### Let's see if we can put all this on one line,
    # using the ideas at https://groups.google.com/forum/#!searchin/shiny-discuss/shinysky/shiny-discuss/rYMmnAtYuJY/_nnzF1ka1vYJ.
    #fluidRow(
    ## If divs instead of columns, vertical instead of horizontal.
    ## Negative offsets are the same as zero. widths must be in 1,...,12.
    #    column(width=2, offset=0, strong("Editing an insert ", class="INSERTlevel")),
    #    column(width=1, offset=-10, img(src='Insert32.png', align="absmiddle"))  ### Place in app root. Also, "www/" will not work.
    h2(strong("Editing an Insert ",
              img(width=50, height=50, src='Insert32.png', align="absmiddle"),  ### Place in app root. Also, "www/" will not work.
              class="INSERTlevel")),
    #),
    #    br(),
    #    hr(),
    #    div(class='col-12',
    div(class = "well container-fluid", 
        actionButton(inputId="btnNewInsert" , 
                     label=div("New Insert", class = "INSERTlevel")),
        tagAppendAttributes(a(
          actionButton(inputId="btnSearchInsert" , 
                       label=div("Search swapmeet & load Insert", class = "INSERTlevel"),
          )),
          href="#idSearchInsert"),
        actionButton(inputId="btnReplaceInsertInScenario" , 
                     label=div("Replace Insert In Scenario", class="INSERTlevel")),
        actionButton(inputId="btnSaveInsert" , 
                     label=div("Save Insert...", class = "INSERTlevel"))
        # css.class doesnt work here.   css.class = "treeclass-2",
    ),
    div(class = "well container-fluid", 
        fluidRow(
          #      span(class="BLOCKlevel", 
          column(width = 2, selectInput(inputId="selectedInsertSubType", 
                                        label=strong(class="INSERTlevel", "Insert Type"),
                                        selected=theInsert@insertSubType,
                                        choices=scaffoldInsertSubTypes)),
          column(width=2, 
                 em("  which is placed in block ", class="BLOCKlevel"),
                 br(),
                 fluidRow(
                   column(width=4,
                          img(src='BLOCK32.png', align="absmiddle")),  ### Place in app root. Also, "www/" will not work.
                   column(width=8,
                          span(class="BLOCKlevel", 
                               textOutput(outputId='insertBlockString')
                          )
                   )
                 )
          )
        )
    )
    ,
    tags$style(type="text/css", 
               "#insertName {width: 450px; }"),
    textInput(inputId="insertName", 
              label = strong("name", class="INSERTlevel"), 
              theInsert@name),
    tags$style(type="text/css", 
               "#insertDescription {width: 450px; }"),
    div(tags$label(strong("description", class="INSERTlevel")), 
        tags$textarea(id = "insertDescription", rows=3, cols=80,
              theInsert@description)),
    # showEditableInsertSlot("DESCRIPTION", "description"),
    #         tagAppendAttributes(tag=div(class="row-fluid",
    #                                     strong(
    #           textInput("insertDescription", label = ("description"), 
    #                     theInsert@description))),
    #           class="INSERTlevel,row-fluid", 
    #           style="width:100%"),
    div(class="INSERTlevel", strong("output variable (click ",
               bsActionButton(inputId="btnEditOutputVariable",
                              label=img(src="Var32.png"),
                              style="link"),
               " to edit)" ),        
        renderText( { capture.output(theInsert@outputVariable) })),
    br(),
    #singleton(tags$head(textAreaResize.html)),
    div(tags$label(strong("generatorCode", class="INSERTlevel")), 
        tags$textarea(id = "generatorCode", rows=3, cols=80,
                      printFunctionBody(theInsert@generatorCode)),
        actionButton(inputId="btnCheckCode", "check code"),
        fluidRow(column(width=4, bsAlert(inputId='generatorCodeAlert')))
        )
    ,
    #tags$script('vb.width("100%").css("bold")'),  THE CULPRIT!!! Caused all the renderText elements to fail.
    div(class = "well container-fluid", 
        div(strong("requirements", class="INSERTlevel")), 
        div(class = "row-fluid", 
            conditionalPanel('window.Shiny.shinyapp.$bindings.requirementHOT.el.attributes.length > 0',
                             hotable("requirementHOT"))),
        img(src="Var32.png"),
        actionButton(inputId="btnAddRequirement" , 
                     label=div("Add requirement (not implemented yet)", 
                               class = "VARlevel")),
        conditionalPanel('window.Shiny.shinyapp.$bindings.requirementHOT.el.attributes.length > 0',
                         ## Use JS version of outputPreamble?
                         actionButton(inputId="btnEditRequirement" , 
                                      label=div("Edit requirement (not implemented yet)", 
                                                class = "VARlevel")),
                         actionButton(inputId="btnRemoveRequirement" , 
                                      label=div("Remove requirement (not implemented yet)", 
                                                class = "VARlevel"))
        )
    ),    
    
    div(class = "well container-fluid", 
        div(strong("parameters", class="INSERTlevel")), 
        div(class = "row-fluid", hotable("parameterHOT")),
        actionButton(inputId="btnAddParameter" , 
                     label=div("Add parameter (not implemented yet)", 
                               class = "INSERTlevel")),
        actionButton(inputId="btnCheckParameter" , 
                     label=div("Check parameter (not implemented yet)", 
                               class = "INSERTlevel")),
        actionButton(inputId="btnRemoveParameter" , 
                     label=div("Remove parameter (not implemented yet)", 
                               class = "INSERTlevel"))
        ,
        tableOutput(outputId = "parameterTable")
    ),     
    
    hr(),
    div(class="INSERTlevel",
        renderText({"author: " %&% theInsert@author}),
        renderText({"timestamp: " %&% capture.output(theInsert@timestamp)}),
        renderText({"file: " %&% theInsert@filename})
    ),
    
    div(class='col-6',
        conditionalPanel(
          "input.btnSearchInsert > 0", 
          hr(),
          #        dataTableOutput("allInsertsTable"))
          tagAppendAttributes(a(""), id="idSearchInsert"),
          h3("Click on the radiobutton to load the Insert into the template above."),
          HTML('<div id="chooseInsertRadioGroup" class="control-group shiny-input-radiogroup">
                           <label class="control-label" for="chooseInsertRadioGroup">Swapmeet Inserts</label>'),
          dataTableOutput("allInsertsTable"),
          HTML('</div>')
        )
    )
  )
})

###  Is the following necessary?
observer_btnSearchInsertObserver = observe(label="observer_btnSearchInsertObserver", {
  catn("searchInsertObserver: input$btnSearchInsert = ", 
       input$btnSearchInsert)
  if(!is.null(input$btnSearchInsert))
    if(input$btnSearchInsert > 0) 
      if(!is.null(input$searchTypeAhead)) 
        if(input$searchTypeAhead != "") {
          theInsert = try(source(swapMeetDir() %&% input$searchTypeAhead, 
                              local = TRUE)$value) 
          if(class(theInsert) == "VariableGenerator") rValues$theInsert = theInsert
          else shinyalert("Sorry, it wasn't a VariableGenerator file.")
        }
})

observer_selectedInsertSubType = observe(label="observer_selectedInsertSubType",
  {
    input$selectedInsertSubType
    isolate({
      if(!is.null(rValues$theInsert)) {
        theInsert = rValues$theInsert
        theInsert@insertSubType <- input$selectedInsertSubType
        rValues$theInsert = theInsert
      }
    })
  })

observer_searchInsert = observe(label="observer_searchInsert",
  {       ### Find and load a Insert from a file.
  if(isolate(input$tabsetID)=="Insert Editor" & !is.null(input$btnSearchInsert)){
    if(input$btnSearchInsert > 0 & !is.null(input$insertSearchFileInput)) {
      try({
        theInsert = source(swapMeetDir() %&% input$insertSearchFileInput, 
                        local = TRUE)$value
        print(str(theInsert))
        if(class(theInsert) == "VariableGenerator") rValues$theInsert = theInsert
        else shinyalert("Sorry, it wasn't a VariableGenerator file.")
      })
    }
  }
})

observe({
  if(wasClicked(input$btnCheckCode)) {
    fString = isolate(input$generatorCode)
    theFunction = try(eval(parse(text=fString)))
    theMessage = ifelse(class(theFunction) ==  'try-error',
      "error! " %&% theFunction,
      "generatorCode parses fine" %&% if(!is.function(theFunction)) " but it's not a function."
    )
    createAlert(session, inputId='generatorCodeAlert', alertId = 'generatorCodeAlertCreatorID',
                title='Alert: generatorCode parsing result',
                message=theMessage,
                append=FALSE)
  }
})

###################### Clear the inputs to create a new Insert   ##############

observer_newInsert = observe(label="observer_newInsert",
  {       
  if(isolate(input$tabsetID)=="Insert Editor" & !is.null(input$btnNewInsert)){
    if(input$btnNewInsert > 0) {
      cat("New Insert is generated\n")
      rValues$theInsert = makeTemplateInsert()
    }
  }
})

Insert = VariableGenerator

makeInsert = function() {
  theNewParameters = hot.to.df(input$parameterHOT)
  if(is.null(theNewParameters)) theNewParameters = list()
  result = try(
    Insert(insertSubType = input$selectedInsertSubType,
           parameters = theNewParameters,
           provisions = rValues$theInsert@provisions,  #Not yet editable
           requirements = rValues$theInsert@requirements,  #Not yet editable,
           outputVariable = rValues$theInsert@outputVariable,  #Not yet editable,
           generatorCode = eval(parse(text="function() " %&% input$generatorCode)))
    ) 
  if(class(result) == 'try-error') {
    warning("Could not create the insert: ", result)
    browser("makeInsert error", result)
    return(rValues@theInsert)  ##TODO: should we prefer to throw error?
  }
  result@name = input$insertName 
  result@description = input$insertDescription 
  return(result)
  
}

### This writes the file!
observeBtnSaveInsert = observe(label="observeBtnSaveInsert", {       
  ### Save Insert in a swapMeet file.
  if(isolate(input$tabsetID)=="Insert Editor" & !is.null(input$btnSaveInsert)){
    if(input$btnSaveInsert > 0) {
      theInsert = makeInsert()
      if(class(theInsert) == "try-error")
        shinyalert("Error in observeBtnSaveInsert/makeInsert: " %&% theInsert)
      else {
        theInsert = writeSwapMeetFile(theInsert, verbose = TRUE)
        shinyalert("observeBtnSaveInsert/makeInsert: made new Insert. Wrote file "%&%
                     theInsert@filename %&% ". CAUTION: This will NOT (yet) replace in Scenario." )
        rValues$theInsert = theInsert
        # TODO:  insert into Scenario, 
        #    switch to Scenario tab,
        #    and warn that Scenario is not saved.
      } 
    }
  }
})

addInsert = function(rVcS, theInsert) {
  rVcS@inserts =  
    new('ListOfInserts', c(rVcS@inserts, theInsert))
  return(rVcS)
}
observer_btnReplaceInsertInScenario = observe(label="observer_btnReplaceInsertInScenario", { 
  ### Save Insert in the scenario
  if(wasClicked(input$btnReplaceInsertInScenario)) {
      isolate({
        theInsert = makeInsert()
        if(class(theInsert) == "try-error")
          shinyalert("Error in Insert: " %&% theInsert)
        else {
          rValues$theInsert = theInsert = writeSwapMeetFile(theInsert, verbose = TRUE) 
          #if(!is.null(input$treeSelectionDepth))
          #  if(input$treeSelectionDepth == 2) {
          ### Replace
          rVcS = rValues$currentScenario  ### Trying to prevent too much reactivity.
          rVcS = removeInsert(rVcS, rValues$treeSelectionPath)
          rValues$currentScenario <- addInsert(rVcS, theInsert) 
          updateTabsetPanel(session, "tabsetID", selected = "Current scenario")
          #  }
        }
      })
  }
})

observer_chooseInsertRadioGroup = observe(label="observer_chooseInsertRadioGroup", {
  chooseInsertChoice = input$chooseInsertRadioGroup  # reactivity here
  if(!is.null(chooseInsertChoice))
    isolate({
      insertFileName = allInsertsDF[chooseInsertChoice, "filename"]
      catn("insertFileName = ", insertFileName)
      theInsert = try(
        source(swapMeetDir() %&% insertFileName, local=TRUE)$value
      )
      if(class(theInsert) != "try-error")
        rValues$theInsert = theInsert
      else
        cat("observer_chooseInsertRadioGroup:  insert reading went bad", theInsert, "\n")
    })
})
