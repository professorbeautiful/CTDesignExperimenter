

insertToDataframe = function(theInsert) {
  data.frame( 
    output=capture.output(show(theInsert@outputVariable)),
    parameters=ifelse(is.null(theInsert@parameters) | length(theInsert@parameters)==0,
                      character(0),
                      paste(names(theInsert@parameters), 
                            as.vector(capture.output(theInsert@parameters)),
                            sep="=", collapse="\n")
    ),
    needed=ifelse(is.null(theInsert@requirements),
                  "",
                  paste(sapply(theInsert@requirements, capture.output), 
                        collapse="\n")
    ),
    generator=printFunctionBody(theInsert@generatorCode),
    author=theInsert@author,
    timestamp=capture.output(theInsert@timestamp),
    filename=theInsert@filename      
    
  )
}


output$insertEditorUI = renderUI({ 
  try(rm(requirementDF))
  try(rm(requirementDF_old))
  try(rm(parameterDF))
  try(rm(parameterDF_old))
  rValues$openingInsertEditor = FALSE
  theInsert = rValues$theInsert
  catn("output$insertEditorUI: insert is ", capture.output(theInsert))
  iFilenames <<- rev(dir(swapMeetDir(), pattern = "^I_"))
  allInsertsList = lapply(iFilenames, function(fname) {
    tempInsert = source(swapMeetDir() %&% fname, local=TRUE)$value
    if(! (class(tempInsert)=="VariableGenerator"))
      browser()
    insertToDataframe(tempInsert)
  })
  allInsertsDF <<- Reduce(rbind, allInsertsList)
  radioButtons = sapply(1:nrow(allInsertsDF),
                        function(rownum)
                          HTML("<input type=\"radio\" name=\"chooseInsert\" 
                               id=\"chooseInsert" %&% rownum
                               %&% "\" value=" %&% rownum %&% ">"))
  allInsertsDF = data.frame(select=radioButtons, allInsertsDF) 
  
  allInsertnames <<- data.frame(name=unique(allInsertsDF$name))
  output$allInsertsTable <<- renderDataTable(allInsertsDF)
  
  
  ###################  REQUIREMENTS  ##############################
  
  observe({
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
    df = as.data.frame(sapply(theInsert@requirements,
                  function(v) c(v@description, printFunctionBody(v@checkDataType))))
    names(df) = sapply(theInsert@requirements, slot, name="name")
    cat("requirementHOT: df is:\n")
    print(df)
    print(dim(df))
    rownames(df) = c("description", "checkDataType")
    df
  }, readOnly = FALSE)
  
  ###################  PARAMETERS  ##############################
  observe({
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
    #names(df) = namessapply(theInsert@requirements, slot, name="name")
    cat("parameterHOT: df is:\n")
    print(df)
    print(dim(df))
    df
  }, readOnly = FALSE)
  
  
  output$insertSubTypeTOP = renderText( {  theInsert@insertSubType})
  
  ############ showEditableInsertSlot, a FAILED EXPERIMENT ? ####################
  
#   showEditableInsertSlot = function(label, slotName) {
#     tagList(tag('table', tag('tr', 
#                              tagList(tag('th', label), 
#                              tag('th', 
#                                  renderText({slot(rValues$theInsert, slot=slotName)}))
#     ))))
#   }
  
  ### Return value for renderUI "expr" arg starts here.
  div(
    ### Let's see if we can put all this on one line,
    # using the ideas at https://groups.google.com/forum/#!searchin/shiny-discuss/shinysky/shiny-discuss/rYMmnAtYuJY/_nnzF1ka1vYJ.
    list(
      ## If divs instead of columns, vertical instead of horizontal.
      ## Negative offsets are the same as zero. widths must be in 1,...,12.
        column(width=2, offset=-10, strong("Editing an insert ", class="INSERTlevel")),
        column(width=1, offset=-10, img(src='Insert32.png', align="absmiddle")),  ### Place in app root. Also, "www/" will not work.
        column(width=1, offset=-10, strong(" in block ", class="BLOCKlevel")),
        column(width=1, offset=-10, img(src='BLOCK32.png', align="absmiddle")),  ### Place in app root. Also, "www/" will not work.
        column(width=6, offset=-10, div(class="BLOCKlevel", textOutput("insertSubTypeTOP"))) 
    )
    ,
    br(),
    hr(),
    div(class='col-12',
        div(class = "well container-fluid", 
            actionButton(inputId="btnNewInsert" , 
                         label=div("New Insert", class = "INSERTlevel")),
            actionButton(inputId="btnSearchInsert" , 
                         label=div("Search for insert", class = "INSERTlevel")),
            actionButton(inputId="btnSaveInsert" , 
                         label=div("Save insert", class = "INSERTlevel")),
            actionButton(inputId="btnSaveInsertAs" , 
                         label=div("Save Insert as...", class="INSERTlevel"))
            # css.class doesnt work.   css.class = "treeclass-2"),
        ),
        tagAppendAttributes(tag=strong(
                              textInput("insertName", 
                                        label = strong("name", class="INSERTlevel"), 
                                        theInsert@name)),
                            style="width:100%"),
        textInput("insertDescription", 
                  label = strong("description", class="INSERTlevel"), 
                  theInsert@description),
        # showEditableInsertSlot("DESCRIPTION", "description"),
        #         tagAppendAttributes(tag=div(class="row-fluid",
        #                                     strong(
        #           textInput("insertDescription", label = ("description"), 
        #                     theInsert@description))),
        #           class="INSERTlevel,row-fluid", 
        #           style="width:100%"),
        div(strong("output variable", class="INSERTlevel"), 
            img(src="Var32.png")),
        renderText( { capture.output(theInsert@outputVariable) }),
        
        div(strong("generatorCode", class="INSERTlevel"), 
            div(style="width:1000", 
              textInput(inputId = "generatorCode",  label = "",
                        printFunctionBody(theInsert@generatorCode)),
              actionButton(inputId="btnCheckCode", "check code (not implemented)")
            ))
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
      )
    ),
    div(class='col-6',
        conditionalPanel("input.btnSearchInsert > 0", 
                         hr(),
                         dataTableOutput("allInsertsTable"))
    )
  )
})

###  Is the following necessary?
searchInsertObserver = observe(label="searchInsertObserver", {
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



observe({       ### Find and load a Insert from a file.
  if(input$tabsetID=="Editors" & !is.null(input$btnSearchInsert)){
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

###################### Clear the inputs to create a new Insert   ##############

observe({       
  if(input$tabsetID=="Editors" & !is.null(input$btnNewInsert)){
    if(input$btnNewInsert > 0) {
      cat("NewInsert functionality is not yet implemented\n")
#       rValues$theInsert = 
#         VariableGenerator(name = "", description = "", checkDataType = function(x){TRUE})
    }
  }
})

Insert = VariableGenerator
makeInsert = function() {
  theNewParameters = hot.to.df(input$parameterHOT)
  
  try(
    Insert(insertSubType = rValues$theInsert@insertSubType,
    #       name = input$insertName, 
    #       description = input$insertDescription, 
           parameters = theNewParameters,
           provisions = rValues$theInsert@provisions,  #Not yet editable
           requirements = rValues$theInsert@requirements,  #Not yet editable,
           outputVariable = rValues$theInsert@outputVariable,  #Not yet editable,
           generatorCode = eval(parse(text="function() " %&% input$generatorCode)))
    ) 
}

observeBtnSaveInsert = observe(label="observeBtnSaveInsert", {       
  ### Save Insert in the scenario
  if(input$tabsetID=="Editors" & !is.null(input$btnSavInserte)){
    if(input$btnSaveInsert > 0) {
      theInsert = makeInsert()
      if(class(theInsert) == "try-error")
        shinyalert("Error in observeBtnSaveInsert/makeInsert: " %&% theInsert)
      else {
        theInsert = writeSwapMeetFile(theInsert, verbose = TRUE)
        shinyalert("observeBtnSaveInsert/makeInsert: made new Insert. Wrote file "%&%
                     theInsert$filename)
        rValues$theInsert = theInsert
        # TODO:  insert into Scenario, 
        #    switch to Scenario tab,
        #    and warn that Scenario is not saved.
      } 
    }
  }
})

### This writes the file!
observeBtnSaveInsertAs = observe(label="observeBtnSaveInsertAs", { 
  ### Save Insert in a swapMeet file.
  if(input$tabsetID=="Editors" & !is.null(input$btnSaveInsertAs)){
    if(input$btnSaveInsertAs > 0) {
      theInsert = makeInsert()
      if(class(theInsert) == "try-error")
        shinyalert("Error in Insert: " %&% theInsert)
      else 
        rValues$theInsert = theInsert = writeSwapMeetFile(theInsert, verbose = TRUE)      
    }
  }
})

observeChooseInsert = observe(label="observeChooseInsert", {
  insertFileName = allInsertsDF[input$chooseInsert, "filename"]
  catn("insertFileName = ", insertFileName)
  theInsert = try(
    source(swapMeetDir() %&% insertFileName, local=TRUE)$value
  )
  if(class(theInsert) != "try-error")
    rValues$theInsert = theInsert
  else
    cat("observeChooseInsert:  insert reading went bad", theInsert, "\n")
})
