
output$varEditorUI = renderUI({ 
  rValues$openingVariableEditor <<- FALSE
  if(is.null(rValues$theVar))
    rValues$theVar = makeTemplateVariable()
  theVar = rValues$theVar
  CHECK = printFunctionBody(theVar@checkDataType)
  catn("output$varEditorUI: var is ", capture.output(theVar))
  
  pattern = "^V_"
  objectTypeName="Variable"
  source("createSwapMeetObjectTable.R", local=TRUE)  
  
  ### RETURN VALUE FOR UI
  div(css.class="VARlevel",
      h2(strong("Editing a Variable ",
                img(width=50, height=50, src='Var32.png', align="absmiddle"),  ### Place in app root. Also, "www/" will not work.
                class="VARlevel")),
      hr(),
      div(class='col-6',
          actionButton(inputId="btnNewVar" , 
                       label="New Variable", css.class = "VARlevel"),
          tagAppendAttributes(a(
            actionButton(inputId="btnSearchVar" , 
                         label="Search swapmeet & load Variable", css.class = "VARlevel")
          ),
          href="#idSearchVariable"),
          actionButton(inputId="btnReplaceVariableInInsert" ,
                       label="Replace Variable in Insert", css.class = "VARlevel"),
          actionButton(inputId="btnSaveVarAs" , 
                       label="Save Variable as...", css.class = "VARlevel"),
          hr()
      )
      ,
      # select2Input WORKS, and variable loads, but not very usable.
      HTML('&nbsp;'),
      div(style="overflow-y: auto; max-height: 400px;", class = "well container-fluid",
          tagAppendAttributes(h3(" Variable slots"), class="VARlevel"),
          textInput("varName", label = "name", rValues$theVar@name),
          tagAppendAttributes(div(
            textInput("varDescription", label = "description", rValues$theVar@description)),
            style="width:100%"),
          textInput("varCHECK", label = "check", printFunctionBody(rValues$theVar@checkDataType)),
          hr(),
          renderText({"author: " %&% theVar@author}),
          renderText({"timestamp: " %&% capture.output(theVar@timestamp)}),
          renderText({"file: " %&% theVar@filename})
      ),
      div(class='col-6',
          conditionalPanel(
            condition="input.btnSearchVar > 0", 
            hr(),
            h3("Use typeahead to subset the rows."),
            textInput.typeahead(id="searchTypeAhead", "typeahead", 
                                local=allVariablesDF, 
                                tokens=paste(allVariablesDF$name,
                                             allVariablesDF$description), 
                                valueKey="filename", 
                                template=HTML("{{name}} : {{description}}")
            ),
            tagAppendAttributes(a(""), id="idSearchVariable"),
            h3("Click on the radiobutton to load the Variable into the template above."),
            HTML('<div id="chooseVariable" class="control-group shiny-input-radiogroup">
                           <label class="control-label" for="chooseVariable">Swapmeet Variables</label>'),
            dataTableOutput("allVariablesTable"),
            HTML('</div>')
          )
      )
  )
})


observe({       ### Find and load a variable from a file.
  if(input$tabsetID=="Variable Editor" & !is.null(input$btnSearchVar)){
    if(input$btnSearchVar > 0 & !is.null(input$varSearchFileInput)) {
      try({
        #         fileInfo = input$varSearchFileInput
        #         print(fileInfo)
        #        theVar = source(fileInfo$datapath, local = TRUE)$value
        theVar = source(getSwapMeetDir() %&% input$varSearchFileInput, 
                        local = TRUE)$value
        print(str(theVar))
        if(class(theVar) == "Variable") rValues$theVar = theVar
        else shinyalert("Sorry, it wasn't a Variable file.")
      })
    }
  }
})


observe({       ### Clear the inputs to create a new variable.
  if(input$tabsetID=="Variable Editor" & !is.null(input$btnNewVar)){
    if(input$btnNewVar > 0) {
      rValues$theVar = 
        Variable(name = "", description = "", checkDataType = function(x){TRUE})
    }
  }
})

readVarFromPage = function() {
  theVar = try(
    Variable(name = input$varName, 
             description = input$varDescription, 
             checkDataType = eval(parse(text=input$varCHECK)))) 
  if(class(theVar) == "try-error")
    shinyalert("Error in variable: " %&% theVar)
  else
    rValues$theVar = theVar
  return(theVar)
}

observerBtnReplaceVariableInInsert = observe({
  if(wasClicked(input$btnReplaceVariableInInsert)){
    isolate({
      if(isTRUE(rValues$editingOutputVariable)) {
        theVar = readVarFromPage()
        if(class(theVar) != 'try-error') {
          rValues$theInsert@outputVariable = rValues$theVar
          rValues$editingOutputVariable = FALSE
          updateTabsetPanel(session, "tabsetID", selected = "Insert Editor")  
        }
      }
      else {
        ## TODO: load directly into insert in current scenario
      }
    })
  }
})

observe({       ### Save Variable in a swapMeet file.
  if(input$tabsetID=="Variable Editor" & wasClicked(input$btnSaveVarAs)){
    isolate({
      theVar = readVarFromPage()
      if(class(theVar) != "try-error") {
        theVar = writeSwapMeetFile(theVar, verbose = TRUE)
        rValues$theVar = theVar
      }
    })
  }
})

observerChooseVariable = observe({
  chooseVariable = input$chooseVariable ## reactivity here 
  catn("observerChooseVariable:  chooseVariable number= ", chooseVariable)
  if(!is.null(chooseVariable))
    isolate({
      varFileName = allVariablesDF[chooseVariable, "filename"]
      catn("observerChooseVariable:  varFileName = ", varFileName)
      theVar = try(
        source(getSwapMeetDir() %&% varFileName, local=TRUE)$value
      )
      if(class(theVar) != "try-error")
        rValues$theVar <- theVar
    })
})