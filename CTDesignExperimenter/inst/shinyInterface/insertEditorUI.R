insertToDataframe = function(theInsert) {
  data.frame(
    output=capture.output(show(theInsert@outputVariable)),
    parameters=ifelse(length(theInsert@parameters)==0,
                      "",
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
  rValues$openingInsertEditor = FALSE
  theInsert = rValues$theInsert
  catn("output$insertEditorUI: insert is ", capture.output(theInsert))
  iFilenames <<- rev(dir(swapMeetDir(), pattern = "^I_"))
  allInsertsList = lapply(iFilenames, function(fname) {
    tempInsert = source(swapMeetDir() %&% fname, local=TRUE)$value
    if(! (class(theInsert)=="VariableGenerator"))
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
  
  
  div(
    HTML("Insert Editor"), 
    hr(),
    div(class='col-6',
        actionButton(inputId="btnNewInsert" , 
                     label="New Insert", css.class = "treeClass-2"),
        actionButton(inputId="btnSearchInsert" , 
                     label="Search for insert", css.class = "treeClass-2"),
        actionButton(inputId="btnSaveInsert" , 
                     label="Save insert", css.class = "treeClass-2"),
        actionButton(inputId="btnSaveInsertAs" , 
                     label="Save Insert as...", css.class = "treeClass-2"),
        renderText( {theInsert@insertSubType}),
        textInput("insertName", label = "name", theInsert@name),
        tagAppendAttributes(div(
          textInput("insertDescription", label = "description", theInsert@description)),
          style="width:100%"),
        renderText( {capture.output(theInsert@outputVariable)}),
        tagAppendAttributes(div(
          textInput("generator", label = "generator", printFunctionBody(theInsert@generatorCode))),
          style="width:100%"),
        hr(),
        renderText({"author: " %&% theInsert@author}),
        renderText({"timestamp: " %&% capture.output(theInsert@timestamp)}),
        renderText({"file: " %&% theInsert@filename})
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

###################


observe({       ### Clear the inputs to create a new Insert.
  if(input$tabsetID=="Editors" & !is.null(input$btnNewInsert)){
    if(input$btnNewInsert > 0) {
      cat("NewInsert functionality is not yet implemented\n")
#       rValues$theInsert = 
#         VariableGenerator(name = "", description = "", checkDataType = function(x){TRUE})
    }
  }
})

observeBtnSaveInsertAs = observe(label="observeBtnSaveInsertAs", {       ### Save Insert in a swapMeet file.
  if(input$tabsetID=="Editors" & !is.null(input$btnSaveInsertAs)){
    if(input$btnSaveInsertAs > 0) {
      theInsert = try(
        Insert(name = input$insertName, 
                 description = input$insertDescription, 
                 generatorCode = eval(parse(text=input$generatorCode)))) 
      if(class(theInsert) == "try-error")
        shinyalert("Error in Insert: " %&% theInsert)
      else {
        theInsert = writeSwapMeetFile(theInsert, verbose = TRUE)
        rValues$theInsert = theInsert
      }
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
