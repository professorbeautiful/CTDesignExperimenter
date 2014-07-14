output$varEditorUI = renderUI({ 
  theVar = rValues$theVar
  catn("output$varEditorUI: var is ", capture.output(theVar))
  CHECK<-capture.output(theVar@checkDataType)
  CHECK = gsub("[\t ]+", " ", 
       paste(collapse="", 
             CHECK [-length(CHECK)] ) ) 
  catn("CHECK: ", CHECK)
  result = data.frame(
    name=theVar@name,
    description=theVar@description,
    checkDataType = CHECK ,
    author=theVar@author,
    timestamp=capture.output(theVar@timestamp),
    filename=theVar@filename      
  )
  print(result)
  div(
    HTML(" Editing selected Variable"), 
    hr(),
    textInput("varName", label = "name", theVar@name),
    textInput("varDescription", label = "description", theVar@description),
    textInput("varCHECK", label = "check", CHECK),
    hr(),
    renderText({"author: " %&% theVar@author}),
    renderText({"timestamp: " %&% capture.output(theVar@timestamp)}),
    renderText({"file: " %&% theVar@filename}),
    hr(),
    actionButton(inputId="btnNewVar" , 
                 label="New variable", css.class = "treeClass-3"),
    actionButton(inputId="btnSearchVar" , 
                 label="Find and load variable", css.class = "treeClass-3"),
    actionButton(inputId="btnSaveVar" , 
                 label="Save variable in scenario", css.class = "treeClass-3"),
    actionButton(inputId="btnSaveVarAs" , 
                 label="Save variable as...", css.class = "treeClass-3"),
    hr(),
    conditionalPanel("input.btnSearchVar > 0",
                     fileInput("varSearchFileInput", "varSearchFileInput"))
  )
})

observe({       ### Clear the inputs to create a new variable.
  if(input$tabsetID=="Editors" & !is.null(input$btnSearchVar)){
    if(input$btnSearchVar > 0) {
      try({
        fileInfo = input$varSearchFileInput
        print(fileInfo)
        theVar = source(fileInfo$datapath, local = TRUE)$value
        print(str(theVar))
        if(class(theVar) == "Variable") rValues$theVar = theVar
      })
    }
  }
})


observe({       ### Clear the inputs to create a new variable.
  if(input$tabsetID=="Editors" & !is.null(input$btnNewVar)){
    if(input$btnNewVar > 0) {
      rValues$theVar = 
        Variable(name = "", description = "", checkDataType = function(x){TRUE})
    }
  }
})

observe({       ### Save Variable in a swapMeet file.
  if(input$tabsetID=="Editors" & !is.null(input$btnSaveVarAs)){
    if(input$btnSaveVarAs > 0) {
      theVar = try(
        Variable(name = input$varName, 
                        description = input$varDescription, 
                        checkDataType = eval(parse(text=input$varCHECK)))) 
      if(class(theVar) == "try-error")
        shinyalert("Error in variable: " %&% theVar)
      else {
        theVar = writeSwapMeetFile(theVar, verbose = TRUE)
        rValues$theVar = theVar
      }
    }
  }
})