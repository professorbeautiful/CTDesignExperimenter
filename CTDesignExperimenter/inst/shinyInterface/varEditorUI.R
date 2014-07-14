output$varEditorUI = renderUI({ 
  theVar = findObjectInScenario(rValues$treeSelectionIndex)
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
    actionButton(inputId="btnSearchVar" , 
                 label="Search for variable", css.class = "treeClass-3"),
    actionButton(inputId="btnSaveVar" , 
                 label="Save variable", css.class = "treeClass-3")
  )
})

