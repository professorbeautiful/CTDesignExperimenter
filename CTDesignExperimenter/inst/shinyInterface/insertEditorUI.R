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
    theInsert <<- source(swapMeetDir() %&% fname, local=TRUE)$value
    if(! (class(theInsert)=="VariableGenerator"))
      browser()
    insertToDataframe(theInsert)
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
    HTML(" Editing selected Insert - IN PROGRESS"), 
    hr(),
    actionButton(inputId="btnSearchInsert" , 
                 label="Search for insert", css.class = "treeClass-2"),
    actionButton(inputId="btnSaveInsert" , 
                 label="Save insert", css.class = "treeClass-2"),
    textInput("insertName", label = "name", theInsert@name)
    #   ,  MORE STUFF
    
  )
})

