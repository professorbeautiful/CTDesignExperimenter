insertToDataframe = function(theInsert) {
  data.frame(
    output=capture.output(show(theInsert@outputVariable)),
    parameters=ifelse(length(theInsert@parameters)==0,
                      "",
                      paste(names(insert@parameters), 
                            as.vector(capture.output(insert@parameters)),
                            sep="=", collapse="\n")
    ),
    needed=ifelse(is.null(theInsert@requirements),
                      "",
                      paste(sapply(theInsert@requirements, capture.output), 
                            collapse="\n")
    ),
    generator=printFunctionBody(object@generatorCode),
    author=theInsert@author,
    timestamp=capture.output(theInsert@timestamp),
    filename=theInsert@filename      
    
  )
}


output$insertEditorUI = renderUI({ 
  theInsert = rValues$theInsert
  catn("output$insertEditorUI: insert is ", capture.output(theInsert))
  iFilenames <<- rev(dir(swapMeetDir(), pattern = "^I_"))
  allInsertsList = lapply(vFilenames, function(fname) {
    theInsert = source(swapMeetDir() %&% fname, local=TRUE)$value
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
    HTML(" Editing selected Insert - NOT IMPLEMENTED"), 
    hr(),
    actionButton(inputId="btnSearchInsert" , 
                 label="Search for insert", css.class = "treeClass-2"),
    actionButton(inputId="btnSaveInsert" , 
                 label="Save insert", css.class = "treeClass-2")
    textInput("insertName", label = "name", theInsert@name)
    #   ,  MORE STUFF
    
  )
})

