

require(shiny)
require(shinysky)
require("CTDesignExperimenter")

shinyServer(function(input, output, session) {
  
  source("debugTools.R", local=TRUE)
  
  rValues = reactiveValues()

  treeObserver = observe(
    label="myTreeObserver", {
      nColumnsInTreeValue = 5
      if(length(input$jstree1) > 0) {
        nSelected <<- length(input$jstree1) / nColumnsInTreeValue
        rValues$nSelected <<- nSelected
        treeSelection <<- matrix(ncol=nColumnsInTreeValue, input$jstree1, byrow=T,
                                 dimnames=list(1:nSelected,
                                   names(input$jstree1)[1:nColumnsInTreeValue]))
        ## Trim leading and trailing whitespace.
        treeSelection[ , "text"] <<- gsub("^[\n\t ]*", "",
                                        gsub("[\n\t ]*$", "",
                                             treeSelection[ , "text"] ))
        rValues$treeSelection = treeSelection
        cat("Entered treeObserver.\n")
        print(rValues$treeSelection)
      }
    }
  )
  # treeObserver$onInvalidate(function() print("jstree1 selection changed!"))

  output$selectedNode = renderText({
    print(paste0(input$jstree1, collapse = ", "))
  })
output$selectedNodes = renderText({  ## Must have a distinct name!
  print(paste0(input$jstree1, collapse = ", "))
})
output$numberSelected = reactive({
    length(input$jstree1)
  })
  output$moreThanOneSelected = reactive({
    length(input$jstree1) > 1
  })
  output$nSelectedText = renderText({
    print(paste0("nSelected=", rValues$nSelected))
    #paste0("nSelected=", output$numberSelected) #  CANNOT READ FROM output.
  })
  
  output$oneRunResults = renderUI(
    {
      input$btnRunOne ## to kick it off.
      runTrial()
      nPatients = length(trialData$patientData)
      returnvalueString1 = paste0(
        "tagList(",
        "div(",
            paste("'", capture.output(
              printVVenv(trialData$trialSummaries))
            , "'", collapse=","),
        "))")
      returnvalue = eval(parse(text=returnvalueString1))
       
      for(iPatient in 1:nPatients) {
        returnvalueString2 = paste0(
          "tagList(hr(), ",
          "p(style='fontsize:large', em('Patient #",
          iPatient,
          "')), ",
          "hr(), ",
          paste("div('", capture.output(
            printVVenv(trialData$patientData[[iPatient]]$VVenv))
            , "')", collapse=", \n") ,
          ")")
        returnvalue = tagList(
            returnvalue , eval(parse(text=returnvalueString2))
        ) 
      }
#         ,
#         paste(collapse=",",
#               "HTML('Patient #", 1:nPatients,
#           "'), div(capture.output(printVVenv(trialData$patientData[[", 1:nPatients,
#               "]]$VVenv))), hr()")
        returnvalue
    })

  output$experimentTableOut = renderTable({
    input$btnAddScen
    experimentTable[nrow(experimentTable)+1, ] <<- NA
    rownames(experimentTable) [nrow(experimentTable)] <<- 
      currentScenario@name
    print(experimentTable)
  })

  reactive({input$btnAddScen; addScenarioToExperiment(currentScenario@name)} )

  observe({
    input$btnCloneScen    # Trigger if clicked
    cat("\nSaving scenario\n")
    assign(isolate(input$scenarioName), pos = 1,
                   currentScenario
                   ##TODO: update currentScenario 
                   ## responding to deletes, insertions, edits in place.
    )
    showshinyalert(session, id="cloneScen",
                   HTMLtext=paste(
                     "Saving scenario, name = ",
                     isolate(input$scenarioName)))
    #window.prompt("sometext","defaultText");
  })
}) 
