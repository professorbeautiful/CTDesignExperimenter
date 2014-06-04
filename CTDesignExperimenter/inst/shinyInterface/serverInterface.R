

require(shiny)
require(shinysky)
require("CTDesignExperimenter")

shinyServer(function(input, output, session) {
  
  source("debugTools.R", local=TRUE)
  
  rValues = reactiveValues()

  treeObserver = observe(
    label="myTreeObserver", {
      if(length(input$jstree1) > 0) {
        nSelected <<- length(input$jstree1) / 4
        treeSelection <<- matrix(ncol=4, input$jstree1, byrow=T,
                                 dimnames=list(1:nSelected,
                                   names(input$jstree1)[1:4]))
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
    paste0("nSelected=", as.character(length(input$jstree1)))
    #paste0("nSelected=", output$numberSelected) #  CANNOT READ FROM output.
  })
  
  output$experimentTable = renderTable({experimentTable})

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
