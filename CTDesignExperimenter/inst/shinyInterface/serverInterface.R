

require(shiny)
require(shinysky)
require("CTDesignExperimenter")

shinyServer(function(input, output, session) {
  # jstree
  rValues = reactiveValues()
  treeObserver = observe(
    label="myTreeObserver",
    {
      showshinyalert(session, "alert_jstree1", 
                   paste0("You selected these items in the tree: ", 
                          paste0(input$jstree1, collapse = ", ")))
      rValues$selectionLength = length(input$jstree1)
    }
  )
  # treeObserver$onInvalidate(function() print("jstree1 selection changed!"))
  output$selectedNode = renderText({
    print(paste0(input$jstree1, collapse = ", "))
    })
  output$numberSelected = reactive({
    length(input$jstree1)
    })
  output$nSelectedText = renderText({
    paste0("nSelected=", as.character(length(input$jstree1)))
    #paste0("nSelected=", output$numberSelected) #  CANNOT READ FROM output.
  })
  #anySelected = reactiveValues(numberSelected > 0)
                                  
}) 
