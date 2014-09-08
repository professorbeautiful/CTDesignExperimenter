require(shiny)
require(shinysky)


shinyServer(function(input, output, session) {

  source("debugTools.R", local=TRUE)    ### This is super-useful!
  
  rV = reactiveValues()
  rV$nav = list(
    "Branch"=list("twig"=list("leafOnTwigA","leafOnTwigB" ),"leafOnBranch"),
    "Topleaf"
  )
  ## The next line has no effect unless ui.R is changed as indicated.
  output$testTree = renderUI({jstree("jstree1", jstree.obj(rV$nav))})
  observe({
    if(input$changeTree > 0) {  ### button pressed
      names(rV$newData)[[2]] = paste("root2 CHANGED!", input$changeTree)
    }
  })
  observe({
    showshinyalert(session, "alert_jstree1", 
                   paste0("You selected these items in the tree: ", 
                          paste0(input$jstree1, collapse = ", ")))
  })
})