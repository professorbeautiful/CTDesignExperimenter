options(shiny.trace = F)  # cahnge to T for trace
require(shiny)
require(shinysky)


shinyServer(function(input, output, session) {
  # jstree
  source("../shinyInterface/debugTools.R", local=TRUE)
  
  rV = reactiveValues()
  rV$nav = list(
    "Branch"=list("twig"=list("leafOnTwigA","leafOnTwigB" ),"leafOnBranch"),
    "Topleaf"
  )
  output$testTree = renderUI({jstree("jstree1", jstree.obj(rV$nav))})
  observe({
    if(input$changeTree > 0) {
      rV$nav[[2]] = paste("CHANGED!", input$changeTree)
    }
  })
  observe({
    showshinyalert(session, "alert_jstree1", paste0("You selected these items in the tree: ", 
                                                    paste0(input$jstree1, collapse = ", ")))
  })
})