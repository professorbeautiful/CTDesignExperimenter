require(shiny)
require(shinysky)
require(shinyTree)
assignInNamespace("listToTags", myListToTags, ns="shinyTree")

shinyServer(function(input, output, session) {

  source("debugTools.R", local=TRUE)    ### This is super-useful!
  
  rV = reactiveValues()
  rV$nav = list(
    "Branch"=list(
      "twig"=list("leafOnTwigA"="leafOnTwigATEXT","leafOnTwigB" ),
      "leafOnBranch"),
    ThisIsTopleaf="Topleaf"
  )
  
  
  rV$scenarioTree = scenarioTree
  # "st" is shinyTree?
  
  openedNode = function(theNode) {
    structure(theNode, stopened=TRUE)
  }
  rV$newData =     list(
    root1 = structure("StuffInRoot1", stselected=TRUE, sticon="signal"),
    root2 = structure(
      list(
        SubListA = 
          openedNode(list(leaf1 = "", leaf2 = "", leaf3="")),
        SubListB = structure(list(leafA = "", leafB = ""), stdisabled=TRUE)
      ),
      stopened=TRUE    )
  )
  ## The next line has no effect unless ui.R is changed as indicated.
  output$testTree = renderUI({jstree("jstree1", jstree.obj(rV$nav))})
  output$testText = renderText({ rV$nav[[2]] })
  
  #  output$newTree = renderTree( { rV$newData    } )
  myRenderTree =
    function (expr, env = parent.frame(), quoted = FALSE) 
    {
      func <- exprToFunction(expr, env, quoted)
      return(function(shinysession, name, ...) {
        cat("====myRenderTree====\n")
        tree <- func()
        HTML(as.character(expr))
      })
    }
  
  output$newTree = myRenderTree( {myjstree.obj(rV$scenarioTree)} )
  
  
  output$selTxt <- renderText({
    sel <- input$treeSel
    if (is.null(sel)){
      "None"
    } else{
      sel
    }    
  })
  observe({
    if(input$changeTree > 0) {  ### button pressed
      #names(rV$newData)[[2]] = paste("root2 CHANGED!", input$changeTree)
      names(rV$scenarioTree)[[2]] = paste("BeginClinicalTrial CHANGED!", input$changeTree)
    }
  })
  observe({
    showshinyalert(session, "alert_jstree1", 
                   paste0("You selected these items in the tree: ", 
                          paste0(input$jstree1, collapse = ", ")))
  })
  observe({
    showshinyalert(session, "alert_jstree1", 
                   paste0("You selected these items in the tree: ", 
                          paste0(input$testText, collapse = ", ")))
  })
})