##   NOTE: openTree() works if jstree is directly here,
##   but not if jstree is wrapped in uiOutput.
##   Likewise for the conditionalPanel and the shinyalert.
##   But then, it's not reactive.
## For the renderUI/uiOutput approach, 
## uncomment the line uiOutput(outputId = 'testTree')
## and comment out the line jstree("jstree1", jstree.obj(nav)).
## This will enable the reactivity (button press)
## but disable JS access (the openTree() call and the conditionalPanel conditions).
## Oddly, openTree() does work when pasted into the debug box 
## (then click "JS" and click "evaluate")

library(shiny)
library(shinysky)

shiny.trace = TRUE

nav = list(
  "Branch"=list("twig"=list("leafOnTwigA","leafOnTwigB" ),"leafOnBranch"),
  "Topleaf"
)

shinyUI(basicPage(
  div(class="well container-fluid", 
      uiOutput(outputId="debugTools"),  ### This is super-useful!
      div(class="row-fluid" 
          , jstree("jstree1", jstree.obj(nav))
          # , uiOutput(outputId = 'testTree')
          , div(class="span10"
                , shinyalert("alert_jstree1")
                , actionButton('changeTree', 'changeTree by changing the data')
                , br()
                , "below will show up only after selecting a leaf (input.jstree1.length > 0)"
                , br()
                , conditionalPanel('input.jstree1.length > 0', 
                                   'jstree1- something selected  ' %&% 
                                     textOutput('jstree1'))
                , br()
          )
          , conditionalPanel('openTree()', "OPENING TREE")
      )
  )
  , tags$script(
    'function openTree(){$("#jstree1").jstree("open_all");};
  $(document).ready(function() {$("#jstree1").ready(openTree);});'
  )
))