library(shiny)
library(shinysky)


shiny.trace = TRUE

shinyUI(basicPage(
  div(class="well container-fluid", 
      #div(class="row-fluid", 
          uiOutput(outputId="debugTools"),
          div(class="span2",
              uiOutput(outputId = 'testTree')
      #    )
              # '$(document).ready(function()
              # $("#jstree1").jstree("open_all"); );'
              # )
          , div(class="span10", shinyalert("alert_jstree1"))
          , actionButton('changeTree', 'changeTree by changing the data')
          , br()
          , "below will show up only after selecting a leaf (input.jstree1.length > 0)"
          , br()
          , conditionalPanel('input.jstree1.length > 0', 
                             'jstree1- something selected  ' %&% 
                               textOutput('jstree1'))
          , br()
#           , "below will show up all the time (input.testTree.length > 0  is true)"
#           , conditionalPanel('input.testTree.length > 0', 
#                              'testTree-always' %&% textOutput('testTree'))
      )
, conditionalPanel('openTree()', "OPENING TREE")
### If I add this, then the tree does open, but the conditionals are both true.
### WHAT? no longer opening.
  )
, tags$script(
  'function openTree(){$("#jstree1").jstree("open_all");};
  $("#jstree1").ready(openTree);'
)
))
