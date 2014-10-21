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
require(shinyTree)

shiny.trace = TRUE

nav = list(
  "Branch"=list("twig"=list("leafOnTwigA","leafOnTwigB" ),"leafOnBranch"),
  "Topleaf"
)

shinyUI(basicPage(
  div(class="well container-fluid", 
      uiOutput(outputId="debugTools"),  ### This is super-useful!
      div(class="row-fluid" 
          # , jstree("jstree1", jstree.obj(nav))
          # , uiOutput(outputId = 'testTree')
          , shinyTree('newTree', selected='treeSel')
          , uiOutput(outputId = 'testText')
          , div(class="span10"
                , includeScript("www/ctde-types.js") 
                , shinyalert("alert_jstree1")
                , actionButton('changeTree', 'changeTree by changing the data')
                , br()
                , "The newTree:"
                #, verbatimTextOutput(outputId = 'newTree')
                , "Currently Selected:"
                , verbatimTextOutput("selTxt")
                , "below will show up only after selecting a leaf with text vg_SampleSizeMax_2"
                , br()
                , conditionalPanel("window.Shiny.shinyapp.$bindings.selTxt.el.firstChild.nodeValue
                                   == 'vg_SampleSizeMax_2'", 
                                   paste('vg_SampleSizeMax_2 was selected  '))
                , br()
                , hr()
                , "below will show up only after changing the testText"
                , br()
                , conditionalPanel('selTxt != "Topleaf"', 
                                   paste('testText is now  ',
                                         textOutput('testText')))
                , br()
          )
          , conditionalPanel('openTree()', "OPENING TREE")
      )
  )
  , tags$script(
    'function openTree(){$("#newTree").jstree("open_all");};
  $(document).ready(function() {$("#newTree").ready(openTree);});'
  )
))