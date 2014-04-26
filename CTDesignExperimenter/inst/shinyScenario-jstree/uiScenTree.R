### shinyScenario-jstree 
### See /Users/Roger/Library/R/3.0/library/shinysky/examples/6_jstree/

require(shinysky)

scenarioTree = makeTree("full")

# nav = list(Branch = list(twig = list("leafOnTwigA", "leafOnTwigB"), 
#                          "leafOnBranch"), 
#            "Topleaf")

shinyUI(basicPage(headerPanel("shinyScenario-jstree"), br(), br(),
                  h4("Treeview (based on jstree)"), 
                  div(class = "well container-fluid", 
                      div(class = "row-fluid", 
                          div(class = "row-fluid",
                              div(class = "span2", 
                                  jstree("jstree1", jstree.obj(scenarioTree))), 
                              div(class = "span10", shinyalert("alert_jstree1"))
                              ))
                      )))
