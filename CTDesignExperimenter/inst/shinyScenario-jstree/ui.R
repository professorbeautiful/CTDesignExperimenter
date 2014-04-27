### shinyScenario-jstree 
### See /Users/Roger/Library/R/3.0/library/shinysky/examples/6_jstree/

require(shinysky)
"%&%" = function (a, b)   paste(a, b, sep = "") 
scenarioTree = makeTree("full")

# nav = list(Branch = list(twig = list("leafOnTwigA", "leafOnTwigB"), 
#                          "leafOnBranch"), 
#            "Topleaf")

myTreeObj = jstree.obj(scenarioTree)
# myTreeObj[["children"]][[1]][[11]][[1]] = 
#   'li class="jstree-open"'
#myTree = jstree.local("jstree1", myTreeObj, '.jstree("open_all")')
myTree = jstree.local("jstree1", myTreeObj, '')
# Fails.  The closing tag is also set to class="jstree-open"
# Must be a special [[<- method.
shinyUI(
  basicPage(
    headerPanel("shinyScenario-jstree"), 
    br(), br(),
    h4("startup 3"), 
    div(class = "well container-fluid", 
        div(class = "row-fluid", 
            div(class = "row-fluid",
                div(class = "span2", 
                    myTree) , 
                div(class = "span10", 
                    shinyalert("alert_jstree1"))
            ),
            hr(),
            ### Seems to have to be at the end.
            tags$script(
              #  '$("#jstree1").open_all();'
              'function openTree(){
                            $("#jstree1").jstree("open_all");
                            };
              $(document).ready(openTree);')
        )
    )
  )
)
