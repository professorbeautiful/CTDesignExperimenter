### shinyScenario-jstree 
### See /Users/Roger/Library/R/3.0/library/shinysky/examples/6_jstree/


require(shinysky)

### Convenience function:
"%&%" = function (a, b)   paste(a, b, sep = "") 

scenarioTree = makeTree("full")

# nav = list(Branch = list(twig = list("leafOnTwigA", "leafOnTwigB"), 
#                          "leafOnBranch"), 
#            "Topleaf")

myTreeObj = jstree.obj(scenarioTree)
# myTreeObj[["children"]][[1]][[11]][[1]] = 
#   'li class="jstree-open"'
# Fails.  The closing tag is also set to class="jstree-open"
# Must be a special [[<- method.

#myTree = jstree.local("jstree1", myTreeObj, '.jstree("open_all")')
#That works. Now we try another method.

## This works too, and does not require jstree.local
OpenSesame = '$("#jstree1").jstree("open_all");'
tagToOpenTree =
  tags$script(paste0(
    'function openTree(){'
    , OpenSesame 
    , '                   };
              $(document).ready(openTree);'))

myTree = jstree("jstree1", myTreeObj)

shinyUI(
  basicPage(
    headerPanel("shinyScenario-jstree"), 
    br(), br(),
    h4("startup 4"), 
    div(class = "well container-fluid", 
        div(class = "row-fluid", 
            div(class = "row-fluid",
                div(class = "span2", 
                    myTree) , 
                div(class = "span10", 
                    shinyalert("alert_jstree1"))
            ),
            hr(),
            tagToOpenTree
        )
    )
  )
)
