### shinyScenario-jstree 
### See /Users/Roger/Library/R/3.0/library/shinysky/examples/6_jstree/

require(shinysky)
"%&%" = function (a, b)   paste(a, b, sep = "") 

scenarioTree = makeTree("full", defaultScenario)
myTreeObj = jstree.obj(scenarioTree)
myTree = jstree("jstree1", myTreeObj)
OpenSesame = '$("#jstree1").jstree("open_all");'
tagToOpenTree =
  tags$script(paste0(
    'function openTree(){'
    , OpenSesame 
    , '                   };
       $(document).ready(openTree);'))

scenarioPanel = tabPanel("scenario",
                         textInput(inputId="scenarioName",  
                                   value = defaultScenario@name,
                                   label="scenario name"),
                         div(class="row-fluid span3",
                             actionButton(inputId="btnRenameScen", label="rename scenario", styleclass = "success"),
                             actionButton(inputId="btnSaveScen" , label="save scenario", styleclass = "success"),
                             actionButton(inputId="btnFindScen" , label="find/replace scenario", styleclass = "success"),
                             actionButton(inputId="btnAddScen" , label="add scenario", styleclass = "success"),
                             hr()),
                         myTree,
                         tagToOpenTree 
)

shinyUI(
  basicPage(
    headerPanel(h4("CTDE: Clinical trial design experimenter")), 
    div(class = "well container-fluid", 
        tabsetPanel(id="panelSet", type="pills",
                    scenarioPanel,
                    tabPanel("one run", 
                             "Display results from a single CT run for the selected scenario."),
                    tabPanel("criteria"),
                    tabPanel("evaluation"))
    )
  )
)
