### shinyScenario-jstree 
### See /Users/Roger/Library/R/3.0/library/shinysky/examples/6_jstree/

"%&%" = function (a, b)   paste(a, b, sep = "") 

# myTree is created in global.R
OpenSesame = '$("#jstree1").jstree("open_all");'
tagToOpenTree =
  tags$script(paste0(
    'function openTree(){'
    , OpenSesame 
    , '                   };
       $(document).ready(openTree);'))

scenarioPanel = tabPanel("scenario",
                         #div(class="row-fluid span1",
                         uiOutput(outputId="debugTools")
                         #                             )
                         ,
                         textInput(inputId="scenarioName",  
                                   label="scenario name"),
                         div(class="row-fluid span3",
                             actionButton(inputId="btnRenameScen", label="rename scenario", styleclass = "success"),
                             actionButton(inputId="btnSaveScen" , label="save scenario", styleclass = "success"),
                             actionButton(inputId="btnFindScen" , label="find/replace scenario", styleclass = "success"),
                             actionButton(inputId="btnAddScen" , label="add scenario", styleclass = "success"),
                             hr()),
                         textOutput("selectedNode"),
                         conditionalPanel(condition = #'$( "select option:selected".length > 0 )',
                                            #  '!(typeof input.jstree1 === "undefined") && input.jstree1.length > 0',
                                          #  'output.numberSelected', ## fails
                                             'output.selectedNode', # this works!
                                          hr(),
                                          actionButton(inputId="btnCloneInsert" , label="Clone insert", styleclass = "success"),
                                          actionButton(inputId="btnEditInsert" , label="Edit insert", styleclass = "success"),
                                          textOutput("nSelectedText"),
                                          hr()),
                         ###  Buttons when > 1 items are selected:
                         conditionalPanel(condition = 'output.numberSelected == 2', 
                                          hr(),
                                          actionButton(inputId="btnSaveListOfInserts" , label="btnSaveListOfInserts", styleclass = "success"),
                                          actionButton(inputId="btnEditInsert" , label="Edit insert", styleclass = "success"),
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
