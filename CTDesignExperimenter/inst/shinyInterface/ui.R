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

conditionPanel1 = conditionalPanel(condition = #'$( "select option:selected".length > 0 )',
                                     #  '!(typeof input.jstree1 === "undefined") && input.jstree1.length > 0',
                                     #  'output.numberSelected', ## fails. why?
                                     #   'output.selectedNode', # this works better
                                     'input.jstree1.length == 1', #perfect
                                   hr(),
                                   actionButton(inputId="btnRemoveInsert" , label="Remove insert", styleclass = "success"),
                                   actionButton(inputId="btnCloneInsert" , label="Clone insert", styleclass = "success"),
                                   actionButton(inputId="btnEditInsert" , label="Edit insert", styleclass = "success"),
                                   textOutput("selectedNode"),
                                   #                                           textOutput("nSelectedText"),
                                   hr())
conditionPanelMoreThan1 = 
  ###  Buttons when > 1 items are selected:
  conditionalPanel(condition =  'input.jstree1.length > 1',
                   # 'output.moreThanOneSelected == true', 
                   hr(),
                   actionButton(inputId="btnSaveListOfInserts" , label="btnSaveListOfInserts", styleclass = "success"),
                   hr())

scenarioPanel = tabPanel("scenario",
                         #div(class="row-fluid span1",
                         uiOutput(outputId="debugTools")
                         #                             )
                         ,
                         textInput(inputId="scenarioName",  
                                   label="scenario name",
                                   value=currentScenario@name),
                         div(class="row-fluid span3",
                             actionButton(inputId="btnCloneScen", label="Clone and save scenario", styleclass = "success"),
                             actionButton(inputId="btnFindScen" , label="Find &replace scenario", styleclass = "success"),
                             actionButton(inputId="btnAddScen" , label="Add scenario to experiment", styleclass = "success"),
                             hr()),
                         conditionPanel1,
                         conditionPanelMoreThan1,
                         myTree,
                         tagToOpenTree 
)

shinyUI(
  navbarPage(
    headerPanel(h4("CTDE: Clinical trial design experimenter")), 
#    div(class = "well container-fluid", 
#        tabsetPanel(id="panelSet", type="pills",
                    scenarioPanel,
                    tabPanel("one run", 
                             "Display results from a single CT run for the selected scenario."),
                    tabPanel("criteria"),
                    tabPanel("evaluation"))
#    )
#  )
)
