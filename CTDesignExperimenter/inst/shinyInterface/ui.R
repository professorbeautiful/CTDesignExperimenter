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


conditionPanelNoneSelected = conditionalPanel(
  '$("#jstree1").jstree().get_selected().length == 0',  #This works!! 0 1 2 etc.
  div(class="row-fluid span3",
      actionButton(inputId="btnCloneScen", label="Clone and save scenario", styleclass = "success"),
      actionButton(inputId="btnFindScen" , label="Find &replace scenario", styleclass = "success"),
      actionButton(inputId="btnAddScen" , label="Add scenario to experiment", styleclass = "success"),
      shinyalert(id = "cloneScen"),
      hr())
)
  
outputPreamble <<- 'window.Shiny.shinyapp.$bindings.'

leafDepthJSfunction = singleton(tags$script(
  "function leafDepth() { return " %&% outputPreamble %&% 
    " treeSelectionDepth.el.textContent; }; "))  

conditionPanel_1_insert = conditionalPanel(condition = 
  'input.jstree1.length == 1 & (leafDepth() == 2)', 
    #  '($("#jstree1").jstree().get_selected().length == 1)', # & (" %&%
    #  outputPreamble %&% 'treeSelectionDepth == 2)',
    ## THE FOLLOWING expression shows #j1_2 etc:
    ## 'alert($("#jstree1").jstree().get_selected().toString())',  
    actionButton(inputId="btnRemoveInsert" , label="Remove insert", styleclass = "success"),
    actionButton(inputId="btnCloneInsert" , label="Clone insert", styleclass = "success"),
    actionButton(inputId="btnEditInsert" , label="Edit insert", styleclass = "success"),
    actionButton(inputId="btnAddRequirement" , label="Add a needed Variable", styleclass = "success"),
    # NOT WORKING  textOutput("selectedNode"),
    hr()
)
# conditionPanel_1_variable = 
#   conditionalPanel(condition = 
#                      "($('#jstree1').jstree().get_selected().length == 1) & (" %&%
#                      outputPreamble %&% 'treeSelectionDepth == 3)',
#                    HTML("Selected one variable."),
#                    textOutput("selectedNode"),
#                    hr()
#   )
conditionPanelMoreThan1 = 
  ###  Buttons when > 1 items are selected:
  conditionalPanel(
    '$("#jstree1").jstree().get_selected().length > 1',  #This works!! 0 1 2 etc.
    # 'output.moreThanOneSelected == true', 
                   actionButton(inputId="btnSaveListOfInserts" , label="btnSaveListOfInserts", styleclass = "success"),
                   textOutput("selectedNodes"),
                   hr()
  )



scenarioPanel = tabPanel("Current scenario",
                         #div(class="row-fluid span1",
                         textInput(inputId="scenarioName",  
                                   label="scenario name",
                                   value=currentScenario@name),
                         conditionPanelNoneSelected,
                         conditionPanel_1_insert,
#                         conditionPanel_1_variable,
                         conditionPanelMoreThan1,
                         div(style="overflow:auto; height:800px", 
                         myTree),
                         tagToOpenTree 
)

variableEditorPanel = #conditionalPanel(condition = 'true',
  # Sadly, cannot put a conditionalPanel in a tabsetPanel.
                        tabPanel("Edit var",
                          HTML("This is the variableEditor panel."),
                          actionButton(inputId="btnCreateVar" , 
                                       label="Create new Variable", styleclass = "success")
                          #,conditionalPanel(condition = 'input.btnCreateVar')
                          
                        )
                                       #)

CSSreference = singleton(tags$head(tags$link(href = "ctde.css", 
                                             rel = "stylesheet")))
# getLevelOfSelection = singleton(tags$head(tags$script(
#   var levelOfSelection;
#   
#   )))


myJSincludes = tagList(
  CSSreference ### OK. Works (for text colors)
  , includeScript("www/ctde-types.js") ## It does find this !
  , includeScript("www/ss-jstree.js")  # and this.
  , leafDepthJSfunction
)
## The context menu appears with the standard menu, not in place of.
#  scriptToGetDepths?

shinyUI(
  navbarPage(
    inverse=TRUE,
    id="tabsetID",
    title = 
      h4("CTDE: Clinical trial design experimenter"),
    header=tagList(myJSincludes,
                   hr(),
                   uiOutput(outputId="debugTools"),
                   hr(),
                   conditionalPanel(condition = 'true',
                                    textOutput('treeSelectionDepth' 
                                              ))),
    # message-handler code causes hang.
    #       singleton(
    #         tags$head(tags$script(src = "message-handler.js"))
    #       ),
    scenarioPanel,
    navbarMenu("Editors",
               variableEditorPanel,
               tabPanel("Create new Variable"),
               tabPanel("Create new Insert")
    ),
#    insertEditorPanel,
    tabPanel("One CT run", 
             "Display results from a single CT run for the selected scenario.",
             hr(),
             actionButton(inputId="btnRunOne", label="Run one CT", styleclass = "success")
             , uiOutput("oneRunResults")
    ),
    tabPanel("Criteria",
             "Criteria will be selected and created here."),
    tabPanel("Experiment",
             "A table, scenarios by criteria."
             , actionButton(inputId="btnRunExperiment" , label="Run Experiment", styleclass = "success")
             , tableOutput("experimentTableOut")
    )
  )
)
