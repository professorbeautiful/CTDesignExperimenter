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
      actionButton(inputId="btnCloneScen", 
                   label="Clone and save scenario", 
                   css.class = "BLOCKlevel"),
      actionButton(inputId="btnFindScen" , 
                   label="Find &replace scenario", css.class = "BLOCKlevel"),
      actionButton(inputId="btnAddScen" , 
                   label="Add scenario to experiment", css.class = "BLOCKlevel"),
      shinyalert(id = "cloneScen"),
      hr())
)

outputPreamble <<- 'window.Shiny.shinyapp.$bindings.'

treeSelectionDepthJSfunction = singleton(tags$script(
  "function treeSelectionDepth() { return " %&% outputPreamble %&% 
    " treeSelectionDepth.el.textContent; }; "))
treeSelectionTextJSfunction = singleton(tags$script(
  "function treeSelectionText() { return " %&% outputPreamble %&% 
    " treeSelectionText.el.textContent; };
   function is_needed() {return treeSelectionText().search('needs:') == 0;}; 
   function is_code() {return treeSelectionText().search('generator code:') == 0;}; 
   function is_param() {return treeSelectionText().search('param:') == 0;}; 
   function is_provision() {return treeSelectionText().search('provides:') == 0;}; 
  "))

conditionPanel_1_insert = conditionalPanel(condition = 
       '($("#jstree1").jstree().get_selected().length == 1) & (treeSelectionDepth() == 2)', 
     ## THE FOLLOWING expression shows #j1_2 etc:
     ## 'alert($("#jstree1").jstree().get_selected().toString())',  
     actionButton(inputId="btnRemoveInsert" , label="Remove insert", css.class = "treeclass_2"),
     actionButton(inputId="btnCloneInsert" , label="Clone insert", css.class = "treeclass_2"),
     actionButton(inputId="btnEditInsert" , label="Edit insert", css.class = "treeclass_2"),
     actionButton(inputId="btnAddRequirement" , label="Add a needed Variable", css.class = "treeclass_2"),
     hr()
)

###  TODO:   this condition is not working correctly.
conditionPanel_1_vg_code = 
  conditionalPanel(condition = 
                     '($("#jstree1").jstree().get_selected().length == 1) & (treeSelectionDepth() == 3)
                   & is_code()', 
                   actionButton(inputId="btnEditCode" , label="Edit code", css.class = "treeclass_3"),
                   hr()
  )
conditionPanel_1_needed_var = 
  conditionalPanel(condition = 
                     '($("#jstree1").jstree().get_selected().length == 1)
                    & (treeSelectionDepth() == 3)
                    & is_needed()', ### Requirement
                   HTML("Selected one variable."),
                   actionButton(inputId="btnRemoveVariable" , label="Remove Variable", css.class = "treeclass_3"),
                   actionButton(inputId="btnCloneVariable" , label="Clone Variable", css.class = "treeclass_3"),
                   actionButton(inputId="btnEditVariable" , label="Edit Variable", css.class = "treeclass_3"),
                   hr()
  )
conditionPanel_1_generator_code = 
  conditionalPanel(condition = 
                     '($("#jstree1").jstree().get_selected().length == 1)
                   & (treeSelectionDepth() == 3)
                   & is_code()', ### Generator code.
                   HTML("Selected one variable."),
                   actionButton(inputId="btnRemoveVariable" , label="Remove Variable", css.class = "treeclass_3"),
                   actionButton(inputId="btnCloneVariable" , label="Clone Variable", css.class = "treeclass_3"),
                   actionButton(inputId="btnEditVariable" , label="Edit Variable", css.class = "treeclass_3"),
                   hr()
  )
conditionPanel_moreThan1_insert = 
  conditionalPanel(
    '$("#jstree1").jstree().get_selected().length > 1
    & (treeSelectionDepth() == 2)',
    actionButton(inputId="btnSaveListOfInserts" , label="btnSaveListOfInserts", css.class = "treeclass_2"),
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
                         conditionPanel_1_vg_code,
                         conditionPanel_1_needed_var,
                         conditionPanel_moreThan1_insert,
                         conditionalPanel(condition = 
                                            '$("#jstree1").jstree().get_selected().length > 0',
                                          div(class="row-fluid",
                                              div(class="span1"
                                                  ,tags$em("Selection")
                                              ),
                                              div(class="span1" 
                                                  ,textOutput('treeSelectionDepth')
                                              ),
                                              div(class="span9"
                                                  ,textOutput('treeSelectionText')
                                              )
                                            ) 
                         ),
                         div(style="overflow:auto; height:800px", 
                             myTree),
                         tagToOpenTree 
)

editorPanel = #conditionalPanel(condition = 'true',
  # Sadly, cannot put a conditionalPanel in a tabsetPanel.
  tabPanel("Editors",
           conditionalPanel(condition = 'treeSelectionDepth()==2',
                            uiOutput("insertEditorUI")),
           conditionalPanel(condition = 'treeSelectionDepth()==3
                            && (is_needed() || is_provision() ) ',
                            uiOutput("varEditorUI"))
  )

CSSreference = singleton(tags$head(tags$link(href = "ctde.css", 
                                             rel = "stylesheet")))
# getLevelOfSelection = singleton(tags$head(tags$script(
#   var levelOfSelection;
#   
#   )))


myJSincludes = tagList(
  includeScript("www/ctde-types.js") ## It does find this !
  , includeScript("www/ss-jstree.js")  # and this.
  , CSSreference ### OK. Works (for text colors)
  , leafDepthJSfunction
  , leafTextJSfunction
##  Fiddling with jqueryUI popups:
popupJSincludes = tagList(
  includeCSS("www/css/jquery-ui.css"),  
  includeScript("www/js/jquery-ui.js")
  #, includeScript("www/js/varEditPopup.js")
)
# also see jQuery-dialogextend,  https://github.com/ROMB/jquery-dialogextend

## The context menu appears with the standard menu, not in place of.

shinyUI(
  navbarPage(
    inverse=TRUE,
    id="tabsetID",
    title = 
      h4("CTDE: Clinical trial design experimenter"),
    header=tagList(myJSincludes,
                   hr(),
                   popupJSincludes,
                   # textInput("console", "Enter an R Command"),
                   # uiOutput("varEditPopup"), ### Alternative to using tabsets.
                   uiOutput(outputId="debugTools"),
                   hr()),
    # message-handler code causes hang.
    #       singleton(
    #         tags$head(tags$script(src = "message-handler.js"))
    #       ),
    scenarioPanel,
    editorPanel,
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
