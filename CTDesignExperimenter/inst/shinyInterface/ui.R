
### shinyScenario-jstree 
### See /Users/Roger/Library/R/3.0/library/shinysky/examples/6_jstree/

"%&%" = function (a, b)   paste(a, b, sep = "") 

conditionPanelNoneSelected = conditionalPanel(
  '$("#jstreeScenario").jstree().get_selected().length == 0',  #This works!! 0 1 2 etc.
  ####  To UNSELECT, Cmd-click on the selected node.
  div(class="row-fluid span3",
      actionButton(inputId="btnNewScenario", 
                   label="New Scenario"),
      tagAppendAttributes(tag = a(
        actionButton(inputId="btnSearchScenario", 
                     label="Search swapmeet & load Scenario"),
        href="#idSearchScenario")
      ),
      actionButton(inputId="btnAddScen", 
                   label="Add Scenario to Experiment"),
      actionButton(inputId="btnSaveScenarioToGlobalEnv", 
                   label="Save Scenario to GlobalEnv"),
      actionButton(inputId="btnWriteScenarioToSwapmeet", 
                   label="Write Scenario to Swapmeet"),
      hr())
)

conditionPanelBlockSelected = conditionalPanel(
  '($("#jstreeScenario").jstree().get_selected().length == 1) & (treeSelectionDepth() == 1)', 
  div(class="row-fluid span3",
      img(src="BLOCK32.png"),
      actionButton(inputId="btnWriteBlockToSwapmeet", 
                   label="Write this block to Swapmeet", 
                   css.class = "BLOCKlevel"),
      actionButton(inputId="btnSaveBlockToGlobalEnv", 
                   label="Save scenario to GlobalEnv", 
                   css.class = "BLOCKlevel"),
      actionButton(inputId="btnFindBlock" , 
                   label="Find & replace block", css.class = "BLOCKlevel"),
      hr())
)

outputPreamble <<- 'window.Shiny.shinyapp.$bindings.'
inputPreamble <<- 'window.Shiny.shinyapp.$inputValues'

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

conditionPanel_1_insert = 
  conditionalPanel(
    condition = 
      '($("#jstreeScenario").jstree().get_selected().length == 1) & (treeSelectionDepth() == 2)', 
    ## THE FOLLOWING expression shows #j1_2 etc:
    ## 'alert($("#jstreeScenario").jstree().get_selected().toString())', 
    img(src="Insert32.png"),
    actionButton(inputId="btnRemoveInsert" , label="Remove insert", css.class = "treeclass_2"),
    actionButton(inputId="btnCloneInsert" , label="Clone insert", css.class = "treeclass_2"),
    actionButton(inputId="btnEditInsert" , label="Edit insert", css.class = "treeclass_2"),
    #actionButton(inputId="btnAddRequirement" , label="Add a needed Variable", css.class = "treeclass_2"),
    hr()
)

###  TODO:   this condition is not working correctly.
conditionPanel_1_vg_code = 
  conditionalPanel(condition = 
                     '($("#jstreeScenario").jstree().get_selected().length == 1) & (treeSelectionDepth() == 3)
                   & is_code()', 
                   actionButton(inputId="btnEditCode" , label="Edit code", css.class = "treeclass_3"),
                   hr()
  )
conditionPanel_1_needed_var = 
  conditionalPanel(condition = 
                     '($("#jstreeScenario").jstree().get_selected().length == 1)
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
                     '($("#jstreeScenario").jstree().get_selected().length == 1)
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
    '$("#jstreeScenario").jstree().get_selected().length > 1
    & (treeSelectionDepth() == 2)',
    actionButton(inputId="btnSaveListOfInserts" , label="btnSaveListOfInserts", css.class = "treeclass_2"),
    textOutput("selectedNodes"),
    hr()
  )

scenarioPanel = tabPanel(
  title="Current scenario",
  h3("Current scenario"),
  #div(class="row-fluid span1",
  #includeHTML("jstreeTestContent.html"),
  textInput(inputId="scenarioName",  
            label=em("Scenario name"),
            value=currentScenario@name),
  hr(),
  conditionPanelNoneSelected,
  conditionPanelBlockSelected,
  conditionPanel_1_insert,
  conditionPanel_1_vg_code,
  conditionPanel_1_needed_var,
  conditionPanel_moreThan1_insert,
  conditionalPanel(condition = 
                     '$("#jstreeScenario").jstree().get_selected().length > 0',
                   #$("#modalContents").dialog({bgiframe: true, height: 140, modal: true});
                   div(class="row-fluid", 
                       ### style='display:none'?? 
                       ###  We only need these textOutputs to calculate JS conditions.
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
  )
  , tagAppendAttributes(em(
          textOutput("SCENARIO_TREE_label"), 
        class="clickMeToClearSelection"))
  # THE FOLLOWING div LINE IS RESPONSIBLE FOR NOT SHOWING UP IN CHROME AND SAFARI
  # Specifically, it is overflow:auto.  Also overflow:scroll breaks it.
  #, div(style="height:800px;" 
  #, div(style="overflow:scroll;height:400px;background-color:lightgrey"
  ### myTree responds to JS (tagToOpenTree, and conditionals)
  ### The uiOutput element does not.
  #  , uiOutput(outputId = 'jstreeScenarioOutput')  
  #   , jstree("jstreeScenario",  
  #           myjstree.obj(list("a","b"))
  #           # myjstree.obj(makeTree(scenario=currentScenario, "full"))
  #     )
  # 
  #   #) 
  #   , tags$script("  var jsonMessage;
  #                 $('#jstreeScenario').jstree(
  #                   { core: { data: function (node, cb) { cb(jsonMessage); } }
  #               });")
  , includeHTML("jstreeScenarioContent.html")
  , tags$script('ss_jstree.subscribe(tree(), function() { fixColors(); }); ')
  , tagToOpenTree  ## This tag MUST be AFTER myTree! Why? (Can be inside the div or not)
  , uiOutput('scenarioSearchTable')
  #   , div(class='col-6',
  #         conditionalPanel(
  #           "input.btnSearchScenario > 0", 
  #           hr(),
  #           tagAppendAttributes(a(""), id="idSearchScenario"),
  #           h3("Click on the radiobutton to load the Scenario into the template above."),
  #           HTML('<div id="chooseScenario" class="control-group shiny-input-radiogroup">
  #                    <label class="control-label" for="chooseScenario">Swapmeet Scenarios</label>'),
  #           dataTableOutput("allScenariosTable"),
  #           HTML('</div>')
  #         )
  #   )
  
  #  )  ###  end of overflow div..  Fails in chrome and safari
)

CSSreference = singleton(tags$head(tags$link(href = "ctde.css", 
                                             rel = "stylesheet")))
# getLevelOfSelection = singleton(tags$head(tags$script(
#   var levelOfSelection;
#   
#   )))


myJSincludes = tagList(  ### Goes into the header.
  includeScript("www/jstree.min.js"),
  includeScript("www/ss-jstree.js")  # and this.  
  , CSSreference ### OK. Works (for text colors)
  , treeSelectionDepthJSfunction
  , treeSelectionTextJSfunction
  , singleton(tags$script("var outputPreamble = '" %&% outputPreamble %&% "';"))
  , singleton(tagToOpenTree)
  , singleton(tags$script( HTML( #### HTML prevents conversion of & into &amp; .
    ' $(document).bind("click", 
        function (e) { 
          clickEvent = e; // to make available globall
          if(window.Shiny.shinyapp.$inputValues.tabsetID=="Current scenario" 
              && !$(e.target).parents(".jstree:eq(0)").length
              && $(e.target).attr("id")=="SCENARIO_TREE_label") { 
                      $("#jstreeScenario").jstree("deselect_all");
                      console.log("DESELECTED");
                } 
        }); '
    )))
  ### id = $('#jstreeScenario').jstree('get_selected'); $("#" + id[0]) picks up the (first) selected node itself.
  # singleton(tags$head("<script src='www/ctde-types.js'> </script> ")),
  # singleton(tags$head("<script src='www/priority.js'> </script> ")),
  #  includeScript("www/ctde-types.js"), ## It does find this !
  #   includeScript("www/priority.js"), 
  # includeScript("https://code.jquery.com/jquery-2.1.1.min.js"),
  #  includeScript("http://static.jstree.com/3.0.8/assets/dist/jstree.min.js"),
  #  includeScript("http://cdnjs.cloudflare.com/ajax/libs/json2/20110223/json2.min.js"),
  #   includeScript("www/jquery-2.1.1.min.js"),
  #   includeScript("www/json2.min.js"),
  #  , includeScript("http://cdn.datatables.net/1.10.1/js/jquery.dataTables.min.js")
  # includeScript("www/html-to-json.js")
)

##  Fiddling with jqueryUI popups:
popupJSincludes = tagList(
  includeCSS("www/css/jquery-ui.css"),  
  includeScript("www/js/jquery-ui.js")
  #, includeScript("www/js/varEditPopup.js")
)
# also see jQuery-dialogextend,  https://github.com/ROMB/jquery-dialogextend

## The context menu appears with the standard menu, not in place of.

shinyUI(
  div(h1("CTDE: Clinical trial design experimenter"),  hr(),
      navbarPage(
        inverse=TRUE,
        id="tabsetID",
        #        title = div(h2("CTDE: Clinical trial design experimenter"), hr()),
        title="",
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
        # editorPanel,
        # tabPanel("Scenario Editor", uiOutput("scenarioEditorUI")),
        
        tabPanel("Insert Editor", uiOutput("insertEditorUI")),
        
        tabPanel("Variable Editor", uiOutput("varEditorUI")),
        
        tabPanel("One CT run", 
                 h2("Display results from a single CT run for the selected scenario."),
                 hr()
                 , uiOutput("oneRunHeader")
                 , actionButton(inputId="btnOneRun", label="Run one CT", styleclass = "success")
                 , uiOutput("oneRunSummaries")
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
)
