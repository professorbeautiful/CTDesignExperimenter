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

conditionPanelNoneSelected = conditionalPanel(condition = 'input.jstree1.length == 0',
                                              div(class="row-fluid span3",
                                                  actionButton(inputId="btnCloneScen", label="Clone and save scenario", styleclass = "success"),
                                                  actionButton(inputId="btnFindScen" , label="Find &replace scenario", styleclass = "success"),
                                                  actionButton(inputId="btnAddScen" , label="Add scenario to experiment", styleclass = "success"),
                                                  hr())
)
                                              
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


# This does not add depths data to input.
# scriptToGetDepths = tagList(singleton(tags$head(tags$script(
#                             'var depths; 
# $(document).ready(function(){
#    $("#jstree1").on("select_node.jstree", function(e,data) {
#       var inst=data.inst;
#       var selected=inst.get_selected();
#       depths = selected().length;
#       alert("depths = ", depths);
#       input.depths = depths;
# //      var id=selected.attr("id");
# //      var name=selected.prop("tagName");
# //      console.log(name,id,depths);
#   });
# })'
# ))))

scenarioPanel = tabPanel("Current scenario",
                         #div(class="row-fluid span1",
                         textInput(inputId="scenarioName",  
                                   label="scenario name",
                                   value=currentScenario@name),
                         conditionPanelNoneSelected,
                         conditionPanel1,
                         conditionPanelMoreThan1,
                         div(style="overflow:auto; height:800px", 
                         myTree),
                         tagToOpenTree 
)

CSSreference = singleton(tags$head(tags$link(href = "ctde.css", 
                                             rel = "stylesheet")))
shinyUI(
#  scriptToGetDepths,
  navbarPage(
    title = 
      h4("CTDE: Clinical trial design experimenter"),
    header=tagList(CSSreference,
                   hr(),
                   uiOutput(outputId="debugTools"),
                   hr()),
    # message-handler code causes hang.
    #       singleton(
    #         tags$head(tags$script(src = "message-handler.js"))
    #       ),
    scenarioPanel,
    tabPanel("One CT run", 
             "Display results from a single CT run for the selected scenario.",
             actionButton(inputId="btnRunOne" , label="Run one CT", styleclass = "success")
    ),
    tabPanel("Criteria",
             "Criteria will be selected and created here."),
    tabPanel("Experiment",
             "A table, scenarios by criteria."
             , actionButton(inputId="btnRunExperiment" , label="Run Experiment", styleclass = "success")
             , dataTableOutput("experimentTable")
    )
  )
)
