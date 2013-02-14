## see  shinyuiTest.R and shinyserverTest.R for originl wxmple.

require(RBioinf)

specClassNames = c(base="BaseCharModelSpecifier",
                   pop="PopModelSpecifier",
                   outcome="OutcomeModelSpecifier",
                   design="DesignSpecifier",
                   eval="EvalSpecifier")
shortName = function(specifierName)
  names(classNames)[match(specifierName, classNames)]
addAttr = function(x, att, value) {attr(x, att) = value; x }

shinyUI(pageWithSidebar(  
  headerPanel=
#     addAttr("html", TRUE,
# x="<head>\n  <title>Clinical Trial Experiment Platform</title>\n</head>\n<div class=\"span12\" style=\"padding: 10px 0px;\">\n
#   <h3><style font='red' bg='yellow>
#   Clinical Trial Experiment Platform</style></h3>\n</div>")
  
    headerPanel(title="Clinical Trial Experiment Platform", windowTitle="Clinical Trial Experiment Platform")
  ,
  sidebarPanel=sidebarPanel(
    #tag('font', varArgs=c(size=-2))
    #tags$style(" size=\"-2\" color='red'",
    radioButtons("specChoice", "choose spec type",
                 specClassNames)
    #           )
    ,    tag('hr', NULL),
    selectInput("ClassesOrObjects", "ClassesOrObjects",
                c("Classes", "Objects")),
    tag('hr', NULL),
    h3("Actions-TODO-View"),
    tags$button(type="button", value='view_experiment',
                name='view_experiment')
  ),
  mainPanel=mainPanel(
    textOutput(outputId="specName"),
    textOutput(outputId="ClassesOrObjects")
    ,   tableOutput(outputId="classes_table")
    ,   tableOutput(outputId="objects_table")
    #     ,    conditionalPanel(condition=
#                            "input.ClassesOrObjects=='Classes'",
#                          tableOutput(outputId="classes_table")
#         ),
#         conditionalPanel(condition=
#                            "input.ClassesOrObjects=='Objects'",
#                          tableOutput(outputId="objects_table")
#         )    
  )
)
)