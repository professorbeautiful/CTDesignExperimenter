## see  shinyuiTest.R and shinyserverTest.R for originl wxmple.

require(RBioinf)

specClassNames = c(`patient attributes`="BaseCharModelSpecifier",
                   `population models`="PopModelSpecifier",
                   `outcome models`="OutcomeModelSpecifier",
                   designs="DesignSpecifier",
                   `evaluation criteria`="EvalSpecifier")
shortName = function(specifierName)
  names(classNames)[match(specifierName, classNames)]

addAttr = function(x, att, value) {attr(x, att) = value; x }
asHTML = function(x) addAttr(x, "html", TRUE)

shinyUI(pageWithSidebar(  
#<style font='red' bg='yellow' > </style>
  headerPanel=
    addAttr("html", TRUE,
x="<head>\n  <title>Clinical Trial Experiment Platform</title>\n</head>\n<div class=\"span12\" style=\"padding: 10px 0px;\">
  <h2 color=rgb(200,300,400) > <i>Clinical Trial Experiment Platform </i> </h2>\n</div>") 
#    headerPanel(title="Clinical Trial Experiment Platform", windowTitle="Clinical Trial Experiment Platform")
  ,
  sidebarPanel=sidebarPanel(
    #tag('font', varArgs=c(size=-2))
    #tags$style(" size=\"-2\" color='red'",
    radioButtons("viewChoice", "Choose a view",
                 cq("View spec classes", 
                    "View spec objects",
                    "Define one clinical trial",
                    "Do a CT experiment")
     , tag('hr', NULL)
     , conditionalPanel(condition=
                     "input.viewChoice==\"View spec classes\"
                  ||  input.viewChoice==\"View spec objects\"",
          radioButtons("specChoice", "choose spec type", specClassNames))
     , conditionalPanel(condition=
                    "input.viewChoice==\"Do a CT experiment\"
                  ||  input.viewChoice==\"Define one clinical trial\"",
        input$viewChoice %&% " not yet implemented")                    
#     ,  tags$button(type="button",
#                 style="color: red",
#                 onclick='SimpleExperimentPane()',
#                 ("Simple experiment"))
#     , tag("script ", 
#         "function SimpleExperimentPane() {alert(\"Not yet ready!\");}")
  ),
  mainPanel=mainPanel(
    # condition MUST BE VALID JS. 
    # Therefore: input & output cannot share an ID. 
    # Thus, the prepended "x".
    # Likewise, you can't put the following 2 lines in 2 places!!
    # If you see...
    #   Error in tag("div", list(...)) : argument is missing, with no default
    # it's because of an extra comma--  empty arg.
    
    h2(textOutput(outputId="actionChoice"))
    ## this'd be nice...addAttr("Subclasses for the spec class", "html", TRUE),
    , conditionalPanel(condition=
                           "input.viewChoice==\"View spec classes\"",
                           tableOutput(outputId="classes_table")
        )
    ,  conditionalPanel(condition=
                           "input.viewChoice==\"View spec objects\"",
                          tableOutput(outputId="objects_table")
        )    
  )
)
)