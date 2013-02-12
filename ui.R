## see  shinyuiTest.R and shinyserverTest.R for originl wxmple.

require(RBioinf)

classNames = c(base="BaseCharModelSpecifier",
               pop="PopModelSpecifier",
               outcome="OutcomeModelSpecifier",
               design="DesignSpecifier",
               eval="EvalSpecifier")

makeSpecifierTab = function(specifierName) {
  shortName = names(classNames)[match(specifierName, classNames)]
  tabPanel(shortName, 
           h3(specifierName %&% "/" %&% shortName),
           br(),
           textOutput(outputId="tabName"),
           textOutput(outputId="tabNameName"),
           h3("2nd line"),
           br(),
           tableOutput(outputId="object_table"),
           br(),
           h3("after the table")
  )
}


makeExperimentTab = function(){
  h1("experiment tab")
}
makeResultsTab = function(){
  h1("results tab")
}

shinyUI(pageWithSidebar(
  sidebarPanel=textOutput(outputId="tabName" ),
#     br(),
    headerPane= h3("TITLE"),## this works, but verbatimTextOutput does not.
#     br(),
    mainPanel=tabsetPanel(id="myTabSet",
              tabPanel(names(classNames)[1], 
                       makeSpecifierTab(classNames[1])), 
              tabPanel(names(classNames)[2], 
                       makeSpecifierTab(classNames[2])), 
              tabPanel(names(classNames)[3], 
                       makeSpecifierTab(classNames[3])), 
              tabPanel(names(classNames)[4], 
                       makeSpecifierTab(classNames[4])), 
              tabPanel(names(classNames)[5], 
                       makeSpecifierTab(classNames[5])), 
              tabPanel("doExperiment", makeExperimentTab()),
              tabPanel("Results", makeResultsTab())
  )))
