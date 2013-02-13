## see  shinyuiTest.R and shinyserverTest.R for originl wxmple.

require(RBioinf)

specClassNames = c(base="BaseCharModelSpecifier",
               pop="PopModelSpecifier",
               outcome="OutcomeModelSpecifier",
               design="DesignSpecifier",
               eval="EvalSpecifier")
shortName = function(specifierName)
  names(classNames)[match(specifierName, classNames)]

shinyUI(pageWithSidebar(
    headerPanel=headerPanel(title="Clinical Trial Experiment Platform", windowTitle="Clinical Trial Experiment Platform"),
    sidebarPanel=sidebarPanel(
      h2("CT element spec types"),
      br(),
      radioButtons("specChoice", "choose spec type",
                   specClassNames),
      br(),
      h2("Actions-TODO-View")
    ),
    mainPanel=mainPanel(
      h3("main panel"),
      textOutput(outputId="specName"),
      tableOutput(outputId="object_table")
    )
) )
