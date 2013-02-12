## see  shinyuiTest.R and shinyserverTest.R for originl wxmple.

require(RBioinf)

shinyUI(bootstrapPage(
  selectInput(inputId = "className",
              label = "class name",
              choices = c("OutcomeModelSpecifier",
                          "DesignSpecifier",
                          "EvalSpecifier"),
              selected = "EvalSpecifier"),
  tableOutput(outputId = "main_table")
  ))



