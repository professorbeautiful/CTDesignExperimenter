# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")
library(datasets)
options(shiny.trace=TRUE)
shinyServer(function(input, output) {
  specName = 
    reactiveText(function()input$specChoice)
  print(specName)
  output$specName = specName  
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$object_table <- 
    reactiveTable(function() {
      data.frame(subClassNames(specName))
    })
})
