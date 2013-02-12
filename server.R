# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")

shinyServer(function(input, output) {
  tabName = input$myTabSet
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$tabName = reactiveText(function()tabName)
  output$tabNameName = reactiveText(function()"tabName")
  if(tabName %in% classNames) {
    className = tabName
    output$object_table <- reactiveTable(function() {
      data.frame(subClassNames(className))
    })
  }
})
