# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")

shinyServer(function(input, output) {
  tabName = input$myTabSet
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  output$tabName = reactiveText(function()input$myTabSet)
  output$tabNameName = reactiveText(function()"tabName")
  output$object_table <- reactiveTable(function() {
    if(input$myTabSet %in% classNames) {
      data.frame(subClassNames(input$myTabSet))
    } else "No Table here"
  })
})
