# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")

shinyServer(function(input, output) {
  output$main_table <- reactiveTable(function() {
    data.frame(OutcomeModelSpecifier=subClassNames(
      input$className))
  })
})
