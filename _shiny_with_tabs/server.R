# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")
library(datasets)

shinyServer(function(input, output) {
  #   output$main_plot <- reactivePlot(function() {
  #     tabName = tryCatch(input$myTabSet, 
  #                                  error=function(e)"no tab")
  #     if(is.null(input$myTabSet)) plot(1:3,1:3)
  #     else hist(faithful$eruptions,
  #          probability = TRUE,
  #          xlab = "Duration (minutes)",
  #          main = tabName)
  #   })
  output$tabName = reactiveText(function()input$myTabSet)
  output$tabNameName = reactiveText(function()"tabName")
  classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
  for(className in classNames) {
    output[["object_table_" %&% className]] <- 
      reactiveTable(function() {
          (className %&% "No Table here yet")
      #    if(length(input$myTabSet) > 0 && input$myTabSet %in% classNames) {
      #data.frame(subClassNames(className))
      #    } else matrix("No Table here")
    })
  }
})
