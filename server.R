# install.packages('shiny')
# install.packages('RJSONIO')
library("shiny")
library(datasets)

shinyServer(function(input, output) {
  output$main_plot <- reactivePlot(function() {
    tabName = tryCatch(input$myTabSet, 
                                 error=function(e)"no tab")
    if(is.null(input$myTabSet)) plot(1:3,1:3)
    else hist(faithful$eruptions,
         probability = TRUE,
         xlab = "Duration (minutes)",
         main = tabName)
  })
  output$object_table <- reactiveTable(function() {
    classNames = cq(BaseCharModelSpecifier,PopModelSpecifier,OutcomeModelSpecifier,DesignSpecifier,EvalSpecifier)
    output$tabName = reactiveText(function()input$myTabSet)
    output$tabNameName = reactiveText(function()"tabName")
#    return(("No Table here"))
    if(length(input$myTabSet) > 0 && input$myTabSet %in% classNames) {
      data.frame(subClassNames(input$myTabSet))
    } else matrix("No Table here")
  })
})
