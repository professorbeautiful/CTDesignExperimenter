##  For source'ing in server.R

outputPreamble <<- 'window.Shiny.shinyapp.$bindings.'
# EXAMPLE:  window.Shiny.shinyapp.$bindings.selTxt.firstChild.nodeValue

output$evaluatedOutput = renderText({
  evalString = isolate(input$evalString)
  ## you have to isolate; otherwise each character typed calls this callback.
  ## This might be useful later for up-arrowing through past expressions.
  #   if(is.null(rValues$evalStringHistory))
  #     rValues$evalStringHistory = character(0)
  #  rValues$evalStringHistory = c(rValues$evalStringHistory, evalString)
  
  if(input$evalButton > 0) {
    evalToggle = input$evalToggle
    if(evalToggle == "R") {
      capture.output(eval(parse(text=evalString)))
    }
    else {
      if(evalToggle == "JSoutput")
        evalJS(paste0(outputPreamble, evalString))
      #       else
      evalJS(evalString)
      "JS output is in alert window, if there was no error."
    }
  }
})  

evalJS = function(evalString="1+5"){
  #JSevaluation
  evalString = gsub('"', "'", evalString) # replace all DQ with SQ.
  div(list(tags$script(
    # 'alert(', '"HERE IS JS"', ')'     # THIS WORKS! 
    # 'alert(eval(', '"1+2"', '))'       # THIS WORKS! 
    paste0(
      'alert(eval("', evalString, '"))'       # THIS WORKS!
    )
  )))
}

output$JSevaluation = renderUI({
  # TRY THIS SOME TIME, to avoid the alert window: 
  #document.getElementById("demo").innerHTML = fruits;
  
  evalString = isolate(input$evalString)
  if(input$evalButton > 0) {
    if(input$evalToggle == "JS")
      evalJS(evalString)
  }
})  


### Evaluation examples:
### example:   options(shiny.trace=input$traceButton)[[1]]
### example:   options("shiny.trace")[[1]]

output$shiny.trace.text = renderText({
  eval(options(shiny.trace=input$traceCheckbox), envir = .GlobalEnv);
  cat("shiny.trace: ", options("shiny.trace")[[1]], "\n")
  if( options("shiny.trace")[[1]] != input$traceCheckbox)
    cat('Error: options("shiny.trace")[[1]] should equal input$traceCheckbox', "\n");
  paste("trace=", input$traceCheckbox)
})   #### OK this works now.


output$debugTools = renderUI({
  div(style="background:darkGrey",
      singleton(tags$script(paste(
        "outputPreamble = '", outputPreamble, "';")))
      ,
      checkboxInput(inputId='debugToolsCheckbox', value=TRUE,
                    label=em(strong("Debugging aids"))),
      conditionalPanel('input.debugToolsCheckbox',
                      tag("table", list(
                        tag("tr", 
                            list(
                              tag("TD",
                                  list(width=120, style="color: blue",
                                       checkboxInput(inputId="traceCheckbox", 
                                                     value=FALSE,
                                                     label=textOutput("shiny.trace.text")
                                       ))),
                              tag("TD", list(HTML(paste0(rep("&nbsp;",15), collapse="")))),
                              tag("TD", 
                                  list(actionButton("evalButton", 
                                                    HTML("<font color='red'> evaluate</font>")))),
                              tag("TD", list( 
                                radioButtons("evalToggle", "", c("R","JS"))
                                #              div(style="size:large;color:red", "R"),
                                #                  div(style="size:small;color:black", "/JS")
                              )),
                              tag("TD",
                                  list(width=10, textInput(inputId="evalString", label="", value="1+1"))),
                              tag("TD", list(style="color:red", HTML("&rarr;"))),
                              tag("TD",
                                  list(width=800, 
                                       style='text-align:"right"; color:white', 
                                       uiOutput(outputId="evaluatedOutput"))),
                              uiOutput(outputId='JSevaluation')
                            )
                        )
                      )
                      )
      )
      )
})

