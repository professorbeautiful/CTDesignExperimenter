output$evalOutput = renderText({
  ## This might be useful later for up-arrowing through past expressions.
  evalString = isolate(input$evalString)
  if(is.null(rValues$evalStringHistory))
    rValues$evalStringHistory = character(0)
  rValues$evalStringHistory = c(rValues$evalStringHistory, evalString)
  
  if(input$evalButton > 0) eval(parse(evalString))
})  
### example:   options(shiny.trace=input$traceButton)[[1]]
### example:   options("shiny.trace")[[1]]

output$shiny.trace = renderText({
  eval(options(shiny.trace=input$traceButton), envir = .GlobalEnv);
  catn("shiny.trace: ", options("shiny.trace")[[1]], 
       " should equal ", input$traceButton);
  "trace=" %&% input$traceButton
})   #### Not called!!


output$debugTools = renderUI({
  tag("table", list(
    tag("tr", 
        list(
          tag("TD",
              list(width=120, style="color: blue",
                   checkboxInput(inputId="traceButton", 
                                 value=FALSE,
                                 label=textOutput("shiny.trace")
                   ))),
          tag("TD", 
              list(style="color:\"red\"", actionButton("evalButton", "evalButton"))),
          tag("TD",
              list(width=10, textInput(inputId="evalString", label="", value="1+1"))),
          tag("TD", list(style="color:red", HTML("&rarr;"))),
          tag("TD",
              list(width=800, `text-align`="right", color="red", uiOutput(outputId="evalOutput")))
        )
    )
  )
  )
})

