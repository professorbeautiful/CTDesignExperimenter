###  server.R
###  shiny interface for defining a variable
require(RBioinf)
require(RJSONIO)
require(shiny)
require("shinyIncubator")

shinyServer_scaffold = function(input, output, session) {
  cat("Entered shinyServer_scaffold; 
      folder is ", getwd(), "\n")
  output$scaffoldTable = renderTable({scaffoldObjects})

  for(scafOb in scaffoldObjectNames) {
    parseText = paste0(
    "output$", scafOb, "<- renderUI({
      list(
        renderTable(scaffoldObjects[",
      scafOb,
    ", ]))
    }
    )"
    )
    #catn(parseText)
    #eval(parse(text=parseText))
    #cat("names of output is now ", names(output), "\n")
  }
  #   renderUI({
  #     nTabs = input$nTabs
  #     myTabs = lapply(paste('Tab', 1: nTabs), tabPanel)
  #     do.call(tabsetPanel, myTabs)
  #   })
  extractParamNames = function(insert) {
    ifelse(length(insert@parameters)>0,
           paste(collapse=",", names(insert@parameters) ),
           "")
    ## For now at least, a parameter is NOT a Variable
  }
  extractNeeds = function(insert) {
    ifelse(length(insert@requirements)>0,
           paste(collapse=",", 
                 sapply(insert@requirements, slot, "name") ),
           "")
  }
#  getSlots(class(insert))
  extractOutputVariable = function(insert) 
    insert@outputVariable@name #More later. Hyperlink?

  theUItemplate = 
    'output$Properties_BeginSimulation = renderTable(scaffoldObjects["BeginSimulation", ])
    \n
    insertsFor_BeginSimulation = defaultScenario[which(
      sapply(defaultScenario, function(insert)
        insert@insertSubType==
          scaffoldObjects["BeginSimulation", "eventInsertSubType"]
      ) ) ]
    
    insertsDF_BeginSimulation = 
           data.frame(
             params=sapply(insertsFor_BeginSimulation, extractParamNames),
             needs=sapply(insertsFor_BeginSimulation, extractNeeds),
             out=sapply(insertsFor_BeginSimulation, extractOutputVariable)
           )
    
    output$insertsDataframe_BeginSimulation =
            renderTable(insertsDF_BeginSimulation)
    
    output$BeginSimulation =
           renderUI(list(
               h2("Properties of the BeginSimulation scaffold block:"),
              \n
              tableOutput(outputId="Properties_BeginSimulation"),
              \n
              h2("In this scenario:"),
              \n
              tableOutput(outputId="insertsDataframe_BeginSimulation")
               ))
  '
  print(scaffoldObjectNames)
  for(scaffoldBlockName in scaffoldObjectNames[[1]]) {
    print(scaffoldBlockName)
    theUItext = gsub("BeginSimulation", scaffoldBlockName, theUItemplate)
  #  print(theUItext) }
    eval(parse(text=theUItext))
  }
}

shinyServer(shinyServer_scaffold)
