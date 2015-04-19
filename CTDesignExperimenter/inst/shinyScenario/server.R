###  server.R
###  shiny interface for defining a variable
require(RBioinf)
require(RJSONIO)
require(shiny)
require("shinyIncubator")

shinyServer_scaffold = function(input, output, session) {
  cat("Entered shinyServer_scaffold; 
      folder is ", getwd(), "\n")
  source("debugTools.R", local=TRUE)    ### This is super-useful!
  
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
  scaffoldOutputGenerator = function(scaffoldBlockName) {
    print(scaffoldBlockName)
    output[["Properties_" %&% scaffoldBlockName]] = 
      renderTable(scaffoldObjects[scaffoldBlockName, ])
    insertsFor_scaffoldBlockName = 
           defaultScenario@inserts[which(
             sapply(defaultScenario@inserts, function(insert)
               insert@insertSubType==
                 scaffoldObjects[scaffoldBlockName, "eventInsertSubType"]
             ) ) ]
    insertsDF_scaffoldBlockName = 
      data.frame(
        params=sapply(insertsFor_scaffoldBlockName, extractParamNames),
        needs=sapply(insertsFor_scaffoldBlockName, extractNeeds),
        out=sapply(insertsFor_scaffoldBlockName, extractOutputVariable)
      )
    output[["insertsDataframe_" %&% scaffoldBlockName]] =
      renderTable(insertsDF_scaffoldBlockName)
    
    output[[scaffoldBlockName]] =
      renderUI(list(
        h2("Properties of the scaffoldBlockName scaffold block:"),
        tableOutput(outputId="Properties_" %&% scaffoldBlockName),
        h2("In this scenario:"),
        tableOutput(outputId="insertsDataframe_" %&% scaffoldBlockName)
      ))
    #'
    #   print(scaffoldObjectNames)
    #   for(scaffoldBlockName in scaffoldObjectNames) {
    #     print(scaffoldBlockName)
    #     theUItext = gsub("BeginSimulation", scaffoldBlockName, theUItemplate)
    #     print(theUItext) }
    #     eval(parse(text=theUItext))
    #   }
  }
  lapply(scaffoldObjectNames[[1]], 
         scaffoldOutputGenerator)
}

debug(shinyServer_scaffold)
cat("Will debug shinyServer_scaffold\n")
shinyServer(shinyServer_scaffold)
