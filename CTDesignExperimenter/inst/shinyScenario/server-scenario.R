###  server.R
###  shiny interface for defining a variable
require(RBioinf)
require(RJSONIO)
require(shiny)
require("shinyIncubator")

shinyServer_scaffold = function(input, output, session) {
  cat("Entered shinyServer_scaffold; 
      folder is ", getwd(), "\n")
  scaffoldObjectNames = scaffoldObjects[1]
  rownames(scaffoldObjectNames) = NULL
  names(scaffoldObjectNames) = "event block"
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
#     output$BeginClinicalTrial <- 
#       renderTable(scaffoldObjects["BeginClinicalTrial", ])
#   output$GeneratePatient <- 
#       renderTable(scaffoldObjects["GeneratePatient", ])
#   output$CheckEligibility <- renderUI({
#     list(
#       renderTable(scaffoldObjects[CheckEligibility, ]))
#   }
#   ) 
#   output$EnrollPatient<- renderUI({
#     list(
#       renderTable(scaffoldObjects[EnrollPatient, ]))
#   }
#   ) 
#   output$AssignTreatmentPlan<- renderUI({
#     list(
#       renderTable(scaffoldObjects[AssignTreatmentPlan, ]))
#   }
#   ) 
#   output$GenerateOutcomes<- renderUI({
#     list(
#       renderTable(scaffoldObjects[GenerateOutcomes, ]))
#   }
#   ) 
#   output$CheckOffStudy<- renderUI({
#     list(
#       renderTable(scaffoldObjects[CheckOffStudy, ]))
#   }
#   ) 
#   output$CheckModifications<- renderUI({
#     list(
#       renderTable(scaffoldObjects[CheckModifications, ]))
#   }
#   ) 
#   output$SummarizePatient<- renderUI({
#     list(
#       renderTable(scaffoldObjects[SummarizePatient, ]))
#   }
#   ) 
#   output$CheckStoppingRules<- renderUI({
#     list(
#       renderTable(scaffoldObjects[CheckStoppingRules, ]))
#   }
#   ) 
#   output$SummarizeTrial<- renderUI({
#     list(
#       renderTable(scaffoldObjects[SummarizeTrial, ]))
#   }
#   ) 
#   output$SummarizeSimulation<- renderUI({
#     list(
#       renderTable(scaffoldObjects[SummarizeSimulation, ]))
#   }
#   )
}

shinyServer(shinyServer_scaffold)
