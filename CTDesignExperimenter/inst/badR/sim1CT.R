# cat("======== sim1CT.R  ================\n")
# 
# 
# #' generatePatientsForAccrual
# # Assume generating patient baseline charatceristics is the first action that occurs on a patient 
# # nPats: number of patients
# generatePatientsForAccrual = function(popModelSpec,nPats,currentCTData){ 
#   if (!missing(popModelSpec))
#     NPatsData <- sapply(1:nPats,function(x) 
#       new("PatData",BaseChars=generateBaseChars(popModelSpec)))  
#   else 
#     NPatsData <- sapply(1:nPats,function(x) new("PatData"))
#   currentCTData@PatsData <- c(currentCTData@PatsData,NPatsData)  
#   return (list(NewCTData=currentCTData))
# }
# # It returns a list with one element for NewCTData
# 
# 
# 
# 
# 
# ########################################################################################################
# #                                          The Master Code for Doing Experiment
# #                                           S4 Method: doExperiment
# ########################################################################################################
# 
# ## Method: sim1CT
# # This method simulates a single CT data under the specified population model (optional, which can be NULL), design, outcome model.            
# 
# sim1CT = function(scenario){
#          if(missing(scenario))
#           stop("sim1CT: scenario is missing")
#          sim1CT(scenario@designSpec, scenario@popModelSpec,outcomeModelSpec)
# }
#                              
# setMethod("sim1CT", signature(designSpec="DesignSpecifier",
#                              popModelSpec="OptionalPopModelSpecifier",
#                              outcomeModelSpec="OutcomeModelSpecifier"),
#           function(designSpec,popModelSpec,outcomeModelSpec){
#             CurrentCTData <- new("CTData")
#             CurrentActionQ <- new("ActionQueue",
#                                   ActionQ=generateInitialActions(designSpec))
#             while(length(CurrentActionQ@ActionQ) != 0){
#               CurrentAction <- CurrentActionQ@ActionQ[[1]] 
#               getOtherArgs(CurrentAction)
#               CurrentGlobalTime <- CurrentAction@GlobalTime
#               Output <- eval(parse(text=CurrentAction@MethodCall))
#                       ### note similarity to generatorCode. What's the environment?
#               if (!is.null(Output$NewCTData)) CurrentCTData <- Output$NewCTData
#               if (!is.null(Output$NewActions)) {
#                 for ( i in 1:length(Output$NewActions)) 
#                   CurrentActionQ <- addAction(currentActionQ=CurrentActionQ,
#                                               newAction=Output$NewActions[[i]])
#               }
#               CurrentActionQ@ActionQ <- CurrentActionQ@ActionQ[-1]
#             }
#             return(CurrentCTData)
#           }
# )
