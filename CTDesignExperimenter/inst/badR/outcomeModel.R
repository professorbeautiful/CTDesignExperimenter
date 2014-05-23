# cat("======== OutcomeModelSpecifier  ================\n")
# #' OutcomeModelSpecifier
# #' 
# #' Variable Generators should have access to the current
# setClass("OutcomeModelSpecifier", contains="VariableNetwork")
# 
# 
# ## Method: generatePatsOutcomes
# # The calling string for this method is an action slot in an action 
# # patsIndices are the enrollment order numbers of patients, which are the same as the element order numbers of PatsData object  
# setGeneric("generatePatsOutcomes",
#            function(outcomeModelSpec,patsIndices,currentCTData) 
#              standardGeneric("generatePatsOutcomes"))
# 
# setMethod("generatePatsOutcomes",signature(
#   outcomeModelSpec="OutcomeModelSpecifier",
#   patsIndices="numeric",
#   currentCTData="CTData"),
#           function(outcomeModelSpec,patsIndices,currentCTData){
#             for ( i in patsIndices)
#               currentCTData@PatsData[[i]]<-generateOutcomes(outcomeModelSpec=outcomeModelSpec,thisPatCurrentData=currentCTData@PatsData[[i]])
#             return(list(NewCTData=currentCTData))
#           }
# ) # It returns a list with one element for NewCTData
# 
# 
# 
# ## Class: DoseThresholdOutcomeModelSpecifier
# setClass("DoseThresholdOutcomeModelSpecifier",
#          representation(DoseThresholdName="character"),
#          contains="OutcomeModelSpecifier",
#          validity=function(object){ 
#            ## simplified code. 
#            ##Do we want this restriction?
#            if(!(object@DoseThresholdName %in% 
#                   c("ToxDoseThreshold","EfficacyDoseThreshold")))
#              stop("The dose threshold name is wrong!","\n")
#            else TRUE
#          }
# )
# 
# ## Class: ToxDeathDoseThresholdOutcomeModelSpecifier
# setClass("ToxDeathDoseThresholdOutcomeModelSpecifier",representation(DeltaDeath="numeric"), 
#          contains="OutcomeModelSpecifier",
#          validity=function(object){
#            if(object@DeltaDeath <=1)
#              stop("DeltaDeath needs to be > 1","\n")
#            else TRUE
#          }
# )
# 
# ## Class: ToxEfficacyDoseThresholdsModelSpecifier
# setClass("ToxEfficacyDoseThresholdsModelSpecifier",contains="OutcomeModelSpecifier")
# 
# ## Class: NoParamProbModelSpecifier
# # This class represents specification for a stochastic outcome model which directly provides probabilities of an outcome at each dose
# # for different subpopulations
# # Probs is a matrix with ncol=# Tier Doses, nrow=# subpopulations, row index = subpopulation index
# # The outcome is either toxicity or efficacy outcome (binary)
# setClass("NoParamProbModelSpecifier",representation(Probs="matrix",TierDoses="numeric",OutcomeName="character"), contains="OutcomeModelSpecifier",
#          validity=function(object){
#            for(i in 1:nrow(object@Probs))
#              if(!all(Check<-sapply(object@Probs[i,],function(x) return(x>=0 & x<=1))))
#                stop(paste("The following items in the probability vector for SubPop ",i," are either <0 or >1:"),"\n",
#                     paste(which(!Check), collapse=","),"\n" )
#            if(!any(sapply(c("BinaryToxicity","Efficacy"),function(x) x==object@OutcomeName))) stop("The outcome name is wrong!","\n")
#            else if(ncol(object@Probs)!=length(object@TierDoses)) 
#              stop("The number of probabilities in a subpopulation dosen't match the number of tier doses!","\n")
#            else TRUE
#          }
# )
# 
# # This method is to get the requirements from an "DoseThresholdOutcomeModelSpecifier" object
# setMethod("getRequirements",signature(spec="DoseThresholdOutcomeModelSpecifier"),
#           function(spec){
#             return(list(TrtAllos="Dose",BaseChars=spec@DoseThresholdName))
#           }
# )
# 
# # This method is to get the requirements from an "ToxDeathDoseThresholdOutcomeModelSpecifier" object
# setMethod("getRequirements",signature(spec="ToxDeathDoseThresholdOutcomeModelSpecifier"),
#           function(spec){
#             return(list(TrtAllos="Dose",BaseChars="ToxDoseThreshold"))
#           }
# )
# 
# # This method is to get the requirements from an "ToxEfficacyDoseThresholdsModelSpecifier" object
# setMethod("getRequirements",signature(spec="ToxEfficacyDoseThresholdsModelSpecifier"),
#           function(spec){
#             return(list(TrtAllos="Dose",BaseChars=c("ToxDoseThreshold","EfficacyDoseThreshold")))
#           }
# )
# 
# # This method is to get the requirements from an "NoParamProbModelSpecifier" object 
# setMethod("getRequirements",signature(spec="NoParamProbModelSpecifier"),
#           function(spec){
#             Probs <- spec@Probs
#             if(nrow(Probs)==1)  return(list(TrtAllos="Dose",BaseChars=character(0)))
#             else return(list(TrtAllos="Dose",BaseChars=c("SubPopIndex")))
#           }
# )
# 
# ## Method: getProvisions
# # This method is to get the provisions from an "DoseThresholdOutcomeModelSpecifier" object
# setMethod("getProvisions",signature(spec="DoseThresholdOutcomeModelSpecifier"),
#           function(spec){
#             return(list(Outcomes=c("BinaryToxicity"),TimesToOutcomes=character(0)))
#           }
# )
# 
# # This method is to get the provisions from an "ToxDeathDoseThresholdOutcomeModelSpecifier" object
# setMethod("getProvisions",signature(spec="ToxDeathDoseThresholdOutcomeModelSpecifier"),
#           function(spec){
#             return(list(Outcomes=c("BinaryToxicity","Death"),TimesToOutcomes=character(0)))
#           }
# )
# 
# # This method is to get the provisions from an "ToxEfficacyDoseThresholdsModelSpecifier" object
# setMethod("getProvisions",signature(spec="ToxEfficacyDoseThresholdsModelSpecifier"),
#           function(spec){
#             return(list(Outcomes=c("BinaryToxicity","Efficacy"),TimesToOutcomes=character(0)))
#           }
# )
# 
# # This method is to get the provisions from an "NoParamProbModelSpecifier" object
# setMethod("getProvisions",signature(spec="NoParamProbModelSpecifier"),
#           function(spec){
#             return(list(Outcomes=c(spec@OutcomeName),TimesToOutcomes=character(0)))
#           }
# )
# 
# ## Method: generateOutcomes 
# # This method returns an updated "thisPatCurrentData" with the generated outcome
# # from this patient most recent treatment. 
# setGeneric("generateOutcomes",function(outcomeModelSpec,thisPatCurrentData) standardGeneric("generateOutcomes"))
# 
# setMethod("generateOutcomes",signature(outcomeModelSpec="DoseThresholdOutcomeModelSpecifier",thisPatCurrentData="PatData"),
#           function(outcomeModelSpec,thisPatCurrentData){
#             DoseThresholdName <- outcomeModelSpec@DoseThresholdName
#             DoseThreshold <- thisPatCurrentData@BaseChars[DoseThresholdName]
#             ThisPatOutcome <- 0
#             # Find most recent set of concurrent treatments
#             NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
#             TrtAllo <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
#             if (TrtAllo > DoseThreshold) ThisPatOutcome <- 1
#             if (DoseThresholdName == "ToxDoseThreshold") names(ThisPatOutcome) <- "BinaryToxicity"
#             else names(ThisPatOutcome) <- "Efficacy"
#             thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcome
#             return(thisPatCurrentData)
#           }
# ) 
# 
# setMethod("generateOutcomes",signature(outcomeModelSpec="ToxDeathDoseThresholdOutcomeModelSpecifier",thisPatCurrentData="PatData"),
#           function(outcomeModelSpec,thisPatCurrentData){
#             DeltaDeath <- outcomeModelSpec@DeltaDeath
#             ToxDoseThreshold <- thisPatCurrentData@BaseChars["ToxDoseThreshold"]
#             ThisPatOutcomes <- rep(0,2)
#             names(ThisPatOutcomes) <- c("BinaryToxicity","Death")
#             # Find most recent set of concurrent treatments
#             NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
#             TrtAllo <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
#             if (TrtAllo > ToxDoseThreshold * DeltaDeath) ThisPatOutcomes["Death"] <- 1
#             if (TrtAllo > ToxDoseThreshold) ThisPatOutcomes["BinaryToxicity"] <- 1
#             thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcomes
#             return(thisPatCurrentData)
#           }
# )
# 
# setMethod("generateOutcomes",signature(outcomeModelSpec="ToxEfficacyDoseThresholdsModelSpecifier",thisPatCurrentData="PatData"),
#           function(outcomeModelSpec,thisPatCurrentData){
#             # Find most recent set of concurrent treatments
#             NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
#             TrtAllo <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
#             ThisPatOutcomes <- sapply(thisPatCurrentData@BaseChars[c("ToxDoseThreshold","EfficacyDoseThreshold")],function(x) ifelse(TrtAllo>x,1,0))
#             names(ThisPatOutcomes) <- c("BinaryToxicity","Efficacy")
#             thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcomes
#             return(thisPatCurrentData)
#           }
# )
# 
# setMethod("generateOutcomes",signature(outcomeModelSpec="NoParamProbModelSpecifier",thisPatCurrentData="PatData"),
#           function(outcomeModelSpec,thisPatCurrentData){
#             NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
#             thisPatCurrentTrtsData <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]
#             Probs <- outcomeModelSpec@Probs
#             # Find most recent set of concurrent treatments
#             TrtAllo <- thisPatCurrentTrtsData@TrtAllos$Dose
#             if(nrow(Probs)==1) 
#               ThisPatOutcome <- rbinom(1,1,prob=Probs[1,TrtAllo==outcomeModelSpec@TierDoses])
#             else {
#               SubPopIndex <- thisPatCurrentData@BaseChars["SubPopIndex"]
#               ThisPatOutcome <- rbinom(1,1,prob=Probs[SubPopIndex,TrtAllo==outcomeModelSpec@TierDoses])
#             }
#             names(ThisPatOutcome)<-outcomeModelSpec@OutcomeName
#             thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcome
#             return(thisPatCurrentData)
#           }
# )