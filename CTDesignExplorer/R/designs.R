cat("======== designs.R  ================\n")

### Classes and Methods for design

setClass("DesignSpecifier", contains="VariableNetwork")

##  Class: APlusBSpecifier
# "A+B with dose de-escalation" design was described in the article written by Lin in Biostatistics,v2,203-215,2001
# The default object is 3+3 design specification
setClass("APlusBSpecifier",
         representation(A="numeric",B="numeric",C="numeric",D="numeric",E="numeric", 
                        TierDoses="numeric"),
         contains="DesignSpecifier",
         ## Method: getRequirements
         # This method is to get requirements from an "APlusBSpecifier" object
#          requirements=VariableList(
#                      Outcomes="BinaryToxicity",TimesToOutcomes=character(0),BaseChars=character(0)))
         
         prototype=list(A=3,B=3,C=1,D=1,E=1,TierDoses=1:5),
         validity=function(object){
           if(!all(Check<-c(object@B>0,object@C>0,object@C<=object@D,
                            object@D<=object@A,object@C<=object@E,object@E<=(object@D+object@B)))){
             Wrongs <- c("B<=0","C<=0","C>D","D>A","C>E","E>D+B")
             stop(paste(Wrongs[which(!Check)],collapse=","))
           }
           else TRUE
         })

## Class: CRMSpecifier
# Patients in the same cohort refer to those enrolled before the next model-based update 
# Assumed outcome models are either exponential or logit with one free parameter
# The prior of this one free parameter is assumed to be normal with mean 0.
# EstimationMethod: Bayes
# Switching stage rule for two-stage CRM: switch from initial stage when the first toxicity outcome is observed
# When EscalationRestriction=TRUE and NPatsPerCohort = 1, design will avoid 
# (1) skipping doses in escalation and (2) escalation immediately after a toxic outcome (i.e., incoherent escalation).
# When EscalationRestriction=TRUE and NPatsPerCohort > 1, design will avoid 
# (1) skipping doses in escalation and (2) escalation immediately after the probability of toxicity in the last cohort >= the target.
setClass("CRMSpecifier",representation(InitialProbGuesses="numeric",TierDoses="numeric",TargetProb="numeric",SampleSize="numeric",
                                       StartingDoseLevel="OptionalNumeric",InitialStageDoseLevels="OptionalNumeric",NPatsPerCohort="numeric",EscalationRestriction="logical",
                                       OutcomeModelType="character",InterceptLogitModel="numeric",StandardDeviationPrior="numeric"),
         contains="DesignSpecifier",
         prototype=list(NPatsPerCohort=1,EscalationRestriction=TRUE,OutcomeModelType="Exponential",
                        InterceptLogitModel=3,StandardDeviationPrior=1.34),
         validity=function(object){
           InitialProbGuesses <- object@InitialProbGuesses
           if(length(InitialProbGuesses) > 1){
             sapply(2:length(InitialProbGuesses),function(x) if (InitialProbGuesses[x]<InitialProbGuesses[x-1])
               stop("Initial toxicity probability guesses are not in non-decreasing order!"))
           }
           TierDoses <- object@TierDoses
           NTierDoses <- length(TierDoses)
           if(length(InitialProbGuesses)!=NTierDoses) 
             stop("The length of the initial toxicity probability guesses is not the same as the number of tier doses!")
           InitialStageDoseLevels <- object@InitialStageDoseLevels
           NInitialStageDoseLevels <- length(InitialStageDoseLevels)
           StartingDoseLevel <- object@StartingDoseLevel
           SampleSize <- object@SampleSize
           if(NInitialStageDoseLevels != 0){
             if(NInitialStageDoseLevels != SampleSize) stop("The length of specified dose level sequence in the initial stage
                                                            should be the same as sample size!")
             if(max(InitialStageDoseLevels)>NTierDoses) 
               stop("The maximum dose level in the initial stage dose level sequence is larger than the number of tier doses!")
             if(!is.null(StartingDoseLevel)){
               if(StartingDoseLevel!=NInitialStageDoseLevels[1]) print("The first dose level in the initial stage dose sequence is 
                                                                       going to be used as the starting dose!")
             }
             if(NInitialStageDoseLevels > 1)
               sapply(2:NInitialStageDoseLevels,function(x) {if (InitialStageDoseLevels[x]<InitialStageDoseLevels[x-1])
                 stop("Dose levels in the initial stage are not in non-decreasing order!")})  
             }
           else {
             if(is.null(StartingDoseLevel)) stop("Either starting dose or the dose level sequence in the initial stage
                                                 should be specified!")
           }   
           if(!is.null(StartingDoseLevel)){
             if(StartingDoseLevel > NTierDoses)
               stop("The starting dose level is larger than the number of tier doses!")
           }
           if(is.na(match(object@OutcomeModelType,c("Exponential","Logit")))) stop("Unknown outcome model type!")
           NPatsPerCohort <- object@NPatsPerCohort
           if(SampleSize/NPatsPerCohort != round(SampleSize/NPatsPerCohort)) stop("The number of patients is not multiple of
                                                                                  cohort size!")
           # The M2 regularity condition for the dose-toxicity response model in the appendix A of the paper (Cheung and Chappell,
           # Biometrics,58,671-674
           if(object@OutcomeModelType=="Logit"){
             ScaledDoses <- log(InitialProbGuesses/(1-InitialProbGuesses))-object@InterceptLogitModel
             if(!all(ScaledDoses<0) & !all(ScaledDoses>0)) stop("Scaled doses are not of the same sign, the intercept value
                                                                of the logit model needs to be redefined!")
           }
           else TRUE
           })

## Class: Phase2BryantDaySpecifier
# Bryant and Day Phase II trial design is described in the article, Biometrics, v51, 1372-1383, 1995
setClass("Phase2BryantDaySpecifier",representation(N1Pats="numeric",NPats="numeric",Efficacy1LL="numeric",EfficacyLL="numeric",
                                                   NonTox1LL="numeric",NonToxLL="numeric"),
         contains="DesignSpecifier")


setMethod("getRequirements",signature(spec="CRMSpecifier"),
          function(spec){
            return(list(Outcomes="BinaryToxicity",TimesToOutcomes=character(0),BaseChars=character(0)))
          }
)


## Method: getProvisons
# This method is to get provisions from an "APlusBSpecifier" object
setMethod("getProvisions",signature(spec="APlusBSpecifier"),
          function(spec){
            return(list(TrtAllos=c("Dose"),CTTimes=character(0),Conclusions=c("RP2D")))
          }
)

# RP2DL: recommended Phase 2 dose level
setMethod("getProvisions",signature(spec="CRMSpecifier"),
          function(spec){
            return(list(TrtAllos=c("Dose","Dose Level"),CTTimes=character(0),Conclusions=c("RP2D","RP2DL")))
          }
)

## Method: generateInitialActions
## Method for generating a list of initial actions for an action queue
setGeneric("generateInitialActions", function(designSpec) standardGeneric("generateInitialActions"))

# patsIndices: in the ascending order
setMethod("generateInitialActions",signature(designSpec="APlusBSpecifier"),
          function(designSpec){
            A <- designSpec@A
            Action1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                           OtherArgs=list(NPats=A),GlobalTime=0)
            Action2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,
                           patsIndices=PatsIndices)",
                           OtherArgs=list(PatsIndices=1:A),GlobalTime=1)
            InitialActions <- c(Action1,Action2)
            return(InitialActions)
          }
            )# it returns a list of initial actions in the order of the times they are executed

# This method returns a list of initial actions in the order of the times they are executed
# This method will also globalize the slots of the "CRMSpecifier" object and some relevant variables as well
setMethod("generateInitialActions",signature(designSpec="CRMSpecifier"),
          function(designSpec){
            sapply(slotNames("CRMSpecifier"),function(x) assign(x,slot(designSpec,x),pos=1))
            if(OutcomeModelType=="Exponential") assign("ScaledDoses",InitialProbGuesses,pos=1)
            else assign("ScaledDoses",log(InitialProbGuesses/(1-InitialProbGuesses))-InterceptLogitModel,pos=1)
            Action1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                           OtherArgs=list(NPats=NPatsPerCohort),GlobalTime=0)
            Action2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,
                           patsIndices=PatsIndices)",
                           OtherArgs=list(PatsIndices=1:NPatsPerCohort),GlobalTime=1)
            InitialActions <- c(Action1,Action2)
            return(InitialActions)
          }
            )

## Method: allocateTrts
setGeneric("allocateTrts",function(designSpec,currentCTData,currentGlobalTime,patsIndices) standardGeneric("allocateTrts"))

# It returns a list consisting of a new CT data and a list of new actions
setMethod("allocateTrts",signature(designSpec="APlusBSpecifier",currentCTData="CTData",currentGlobalTime="numeric",
                                   patsIndices="numeric"),
          function(designSpec,currentCTData,currentGlobalTime,patsIndices){
            # Total Number of patients in the current CT data
            NPats <- length(currentCTData@PatsData)
            # Number of current patients
            NCurrentPats <- length(patsIndices)
            TierDoses <- designSpec@TierDoses
            # Design parameters
            A <- designSpec@A
            B <- designSpec@B
            C <- designSpec@C 
            D <- designSpec@D 
            E <- designSpec@E
            APlusB <- A+B
            # When only first A patients have been enrolled to CT
            if (NPats == A)
              ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[1])))
            # When >A patients have been enrolled to CT
            else{
              # Dose vector for each patient that has been treated, in the ascending order of treatement times/enrollment times
              PreviousDoses <- sapply(currentCTData@PatsData[1:(NPats-NCurrentPats)],function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
              # Previous dose
              PreviousDose <- PreviousDoses[NPats-NCurrentPats]
              # Previous dose level
              PreviousDoseLevel <- which(TierDoses==PreviousDose)
              # Indices for patients who are on previous dose
              PatsIndicesOnPreviousDose <- (1:(NPats-NCurrentPats))[PreviousDoses==PreviousDose]
              # Number of patients on previous dose, which can be either A or (A+B)
              NPatsOnPreviousDose <- length(PatsIndicesOnPreviousDose)
              # Number of toxicity outcomes observed on previous dose
              NToxsOnPreviousDose <- sum(rep(1,NPatsOnPreviousDose)[sapply(PatsIndicesOnPreviousDose,function(x)
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes == 1)])
              if ((NPatsOnPreviousDose == A & NToxsOnPreviousDose < C )|(NPatsOnPreviousDose == APlusB  & NToxsOnPreviousDose <= E))
                ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[PreviousDoseLevel+1])))
              else if (NPatsOnPreviousDose == A & NToxsOnPreviousDose >= C & NToxsOnPreviousDose <= D)
                ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=PreviousDose)))
              else ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[PreviousDoseLevel-1])))
            }
            for ( i in patsIndices)
              currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- ThisPatConcurrentTrtsDataList
            NewAction1 <- new("Action",MethodCall="generatePatsOutcomes(outcomeModelSpec=outcomeModelSpec,patsIndices=PatsIndices,currentCTData=CurrentCTData)",
                              OtherArgs=list(PatsIndices=patsIndices),GlobalTime=currentGlobalTime+1)
            NewAction2 <- new("Action",MethodCall="checkStoppingRule(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime)",
                              OtherArgs=list(),GlobalTime=currentGlobalTime+2)
            return(list(NewCTData=currentCTData,NewActions=c(NewAction1,NewAction2)))
          }
)

# Some functions which will be called in the methods associated with the CRM design
# Function: .fLikelihoodCRM
.fLikelihoodCRM <- function(Beta){
  Likelihood <- 1
  if(OutcomeModelType=="Exponential"){
    for ( i in 1:length(AssignedScaledDoses))
      Likelihood <- Likelihood * ((AssignedScaledDoses[i]^exp(Beta))^Outcomes[i]) * ((1-AssignedScaledDoses[i]^exp(Beta))^(1-Outcomes[i]))
  }
  else {
    for ( i in 1:length(AssignedScaledDoses)){
      Prob <- (1+exp(-InterceptLogitModel-exp(Beta)*AssignedScaledDoses[i]))^(-1)
      Likelihood <- Likelihood * (Prob^Outcomes[i]) * ((1-Prob)^(1-Outcomes[i]))
    }
  }
  return(Likelihood)
}

# Function: .fPosteriorCRM
.fPosteriorCRM <- function(Beta){
  return(exp(-(Beta^2)/(2*(StandardDeviationPrior^2)))*.fLikelihoodCRM(Beta))
}

# Function: .fPosteriorXBetaCRM
.fPosteriorXBetaCRM <- function(Beta){
  return(Beta*exp(-(Beta^2)/(2*(StandardDeviationPrior^2)))*.fLikelihoodCRM(Beta))
}

# Function: .fEstimateBetaCRM
.fEstimateBetaCRM <- function(){
  return(integrate(.fPosteriorXBetaCRM,-Inf,Inf)[[1]]/integrate(.fPosteriorCRM,-Inf,Inf)[[1]])
}

# Function: .fFindDoseLevelSingleStageCRM
# It will return an assigned dose based on a specified CRM design and accumulating data
.fFindDoseLevelSingleStageCRM <- function(lastAssignedDoseLevel){
  if(OutcomeModelType=="Exponential")
    Probs <- ScaledDoses^exp(.fEstimateBetaCRM())
  else
    Probs <- (1+exp(-InterceptLogitModel-exp(.fEstimateBetaCRM())*ScaledDoses))^(-1)
  if(all(Probs<=TargetProb)) ModelBasedDoseLevel <- length(InitialProbGuesses)
  else if (all(Probs>=TargetProb)) ModelBasedDoseLevel <- 1
  else ModelBasedDoseLevel <- order(abs(Probs-TargetProb))[1]
  if(EscalationRestriction){
    if(sum(Outcomes[(length(Outcomes)-NPatsPerCohort+1):length(Outcomes)])/NPatsPerCohort >= TargetProb)
      DoseLevelToAssign <- min(ModelBasedDoseLevel,lastAssignedDoseLevel)
    else DoseLevelToAssign <- min(ModelBasedDoseLevel,lastAssignedDoseLevel+1)
  }
  else DoseLevelToAssign <- ModelBasedDoseLevel
  return(DoseLevelToAssign)
}

# Method: allocateTrts for the CRM design
setMethod("allocateTrts",signature(designSpec="CRMSpecifier",currentCTData="CTData",currentGlobalTime="numeric",
                                   patsIndices="numeric"),
          function(designSpec,currentCTData,currentGlobalTime,patsIndices){  
            NPats <- length(currentCTData@PatsData)
            TierDoses <- designSpec@TierDoses
            # When the CRM design is one-stage
            if(is.null(InitialStageDoseLevels)){
              # when allocating treatments to the first patient cohort 
              if(NPats==NPatsPerCohort) 
                ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",
                                                          TrtAllos=data.frame(Dose=TierDoses[StartingDoseLevel],DoseLevel=StartingDoseLevel)))
              # when at least one patient cohort has been treated
              else {
                PreviousOutcomes <- sapply(1:(NPats-NPatsPerCohort),function(x) 
                  currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes)
                PreviousDoseLevels <- sapply(1:(NPats-NPatsPerCohort),function(x) 
                  currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@TrtAllos$DoseLevel)
                assign("AssignedScaledDoses", ScaledDoses[PreviousDoseLevels],pos=1)
                assign("Outcomes",PreviousOutcomes,pos=1)
                ThisPatAssignedDoseLevel <- .fFindDoseLevelSingleStageCRM(lastAssignedDoseLevel=PreviousDoseLevels[NPats-NPatsPerCohort])
                ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[ThisPatAssignedDoseLevel],
                                                                                                   DoseLevel=ThisPatAssignedDoseLevel)))
              }
              for ( i in patsIndices)
                currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- ThisPatConcurrentTrtsDataList
            }
            # when the CRM design is two-stage and the current stage is initial stage   
            else if(is.null(currentCTData@CTTimes)){ 
              for ( i in patsIndices)
                currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",
                                                                               TrtAllos=data.frame(Dose=TierDoses[InitialStageDoseLevels[i]],DoseLevel=InitialStageDoseLevels[i])))
            }
            # when the CRM design is two-stage and the current stage is not initial stage
            else{
              PreviousOutcomes <- sapply(1:(NPats-NPatsPerCohort),function(x) 
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes)
              PreviousDoseLevels <- sapply(1:(NPats-NPatsPerCohort),function(x) 
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@TrtAllos$DoseLevel) 
              assign("AssignedScaledDoses", ScaledDoses[PreviousDoseLevels],pos=1)
              assign("Outcomes",PreviousOutcomes,pos=1)
              ThisPatAssignedDoseLevel <- .fFindDoseLevelSingleStageCRM(lastAssignedDoseLevel=PreviousDoseLevels[NPats-NPatsPerCohort])
              for ( i in patsIndices)
                currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",
                                                                               TrtAllos=data.frame(Dose=TierDoses[ThisPatAssignedDoseLevel],
                                                                                                   DoseLevel=ThisPatAssignedDoseLevel)))
            }
            NewAction1 <- new("Action",MethodCall="generatePatsOutcomes(outcomeModelSpec=outcomeModelSpec,patsIndices=PatsIndices,currentCTData=CurrentCTData)",
                              OtherArgs=list(PatsIndices=patsIndices),GlobalTime=currentGlobalTime+1)
            NewAction2 <- new("Action",MethodCall="checkStoppingRule(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime)",
                              OtherArgs=list(),GlobalTime=currentGlobalTime+2)
            return(list(NewCTData=currentCTData,NewActions=c(NewAction1,NewAction2)))
          }
)

## Method: checkStoppingRule
# It can return a new CT data or a list of new actions
setGeneric("checkStoppingRule",function(designSpec,currentCTData,currentGlobalTime) standardGeneric("checkStoppingRule"))

# Method "checkStoppingRule" for the "A+B" design
setMethod("checkStoppingRule",signature(designSpec="APlusBSpecifier",currentCTData="CTData",currentGlobalTime="numeric"),
          function(designSpec,currentCTData,currentGlobalTime){
            # Design parameters
            A <- designSpec@A
            B <- designSpec@B
            C <- designSpec@C 
            D <- designSpec@D 
            E <- designSpec@E
            APlusB <- A+B
            NPats <- length(currentCTData@PatsData)
            TierDoses <- designSpec@TierDoses
            StartingDose <- TierDoses[1]
            HighestDose <- TierDoses[length(TierDoses)]
            CurrentDose <- currentCTData@PatsData[[NPats]]@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose
            CurrentDoseLevel <- which(TierDoses==CurrentDose)
            AppliedDoses <- sapply(currentCTData@PatsData[1:NPats],function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
            HighestAppliedDose <- max(AppliedDoses)
            # Indices for patients who are on current dose
            PatsIndicesOnCurrentDose <- (1:NPats)[AppliedDoses==CurrentDose]
            # Number of patients on current dose, which can be either A or (A+B)
            NPatsOnCurrentDose <- length(PatsIndicesOnCurrentDose)
            # Number of toxicity outcomes observed on current dose
            NToxsOnCurrentDose <- sum(rep(1,NPatsOnCurrentDose)[sapply(PatsIndicesOnCurrentDose,function(x)
              currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes == 1)])
            if ((NPatsOnCurrentDose==A & NToxsOnCurrentDose>D) | (NPatsOnCurrentDose==APlusB & NToxsOnCurrentDose>E)){
              if (CurrentDose==StartingDose){
                currentCTData@Conclusions <- NA
                names(currentCTData@Conclusions) <- "RP2D"
                return (list(NewCTData=currentCTData))
              }
              else {
                # Dose that is one level lower than current dose
                Level1LowerDose <- TierDoses[CurrentDoseLevel-1]
                # Indices for patients who are on the dose with one level lower than current dose
                PatsIndicesOn1LevelLowerDose <- (1:NPats)[AppliedDoses==Level1LowerDose]
                # Number of patients on that dose, which can be either A or (APlusB)
                NPatsOn1LevelLowerDose <- length(PatsIndicesOn1LevelLowerDose)
                # Number of toxicity outcomes observed on that dose
                NToxsOn1LevelLowerDose <- sum(rep(1,NPatsOn1LevelLowerDose)[sapply(PatsIndicesOn1LevelLowerDose,function(x)
                  currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes == 1)])
                if (NPatsOn1LevelLowerDose==APlusB & NToxsOn1LevelLowerDose<=E) {
                  currentCTData@Conclusions <- Level1LowerDose
                  names(currentCTData@Conclusions) <- "RP2D"
                  return (list(NewCTData=currentCTData))
                }
                else{
                  NewAction1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                    OtherArgs=list(NPats=B),GlobalTime=currentGlobalTime + 1)
                  NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                    OtherArgs=list(PatsIndices=(NPats+1):(NPats+B)),GlobalTime=currentGlobalTime + 2)
                  return (list(NewActions=c(NewAction1,NewAction2)))
                }
              }
            }
            else if (NPatsOnCurrentDose==APlusB & NToxsOnCurrentDose<=E){
              if(CurrentDose==HighestAppliedDose){
                if(CurrentDose==HighestDose){
                  currentCTData@Conclusions <- NA
                  names(currentCTData@Conclusions) <- "RP2D"
                  return (list(NewCTData=currentCTData))
                }
                else{
                  NewAction1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                    OtherArgs=list(NPats=A),GlobalTime=currentGlobalTime + 1)
                  NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                    OtherArgs=list(PatsIndices=(NPats+1):(NPats+A)),GlobalTime=currentGlobalTime + 2)
                  return (list(NewActions=c(NewAction1,NewAction2)))
                }
              }
              else{
                currentCTData@Conclusions <- CurrentDose
                names(currentCTData@Conclusions) <- "RP2D"
                return (list(NewCTData=currentCTData))
              }
            }
            else if(NPatsOnCurrentDose==A & NToxsOnCurrentDose<C){
              if(CurrentDose==HighestDose){
                currentCTData@Conclusions <- NA
                names(currentCTData@Conclusions) <- "RP2D"
                return (list(NewCTData=currentCTData))
              }
              else{
                NewAction1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                  OtherArgs=list(NPats=A),GlobalTime=currentGlobalTime + 1)
                NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                  OtherArgs=list(PatsIndices=(NPats+1):(NPats+A)),GlobalTime=currentGlobalTime + 2)
                return (list(NewActions=c(NewAction1,NewAction2)))
              }
            }
            else {
              NewAction1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                OtherArgs=list(NPats=B),GlobalTime=currentGlobalTime + 1)
              NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                OtherArgs=list(PatsIndices=(NPats+1):(NPats+B)),GlobalTime=currentGlobalTime + 2)
              return (list(NewActions=c(NewAction1,NewAction2)))
            }
          }
) 

# Method "checkStoppingRule" for the CRM design
setMethod("checkStoppingRule",signature(designSpec="CRMSpecifier",currentCTData="CTData",currentGlobalTime="numeric"),
          function(designSpec,currentCTData,currentGlobalTime){
            NPats <- length(currentCTData@PatsData)
            # When all the patients have been treated and their outcomes have been generated
            if (NPats==SampleSize){
              CurrentOutcomes <- sapply(1:SampleSize,function(x) 
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes)
              CurrentDoseLevels <- sapply(1:SampleSize,function(x) 
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@TrtAllos$DoseLevel) 
              assign("AssignedScaledDoses", ScaledDoses[CurrentDoseLevels],pos=1)
              assign("Outcomes",CurrentOutcomes,pos=1)
              assign("EscalationRestriction","FALSE",pos=1)
              RP2DL <- .fFindDoseLevelSingleStageCRM(lastAssignedDoseLevel=CurrentDoseLevels[SampleSize])
              currentCTData@Conclusions <- c(TierDoses[RP2DL],RP2DL)
              names(currentCTData@Conclusions) <- c("RP2D","RP2DL")
              rm(list=c(slotNames("CRMSpecifier"),"ScaledDoses","AssignedScaledDoses","Outcomes"),pos=1)
              return(list(NewCTData=currentCTData))
            }   
            # When more patients needs to be enrolled and the current stage is initial stage
            else if(!is.null(InitialStageDoseLevels) & is.null(currentCTData@CTTimes)){
              NewAction1 <- new("Action",MethodCall="checkSwitchingStageRule(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime)",
                                OtherArgs=list(),GlobalTime=currentGlobalTime+1)
              NewAction2 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                OtherArgs=list(NPats=NPatsPerCohort),GlobalTime=currentGlobalTime + 2)
              NewAction3 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                OtherArgs=list(PatsIndices=(NPats+1):(NPats+NPatsPerCohort)),GlobalTime=currentGlobalTime + 3)
              return (list(NewActions=c(NewAction1,NewAction2,NewAction3)))
            }
            else{
              NewAction1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                OtherArgs=list(NPats=NPatsPerCohort),GlobalTime=currentGlobalTime + 1)
              NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                OtherArgs=list(PatsIndices=(NPats+1):(NPats+NPatsPerCohort)),GlobalTime=currentGlobalTime + 2)
              return (list(NewActions=c(NewAction1,NewAction2)))
            }
          }
)

## Method: checkSwitchingStageRule
# It may return a new CT data
setGeneric("checkSwitchingStageRule",function(designSpec,currentCTData,currentGlobalTime) standardGeneric("checkSwitchingStageRule"))

# Method "checkSwitchingStageRule" for the CRM design
setMethod("checkSwitchingStageRule",signature(designSpec="CRMSpecifier",currentCTData="CTData",currentGlobalTime="numeric"),
          function(designSpec,currentCTData,currentGlobalTime){
            NPats <- length(currentCTData@PatsData)
            CurrentOutcomes <- sapply(1:NPats,function(x) 
              currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes)
            if(any(CurrentOutcomes==1)){
              currentCTData@CTTimes <- c(SwitchingStageTime=currentGlobalTime)
              return(list(NewCTData=currentCTData))
            }
          }
)

### Classes and methods for evaluation criteria
## Class Union: EvalSpecifier
setClass("EvalSpecifier",
         contains="Specifier")

## Class: EvalSampleSizeSpecifier
setClass("EvalSampleSizeSpecifier",contains="EvalSpecifier")

## Class: EvalNToxsSpecifier
setClass("EvalNToxsSpecifier",contains="EvalSpecifier")

## Class: EvalRP2DSpecifier
# RP2D: recommended Phase 2 dose 
setClass("EvalRP2DSpecifier",contains="EvalSpecifier")

## Class: EvalProbRP2DAtEachDoseSpecifier
# This class represents the specification for getting the probability of being chosen as RP2D at each tier dose 
# during the first concurrent treatments in the trials.
setClass("EvalProbRP2DAtEachDoseSpecifier",representation(TierDoses="numeric"),contains="EvalSpecifier")

## Class: EvalNPatsAtEachDoseSpecifier
# This class represents the specification for getting the number of patients allocated at each tier dose 
# during the first concurrent treatments in the trials.
setClass("EvalNPatsAtEachDoseSpecifier",representation(TierDoses="numeric"),contains="EvalSpecifier")

## Class: EvalPctPatsAtEachDoseSpecifier
# This class represents the specification for getting the percentage of patients allocated at each tier dose
# during the first concurrent treatments in the trials.
setClass("EvalPctPatsAtEachDoseSpecifier",representation(TierDoses="numeric"),contains="EvalSpecifier")

## Class: EvalNToxsAtEachDoseSpecifier
# This class represents the specification for getting the number of toxicities observed
# at each tier dose during the first concurrent treatments in the trials.
setClass("EvalNToxsAtEachDoseSpecifier",representation(TierDoses="numeric"),contains="EvalSpecifier")

## Class: EvalToxRateSpecifier
# Tox Rate: the probability of having toxicity in a trial 
setClass("EvalToxRateSpecifier",contains="EvalSpecifier")

## Class: EvalProbSuccPhase2Specifier
setClass("EvalProbSuccPhase2Specifier",representation(LogToxDoseThresholdMean="numeric",LogToxDoseThresholdSd="numeric",
                                                      LogEfficacyThresholdMean="numeric",LogEfficacyDoseThresholdSd="numeric",Phase2DesignSpec="Phase2BryantDaySpecifier"),
         contains="EvalSpecifier")

## Method: getRequirements
# This method is to get requirements from an "EvalSampleSizeSpecifier" object
setMethod("getRequirements",signature(spec="EvalSampleSizeSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes=character(0),CTTimes=character(0),TrtAllos=character(0),Conclusions=character(0)))
          }
)

# This method is to get requirements from an "EvalNToxsSpecifier" object
setMethod("getRequirements",signature(spec="EvalNToxsSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes="BinaryToxicity",CTTimes=character(0),TrtAllos=character(0),Conclusions=character(0)))
          }
)

# This method is to get requirements from an "EvalRP2DSpecifier" object
setMethod("getRequirements",signature(spec="EvalRP2DSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes=character(0),CTTimes=character(0),TrtAllos=character(0),Conclusions="RP2D"))
          }
)

# This method is to get requirements from an "EvalProbRP2DAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalProbRP2DAtEachDoseSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes=character(0),CTTimes=character(0),TrtAllos="Dose",Conclusions="RP2D"))
          }
)

# This method is to get requirements from an "EvalNPatsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalNPatsAtEachDoseSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes=character(0),CTTimes=character(0),TrtAllos="Dose",Conclusions=character(0)))
          }
)

# This method is to get requirements from an "EvalPctPatsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalPctPatsAtEachDoseSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes=character(0),CTTimes=character(0),TrtAllos="Dose",Conclusions=character(0)))
          }
)

# This method is to get requirements from an "EvalNToxsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalNToxsAtEachDoseSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes="BinaryToxicity",CTTimes=character(0),TrtAllos="Dose",Conclusions=character(0)))
          }
)

# This method is to get requirements from an "EvalToxRateSpecifier" object
setMethod("getRequirements",signature(spec="EvalToxRateSpecifier"),
          function(spec){
            return(list(BaseChars=character(0),Outcomes="BinaryToxicity",CTTimes=character(0),TrtAllos=character(0),Conclusions=character(0)))
          }
)