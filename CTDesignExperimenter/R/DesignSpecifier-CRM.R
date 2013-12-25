cat("======== DesignSpecifier-CRM.R  ================\n")


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




setMethod("getRequirements",signature(spec="CRMSpecifier"),
          function(spec){
            return(list(Outcomes="BinaryToxicity",TimesToOutcomes=character(0),BaseChars=character(0)))
          }
)

# RP2DL: recommended Phase 2 dose level
setMethod("getProvisions",signature(spec="CRMSpecifier"),
          function(spec){
            return(list(TrtAllos=c("Dose","Dose Level"),CTTimes=character(0),Conclusions=c("RP2D","RP2DL")))
          }
)



# ???? s they are executed
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
# 

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
