rm(list=ls())

### Some utility classes and methods
## Class Union: OptionalCharacter
setClassUnion("OptionalCharacter",c("character","NULL"))

## Class Union: OptionalNumeric
setClassUnion("OptionalNumeric",c("numeric","NULL"))

## Class Union: NumericLogical
setClassUnion("NumericLogical",c("numeric","logical"))

## Class:ConcurrentTrtsData
# This class represents data from a set of concurrent treatments for a patient
# The number of concurrent treatments is an integer >= 1
# TrtAllos is a data frame which may have columns for TrtName, Dose,Dose level, Unit, Route, StartTime and EndTime 
setClass("ConcurrentTrtsData",representation(TrtAllos="data.frame",Outcomes="numeric",TimesToOutcomes="OptionalNumeric"),
    prototype=list(TimesToOutcomes=NULL))

## Class: PatData
# This class represents data from a patient
# ConcurrentTrtsDataList is a list of data from different sets of concurrent treatments
# PatTimes may include enrollment time, off-CT time for a patient
setClass("PatData",representation(ID="OptionalNumeric",BaseChars="OptionalNumeric",ConcurrentTrtsDataList="list",
    PatTimes="OptionalNumeric"),
    prototype=list(ID=NULL,BaseChars=NULL,PatTimes=NULL),
    validity=function(object){
        if (length(object@ConcurrentTrtsDataList)==0) TRUE
        else{
            if(! all(Check<-sapply(object@ConcurrentTrtsDataList, function(x) is(x,"ConcurrentTrtsData"))))
                stop("The following elements are not objects of the ConcurrentTrtsData class:","\n",paste(which(!Check), collapse=","),"\n" )
            else TRUE
        }
    }
)
    
## Class: CTData
# This class represents data from a single CT
# CTTimes may include switching-stage time and stopping-CT time.
setClass("CTData",representation(PatsData="list",CTTimes="OptionalNumeric",Conclusions="NumericLogical"),
    prototype=list(CTTimes=NULL),
    validity=function(object){
        if (length(object@PatsData)==0) TRUE
        else{
            if(! all(Check<-sapply(object@PatsData, function(x) is(x,"PatData"))))
                stop("The following elements are not objects of the PatData class:","\n",paste(which(!Check), collapse=","),"\n" )
            else TRUE
        }
    }
)
        
### Classes and Methods for Action Queue
## Class: Action
# OtherArgs:the arguments whose values are obtained from the method call within which this action is generated; 
# OtherArgs is a named list 
# GlobalTime in an "Action" object refers to the time when the action method is called.
# In an action queue, all GlobalTime's should have the same reference time point.
# If no time specificBaseCharModelSpecifieration is available in the design specification, GlobalTime can be 1,2,3...for
# first action, second action, third action, etc in an action queue and when a new action is added to
# the queue, its GlobalTime is (GlobalTime of the last action in a queue + 1)
setClass("Action",representation(MethodCall="character",OtherArgs="list",GlobalTime="numeric"))
    
## Method: getOtherArgs
setGeneric("getOtherArgs",function(action) standardGeneric("getOtherArgs"))

setMethod("getOtherArgs",signature(action="Action"),
    function(action){
        OtherArgs <- action@OtherArgs 
        NArgs <- length(OtherArgs)
        if (NArgs != 0){
            for ( i in 1:length(OtherArgs))
                assign(names(OtherArgs)[i],OtherArgs[[i]],envir=parent.frame())
        }
    }
)
    
## Class: ActionQueue
# An Action Queque is in the ascending temproal order of dispatching actions
setClass("ActionQueue",representation(ActionQ="list"),
    validity=function(object){
        if (length(object@ActionQ)!=0){
            if(! all(Check<-sapply(object@ActionQ, function(x) is(x,"Action"))))
            stop("The following elements are not objects of the Action class:","\n",paste(which(!Check), collapse=","),"\n" )
            else TRUE
        }
        else TRUE
    }
)    

## Method: addAction
setGeneric("addAction",function(currentActionQ,newAction) standardGeneric("addAction"))

setMethod("addAction",signature(currentActionQ="ActionQueue",newAction="Action"),
    function(currentActionQ,newAction){
        # Number of Actions in the current action queue
        NActions <- length(currentActionQ@ActionQ)
        if (NActions == 0)
            currentActionQ@ActionQ <- c(currentActionQ@ActionQ, newAction)
        else { 
            CurrentGlobalTimes <- sapply(currentActionQ@ActionQ,function(x) x@GlobalTime)
            NewGlobalTime <- newAction@GlobalTime
            # New action will be the last one to be dispatched among the actions that have the same 
            # GlobalTime as newGlobalTime
            if (CurrentGlobalTimes[NActions] <= NewGlobalTime) currentActionQ@ActionQ <- c(currentActionQ@ActionQ, newAction)
            else{
                IndexAfterNewAction <- min((1:NActions)[CurrentGlobalTimes > NewGlobalTime])
                currentActionQ@ActionQ[IndexAfterNewAction:(NActions+1)] <- c(newAction, currentActionQ@ActionQ[IndexAfterNewAction:NActions])
            }
        }
        return(currentActionQ)
    }
) # It returns a new action queue
        
### Classes and Methods for population model
## Class: BaseCharModelSpecifier
setClass("BaseCharModelSpecifier",
    representation(BaseCharName="character",
                   ConditionBaseCharNames="OptionalCharacter",
                   RGenFun="character"),
    prototype = list(ConditionBaseCharNames = NULL))

## Method: generateBaseChar  
# this method can only be dispatched within the method "generateBaseChars"  
setGeneric("generateBaseChar",
           function(baseCharModelSpec) 
             standardGeneric("generateBaseChar"))
setMethod("generateBaseChar",
          signature(baseCharModelSpec="BaseCharModelSpecifier"),
    function(baseCharModelSpec){       
        BaseChar <- eval(parse(
          text=baseCharModelSpec@RGenFun) 
             # , envir=baseCharModelSpec # you can find slots but not package:stats.
              #           ,enclos="package:stats" ### error.
          ) ## sad. you have to prepend slots with "baseCharModelSpec@"
        ### .self@ doesnt work,  with(baseCharModelSpec,... doesnt work.
        names(BaseChar) <- baseCharModelSpec@BaseCharName
        return(BaseChar)
    }
)

## Method: getProvisions
setGeneric("getProvisions",function(spec) standardGeneric("getProvisions"))

setMethod("getProvisions",signature(spec="BaseCharModelSpecifier"),
    function(spec){
        return(spec@BaseCharName)
    }
)

## Method: getRequirements
setGeneric("getRequirements",function(spec) standardGeneric("getRequirements"))

setMethod("getRequirements",signature(spec="BaseCharModelSpecifier"),
    function(spec){
        if(is.null(spec@ConditionBaseCharNames)) return("None")
        else return(spec@ConditionBaseCharNames)
    }
)
    
## Class: PopModelSpecifier
setClass("PopModelSpecifier",representation(PopModelSpec="list"),
    validity=function(object){
        PopModelSpec <- object@PopModelSpec
        if(! all(Check<-sapply(PopModelSpec, function(x) is(x,"BaseCharModelSpecifier"))))
            stop("The following elements are not objects of the BaseCharModelSpecifier class:","\n",paste(which(!Check), collapse=","),"\n" )
        # make sure the elements in the popModelSpec are listed in the order of generating baseline characteristics
        Provisions <- "None"
        for ( i in 1 : length(PopModelSpec)){
            if(any(is.na(match(getRequirements(PopModelSpec[[i]]),Provisions))))
                stop("The conditioning baseline characteristics of the baseline characteristic model ",paste(i)," are not
                    generated in the previous ones of the list")
            Provisions <- c(Provisions, getProvisions(PopModelSpec[[i]]))
        }     
        return (TRUE)
    }
)
    
## Class: OptionalPopModelSpecifier
setClassUnion("OptionalPopModelSpecifier",
              c("PopModelSpecifier","NULL"))

# This method is to get provisions from an "OptionalPopModelSpecifier" object
setMethod("getProvisions",signature(spec="OptionalPopModelSpecifier"),
    function(spec){
        if(is.null(spec)) BaseChars="None"
        else BaseChars <- c("None",sapply(spec@PopModelSpec,function(x) x@BaseCharName))
        return(list(BaseChars=BaseChars))
    }
)
    
## Method: generateBaseChars
setGeneric("generateBaseChars",function(popModelSpec) standardGeneric("generateBaseChars"))
setMethod("generateBaseChars",signature(popModelSpec="PopModelSpecifier"),
    function(popModelSpec){ 
        BaseChars <- sapply(popModelSpec@PopModelSpec,function(object) assign(object@BaseCharName, generateBaseChar(object),pos=1))
        sapply(popModelSpec@PopModelSpec,function(object) rm(list=object@BaseCharName,pos=1))
        return(BaseChars)
    }
) # It returns a named numeric vector for baseline characteristics
    
## Method: generatePatsBaseChars
# Assume generating patient baseline charatceristics is the first action that occurs on a patient 
# nPats: number of patients
setGeneric("generatePatsBaseChars",function(popModelSpec,nPats,currentCTData) standardGeneric("generatePatsBaseChars"))
setMethod("generatePatsBaseChars",signature(popModelSpec="OptionalPopModelSpecifier",nPats="numeric",currentCTData="CTData"),
    function(popModelSpec,nPats,currentCTData){ 
        if (!is.null(popModelSpec))
           NPatsData <- sapply(1:nPats,function(x) new("PatData",BaseChars=generateBaseChars(popModelSpec)))  
        else 
            NPatsData <- sapply(1:nPats,function(x) new("PatData"))
            currentCTData@PatsData <- c(currentCTData@PatsData,NPatsData)  
        return (list(NewCTData=currentCTData))
    }
)# It returns a list with one element for NewCTData
    

### Classes and methods for outcome model
## Class union: OutcomeModelSpecifier
setClassUnion("OutcomeModelSpecifier")

## Class: DoseThresholdModelSpecifier
setClass("DoseThresholdModelSpecifier",
         representation(DoseThresholdName="character"),
         contains="OutcomeModelSpecifier",
    validity=function(object){ 
      ## simplified code. 
      ##Do we want this restriction?
        if(!(object@DoseThresholdName %in% 
               c("ToxDoseThreshold","EfficacyDoseThreshold")))
            stop("The dose threshold name is wrong!","\n")
        else TRUE
    }
)

## Class: ToxDeathDoseThresholdModelSpecifier
setClass("ToxDeathDoseThresholdModelSpecifier",representation(DeltaDeath="numeric"), contains="OutcomeModelSpecifier",
    validity=function(object){
        if(object@DeltaDeath <=1)
            stop("DeltaDeath needs to be > 1","\n")
        else TRUE
    }
)
    
## Class: ToxEfficacyDoseThresholdsModelSpecifier
setClass("ToxEfficacyDoseThresholdsModelSpecifier",contains="OutcomeModelSpecifier")

## Class: NoParamProbModelSpecifier
# This class represents specification for a stochastic outcome model which directly provides probabilities of an outcome at each dose
# for different subpopulations
# Probs is a matrix with ncol=# Tier Doses, nrow=# subpopulations, row index = subpopulation index
# The outcome is either toxicity or efficacy outcome (binary)
setClass("NoParamProbModelSpecifier",representation(Probs="matrix",TierDoses="numeric",OutcomeName="character"), contains="OutcomeModelSpecifier",
    validity=function(object){
        for(i in 1:nrow(object@Probs))
            if(!all(Check<-sapply(object@Probs[i,],function(x) return(x>=0 & x<=1))))
            stop(paste("The following items in the probability vector for SubPop ",i," are either <0 or >1:"),"\n",
                paste(which(!Check), collapse=","),"\n" )
        if(!any(sapply(c("BinaryToxicity","Efficacy"),function(x) x==object@OutcomeName))) stop("The outcome name is wrong!","\n")
        else if(ncol(object@Probs)!=length(object@TierDoses)) 
            stop("The number of probabilities in a subpopulation dosen't match the number of tier doses!","\n")
        else TRUE
    }
)

# This method is to get the requirements from an "DoseThresholdModelSpecifier" object
setMethod("getRequirements",signature(spec="DoseThresholdModelSpecifier"),
    function(spec){
        return(list(TrtAllos="Dose",BaseChars=spec@DoseThresholdName))
    }
)

# This method is to get the requirements from an "ToxDeathDoseThresholdModelSpecifier" object
setMethod("getRequirements",signature(spec="ToxDeathDoseThresholdModelSpecifier"),
    function(spec){
        return(list(TrtAllos="Dose",BaseChars="ToxDoseThreshold"))
    }
)

# This method is to get the requirements from an "ToxEfficacyDoseThresholdsModelSpecifier" object
setMethod("getRequirements",signature(spec="ToxEfficacyDoseThresholdsModelSpecifier"),
    function(spec){
        return(list(TrtAllos="Dose",BaseChars=c("ToxDoseThreshold","EfficacyDoseThreshold")))
    }
)

# This method is to get the requirements from an "NoParamProbModelSpecifier" object 
setMethod("getRequirements",signature(spec="NoParamProbModelSpecifier"),
    function(spec){
        Probs <- spec@Probs
        if(nrow(Probs)==1)  return(list(TrtAllos="Dose",BaseChars="None"))
        else return(list(TrtAllos="Dose",BaseChars=c("None","SubPopIndex")))
    }
)

## Method: getProvisions
# This method is to get the provisions from an "DoseThresholdModelSpecifier" object
setMethod("getProvisions",signature(spec="DoseThresholdModelSpecifier"),
    function(spec){
        return(list(Outcomes=c("None","BinaryToxicity"),TimesToOutcomes=c("None")))
    }
)

# This method is to get the provisions from an "ToxDeathDoseThresholdModelSpecifier" object
setMethod("getProvisions",signature(spec="ToxDeathDoseThresholdModelSpecifier"),
    function(spec){
        return(list(Outcomes=c("None","BinaryToxicity","Death"),TimesToOutcomes=c("None")))
    }
)

# This method is to get the provisions from an "ToxEfficacyDoseThresholdsModelSpecifier" object
setMethod("getProvisions",signature(spec="ToxEfficacyDoseThresholdsModelSpecifier"),
    function(spec){
        return(list(Outcomes=c("None","BinaryToxicity","Efficacy"),TimesToOutcomes=c("None")))
    }
)

# This method is to get the provisions from an "NoParamProbModelSpecifier" object
setMethod("getProvisions",signature(spec="NoParamProbModelSpecifier"),
    function(spec){
        return(list(Outcomes=c("None",spec@OutcomeName),TimesToOutcomes=c("None")))
    }
)

## Method: generateOutcomes 
# This method returns an updated "thisPatCurrentData" with the generated outcome
# from this patient most recent treatment. 
setGeneric("generateOutcomes",function(outcomeModelSpec,thisPatCurrentData) standardGeneric("generateOutcomes"))

setMethod("generateOutcomes",signature(outcomeModelSpec="DoseThresholdModelSpecifier",thisPatCurrentData="PatData"),
    function(outcomeModelSpec,thisPatCurrentData){
        DoseThresholdName <- outcomeModelSpec@DoseThresholdName
        DoseThreshold <- thisPatCurrentData@BaseChars[DoseThresholdName]
        ThisPatOutcome <- 0
        # Find most recent set of concurrent treatments
        NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
        TrtAllo <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
        if (TrtAllo > DoseThreshold) ThisPatOutcome <- 1
        if (DoseThresholdName == "ToxDoseThreshold") names(ThisPatOutcome) <- "BinaryToxicity"
        else names(ThisPatOutcome) <- "Efficacy"
        thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcome
        return(thisPatCurrentData)
    }
) 

setMethod("generateOutcomes",signature(outcomeModelSpec="ToxDeathDoseThresholdModelSpecifier",thisPatCurrentData="PatData"),
    function(outcomeModelSpec,thisPatCurrentData){
        DeltaDeath <- outcomeModelSpec@DeltaDeath
        ToxDoseThreshold <- thisPatCurrentData@BaseChars["ToxDoseThreshold"]
        ThisPatOutcomes <- rep(0,2)
        names(ThisPatOutcomes) <- c("BinaryToxicity","Death")
        # Find most recent set of concurrent treatments
        NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
        TrtAllo <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
        if (TrtAllo > ToxDoseThreshold * DeltaDeath) ThisPatOutcomes["Death"] <- 1
        if (TrtAllo > ToxDoseThreshold) ThisPatOutcomes["BinaryToxicity"] <- 1
        thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcomes
        return(thisPatCurrentData)
    }
)
    
setMethod("generateOutcomes",signature(outcomeModelSpec="ToxEfficacyDoseThresholdsModelSpecifier",thisPatCurrentData="PatData"),
    function(outcomeModelSpec,thisPatCurrentData){
        # Find most recent set of concurrent treatments
        NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
        TrtAllo <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
        ThisPatOutcomes <- sapply(thisPatCurrentData@BaseChars[c("ToxDoseThreshold","EfficacyDoseThreshold")],function(x) ifelse(TrtAllo>x,1,0))
        names(ThisPatOutcomes) <- c("BinaryToxicity","Efficacy")
        thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcomes
        return(thisPatCurrentData)
    }
)

setMethod("generateOutcomes",signature(outcomeModelSpec="NoParamProbModelSpecifier",thisPatCurrentData="PatData"),
    function(outcomeModelSpec,thisPatCurrentData){
        NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
        thisPatCurrentTrtsData <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]
        Probs <- outcomeModelSpec@Probs
        # Find most recent set of concurrent treatments
        TrtAllo <- thisPatCurrentTrtsData@TrtAllos$Dose
        if(nrow(Probs)==1) 
            ThisPatOutcome <- rbinom(1,1,prob=Probs[1,TrtAllo==outcomeModelSpec@TierDoses])
        else {
            SubPopIndex <- thisPatCurrentData@BaseChars["SubPopIndex"]
            ThisPatOutcome <- rbinom(1,1,prob=Probs[SubPopIndex,TrtAllo==outcomeModelSpec@TierDoses])
        }
        names(ThisPatOutcome)<-outcomeModelSpec@OutcomeName
        thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcome
        return(thisPatCurrentData)
    }
)
        
## Method: generatePatsOutcomes
# The calling string for this method is an action slot in an action 
# patsIndices are the enrollment order numbers of patients, which are the same as the element order numbers of PatsData object  
setGeneric("generatePatsOutcomes",function(outcomeModelSpec,patsIndices,currentCTData) standardGeneric("generatePatsOutcomes"))

setMethod("generatePatsOutcomes",signature(outcomeModelSpec="OutcomeModelSpecifier",patsIndices="numeric",currentCTData="CTData"),
    function(outcomeModelSpec,patsIndices,currentCTData){
        for ( i in patsIndices)
            currentCTData@PatsData[[i]]<-generateOutcomes(outcomeModelSpec=outcomeModelSpec,thisPatCurrentData=currentCTData@PatsData[[i]])
        return(list(NewCTData=currentCTData))
    }
) # It returns a list with one element for NewCTData

### Classes and Methods for design
## Class Union: DesignSpecifier
setClassUnion("DesignSpecifier")

##  Class: APlusBSpecifier
# "A+B with dose de-escalation" design was described in the article written by Lin in Biostatistics,v2,203-215,2001
# The default object is 3+3 design specification
setClass("APlusBSpecifier",representation(A="numeric",B="numeric",C="numeric",D="numeric",E="numeric", TierDoses="numeric"),
    contains="DesignSpecifier",
    prototype=list(A=3,B=3,C=1,D=1,E=1),
    validity=function(object){
        if(!all(Check<-c(object@B>0,object@C>0,object@C<=object@D,object@D<=object@A,object@C<=object@E,object@E<=(object@D+object@B)))){
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

## Method: getRequirements
# This method is to get requirements from an "APlusBSpecifier" object
setMethod("getRequirements",signature(spec="APlusBSpecifier"),
    function(spec){
        return(list(Outcomes="BinaryToxicity",TimesToOutcomes="None",BaseChars="None"))
    }
)

setMethod("getRequirements",signature(spec="CRMSpecifier"),
    function(spec){
        return(list(Outcomes="BinaryToxicity",TimesToOutcomes="None",BaseChars="None"))
    }
)


## Method: getProvisons
# This method is to get provisions from an "APlusBSpecifier" object
setMethod("getProvisions",signature(spec="APlusBSpecifier"),
    function(spec){
        return(list(TrtAllos=c("None","Dose"),CTTimes=c("None"),Conclusions=c("None","RP2D")))
    }
)

# RP2DL: recommended Phase 2 dose level
setMethod("getProvisions",signature(spec="CRMSpecifier"),
    function(spec){
        return(list(TrtAllos=c("None","Dose","Dose Level"),CTTimes=c("None"),Conclusions=c("None","RP2D","RP2DL")))
    }
)

## Method: generateInitialActions
## Method for generating a list of initial actions for an action queue
setGeneric("generateInitialActions", function(designSpec) standardGeneric("generateInitialActions"))

# patsIndices: in the ascending order
setMethod("generateInitialActions",signature(designSpec="APlusBSpecifier"),
    function(designSpec){
        A <- designSpec@A
        Action1 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
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
        Action1 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
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
                    NewAction1 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
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
                    NewAction1 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
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
                NewAction1 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                    OtherArgs=list(NPats=A),GlobalTime=currentGlobalTime + 1)
                NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                    OtherArgs=list(PatsIndices=(NPats+1):(NPats+A)),GlobalTime=currentGlobalTime + 2)
                return (list(NewActions=c(NewAction1,NewAction2)))
            }
        }
        else {
            NewAction1 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
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
            NewAction2 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                OtherArgs=list(NPats=NPatsPerCohort),GlobalTime=currentGlobalTime + 2)
            NewAction3 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                OtherArgs=list(PatsIndices=(NPats+1):(NPats+NPatsPerCohort)),GlobalTime=currentGlobalTime + 3)
            return (list(NewActions=c(NewAction1,NewAction2,NewAction3)))
        }
        else{
            NewAction1 <- new("Action",MethodCall="generatePatsBaseChars(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
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
setClassUnion("EvalSpecifier")

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
        return(list(BaseChars="None",Outcomes="None",CTTimes="None",TrtAllos="None",Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalNToxsSpecifier" object
setMethod("getRequirements",signature(spec="EvalNToxsSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="BinaryToxicity",CTTimes="None",TrtAllos="None",Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalRP2DSpecifier" object
setMethod("getRequirements",signature(spec="EvalRP2DSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",CTTimes="None",TrtAllos="None",Conclusions="RP2D"))
    }
)

# This method is to get requirements from an "EvalProbRP2DAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalProbRP2DAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",CTTimes="None",TrtAllos="Dose",Conclusions="RP2D"))
    }
)

# This method is to get requirements from an "EvalNPatsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalNPatsAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",CTTimes="None",TrtAllos="Dose",Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalPctPatsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalPctPatsAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",CTTimes="None",TrtAllos="Dose",Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalNToxsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalNToxsAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="BinaryToxicity",CTTimes="None",TrtAllos="Dose",Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalToxRateSpecifier" object
setMethod("getRequirements",signature(spec="EvalToxRateSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="BinaryToxicity",CTTimes="None",TrtAllos="None",Conclusions="None"))
    }
)

## Method: evalDesign
setGeneric("evalDesign", function(evalSpec,simCTsData) standardGeneric("evalDesign"))

setMethod("evalDesign",signature(evalSpec="EvalSampleSizeSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
            SampleSizes <- sapply(simCTsData,function(x) length(x@PatsData))
            Mean <- mean(SampleSizes)
            names(Mean) <- "Mean"
            Variance <- var(SampleSizes)
            names(Variance) <- "Variance"
            EvalOutput <- list(SampleSizes=SampleSizes,Summary=c(Mean,Variance))
            return(EvalOutput)
    }
)

# This method evaluates the toxicity outcomes from the first concurrent treatments
setMethod("evalDesign",signature(evalSpec="EvalNToxsSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NToxs <- sapply(simCTsData,function(x) sum(sapply(x@PatsData, function(y) 
            y@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])))
        Mean <- mean(NToxs)
        names(Mean) <- "Mean"
        Variance <- var(NToxs)
        names(Variance) <- "Variance"
        EvalOutput <- list(NToxs=NToxs,Summary=c(Mean,Variance))
        return(EvalOutput)
    }
)

# This method evaluates the chosen RP2Ds from clinical trials
setMethod("evalDesign",signature(evalSpec="EvalRP2DSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
            RP2Ds <- sapply(simCTsData,function(x) as.numeric(x@Conclusions["RP2D"]))
            Mean <- mean(RP2Ds,na.rm=TRUE)
            names(Mean) <- "Mean"
            Variance <- var(RP2Ds,na.rm=TRUE)
            names(Variance) <- "Variance"
            NNAs <- sum(is.na(RP2Ds))
            names(NNAs) <- "Number of NAs"
            EvalOutput <- list(RP2Ds=RP2Ds,Summary=c(Mean,Variance,NNAs))
            return(EvalOutput)
    }
)

# This method evaluates the probability of each tier dose being chosen as RP2D  
# during the first concurrent treatments in the trials.
# This method returns a named vector of the probabilities of each dose (including NA)being chosen as RP2D in the ascending order of
# tier doses.
setMethod("evalDesign",signature(evalSpec="EvalProbRP2DAtEachDoseSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NReps <- length(simCTsData)
        # TierDosesC: vector of tier doses where each tier dose is coerced to be a character
        TierDosesC <- as.character(evalSpec@TierDoses)
        ProbsRP2D <- rep(0,length(TierDosesC))
        names(ProbsRP2D) <- TierDosesC
        RP2Ds <- sapply(simCTsData,function(x) as.numeric(x@Conclusions["RP2D"]))
        ProbRP2DTable <- round(table(RP2Ds)/NReps,3)
        ProbsRP2D[names(ProbRP2DTable)] <- ProbRP2DTable
        if(sum(is.na(RP2Ds)!=0))
            ProbsRP2D <- c(ProbsRP2D,"NA"=round(sum(is.na(RP2Ds))/NReps,3))
        return(list(ProbsRP2D=ProbsRP2D))
    }
)

# This method evaluates the number of patients allocated at each tier dose 
# during the first concurrent treatments in the trials.
# This method returns a list with a named vector of the average numbers of patients in the ascending order of
# tier doses and a matrix whose columns contain numbers of patients allocated for each dose
setMethod("evalDesign",signature(evalSpec="EvalNPatsAtEachDoseSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NReps <- length(simCTsData)
        # TierDosesC: vector of tier doses where each tier dose is coerced to be a character
        TierDosesC <- as.character(evalSpec@TierDoses)
        # Number of patients at each tier dose = number of times each tier dose being assigned
        # AllNPatsMatrix stores the number of patients at each tier dose at each CT replication
        AllNPatsMatrix <- matrix(0,nrow=NReps,ncol=length(TierDosesC))
        colnames(AllNPatsMatrix) <- TierDosesC
        for (RepIndex in 1:NReps){
            AssignedDoses <- sapply(simCTsData[[RepIndex]]@PatsData,function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
            AssignedDoseTable <- table(AssignedDoses)
            AllNPatsMatrix[RepIndex,names(AssignedDoseTable)] <- AssignedDoseTable
        }
        NPats <- apply(AllNPatsMatrix,2,function(x) round(mean(x),3))
        return(list(AverageNPats=NPats,NPatsMatrix=AllNPatsMatrix))
    }
)

# This method evaluates the percentage of patients allocated at each tier dose 
# during the first concurrent treatments in the trials.
# This method returns a a named vector of the percentages of patients in the ascending order of
# tier doses.
setMethod("evalDesign",signature(evalSpec="EvalPctPatsAtEachDoseSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NPats <- evalDesign(evalSpec="EvalNPatsAtEachDoseSpecifier",simCTsData=simCTsData)
        # For percentage of patients, which is calculated as Lin does in his "pmtd" program for validation purpose
        # Average total number of patients enrolled in a CT replication
        AverageNPats <- NPats$AverageNPats
        AverageTotNPats <- sum(AverageNPats)
        PctPats <- sapply(1:length(AverageNPats),function(x) round((AverageNPats[x]/AverageTotNPats)*100,3))
        names(PctPats) <- names(NPats)
        return(list(PctPats=PctPats))
    }
)

# This method evaluates the number of toxicities at each tier dose 
# during the first concurrent treatments in the trials.
# This method returns a list with a named vector of the average numbers of toxicities in the ascending order of
# tier doses and a matrix whose columns contain numbers of toxicities allocated for each dose
setMethod("evalDesign",signature(evalSpec="EvalNToxsAtEachDoseSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NReps <- length(simCTsData)
        # TierDosesC: vector of tier doses where each tier dose is coerced to be a character
        TierDosesC <- as.character(evalSpec@TierDoses)
        # AllNToxsMatrix stores the number of toxicities at each tier dose at each CT replication
        AllNToxsMatrix <- matrix(0,nrow=NReps,ncol=length(TierDosesC))
        colnames(AllNToxsMatrix) <- TierDosesC
        for (RepIndex in 1:NReps){
            AssignedDoses <- sapply(simCTsData[[RepIndex]]@PatsData,function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
            Toxs <- sapply(simCTsData[[RepIndex]]@PatsData,function(x) x@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])
            if(any(Toxs==1)){
                ToxByAssignedDoseTable <- table(Toxs,AssignedDoses)["1",]
                AllNToxsMatrix[RepIndex,names(ToxByAssignedDoseTable)] <- ToxByAssignedDoseTable
            }
        }
        NToxs <- apply(AllNToxsMatrix,2,function(x) round(mean(x),3))
        return(list(AverageNToxs=NToxs,NToxsMatrix=AllNToxsMatrix))
    }
)

# This method calculates the toxicity rate as Lin does for exp. all tox. rate in his "pmtd" program
# for validation purpose
setMethod("evalDesign",signature(evalSpec="EvalToxRateSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NToxs <- sapply(simCTsData,function(x) sum(sapply(x@PatsData, function(y) 
            y@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])))
        SampleSizes <- sapply(simCTsData,function(x) length(x@PatsData))
        EvalOutput <- sum(NToxs)/sum(SampleSizes)
        names(EvalOutput) <- "ToxRate"
        return(EvalOutput)
    }
)
        
########################################################################################################
#                                          The Master Code for Doing Experiment
#                                           S4 Method: doExperiment
########################################################################################################

## Method: sim1CT
# This method simulates a single CT data under the specified population model (optional, which can be NULL), design, outcome model.            
setGeneric("sim1CT",function(designSpec,popModelSpec,outcomeModelSpec) standardGeneric("sim1CT"))
setMethod("sim1CT",signature(designSpec="DesignSpecifier",popModelSpec="OptionalPopModelSpecifier",outcomeModelSpec="OutcomeModelSpecifier"),
    function(designSpec,popModelSpec,outcomeModelSpec){
        CurrentCTData <- new("CTData")
        CurrentActionQ <- new("ActionQueue",ActionQ=generateInitialActions(designSpec))
        while(length(CurrentActionQ@ActionQ) != 0){
            CurrentAction <- CurrentActionQ@ActionQ[[1]] 
            getOtherArgs(CurrentAction)
            CurrentGlobalTime <- CurrentAction@GlobalTime
            Output <- eval(parse(text=CurrentAction@MethodCall))
            if (!is.null(Output$NewCTData)) CurrentCTData <- Output$NewCTData
            if (!is.null(Output$NewActions)) {
                for ( i in 1:length(Output$NewActions)) 
                    CurrentActionQ <- addAction(currentActionQ=CurrentActionQ,newAction=Output$NewActions[[i]])
            }
            CurrentActionQ@ActionQ <- CurrentActionQ@ActionQ[-1]
        }
    return(CurrentCTData)
    }
)

## Method: checkRequirements
# This method is to check requirements among designs, population models,outcome models and evaluation criteria
setGeneric("checkRequirements",function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs) standardGeneric("checkRequirements"))

setMethod("checkRequirements",signature(designSpecs="list",popModelSpecs="list",outcomeModelSpecs="list",
    evalSpecs="list"),function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs){
        ValidSetsMatrix <- NULL
        for ( DesignIndex in 1:length(designSpecs)){
            DesignSpec <- designSpecs[[DesignIndex]] 
            for ( PopMIndex in 1:length(popModelSpecs)){
                PopModelSpec <- popModelSpecs[[PopMIndex]]
                if(any(is.na(match(getRequirements(DesignSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars)))){
                    cat("The population model ",PopMIndex," cannot provide the baseline characteristics the design ", 
                        DesignIndex," requires!","\n","\n")
                    next
                }
                else{
                    for(OutcomeMIndex in 1:length(outcomeModelSpecs)){
                        OutcomeModelSpec <- outcomeModelSpecs[[OutcomeMIndex]]
                        if(any(
                            is.na(match(getRequirements(OutcomeModelSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars)),
                            is.na(match(getRequirements(OutcomeModelSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos)),
                            is.na(match(getRequirements(DesignSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes)),
                            is.na(match(getRequirements(DesignSpec)$TimesToOutcomes, getProvisions(OutcomeModelSpec)$TimesToOutcomes)))){
                            if(any(is.na(match(getRequirements(OutcomeModelSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars))))
                                cat("The population model ",PopMIndex," cannot provide the baseline characteristics the outcome model ", 
                                        OutcomeMIndex," requires!","\n","\n")
                            if(any(is.na(match(getRequirements(OutcomeModelSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos))))
                                cat("The design ",DesignIndex," cannot provide the treatment allocations the outcome model ", 
                                        OutcomeMIndex," requires!","\n","\n")
                            if(any(is.na(match(getRequirements(DesignSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes))))
                                cat("The outcome model ",OutcomeMIndex," cannot provide the outcomes the design ", 
                                        DesignIndex," requires!","\n","\n")
                            if(any(is.na(match(getRequirements(DesignSpec)$TimesToOutcomes, getProvisions(OutcomeModelSpec)$TimesToOutcomes))))
                                cat("The outcome model ",OutcomeMIndex," cannot provide the times to outcomes the design ", 
                                        DesignIndex," requires!","\n","\n")
                            next
                        }
                        else{
                            for(CriterionIndex in 1:length(evalSpecs)){
                                EvalSpec <- evalSpecs[[CriterionIndex]]
                                if(any(
                                    is.na(match(getRequirements(EvalSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars)),
                                    is.na(match(getRequirements(EvalSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes)),
                                    is.na(match(getRequirements(EvalSpec)$CTTimes, getProvisions(DesignSpec)$CTTimes)),
                                    is.na(match(getRequirements(EvalSpec)$Conclusions, getProvisions(DesignSpec)$Conclusions)),
                                    is.na(match(getRequirements(EvalSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos)))){
                                    if(any(is.na(match(getRequirements(EvalSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars))))
                                    cat("The population model ",PopMIndex," cannot provide the baseline characteristics the criterion ", 
                                        CriterionIndex," requires!","\n","\n") 
                                    if(any(is.na(match(getRequirements(EvalSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes))))
                                    cat("The outcome model ",OutcomeMIndex," cannot provide the outcomes the criterion ", 
                                        CriterionIndex," requires!","\n","\n")   
                                    if(any(is.na(match(getRequirements(EvalSpec)$CTTimes, getProvisions(DesignSpec)$CTTimes))))
                                    cat("The design ",DesignIndex," cannot provide the CT level event times the criterion ", 
                                        CriterionIndex," requires!","\n","\n") 
                                    if(any(is.na(match(getRequirements(EvalSpec)$Conclusions, getProvisions(DesignSpec)$Conclusions))))
                                    cat("The design ",DesignIndex," cannot provide the conclusions the criterion ", 
                                        CriterionIndex," requires!","\n","\n")   
                                    if(any(is.na(match(getRequirements(EvalSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos))))
                                    cat("The design ",DesignIndex," cannot provide the treatment allocations the criterion ", 
                                        CriterionIndex," requires!","\n","\n")     
                                    next
                                }
                                else ValidSetsMatrix <- rbind(ValidSetsMatrix,c(DesignIndex,PopMIndex,OutcomeMIndex,CriterionIndex))
                            }
                        }
                    }
                }
            }
        }
        if(!is.null(ValidSetsMatrix)) colnames(ValidSetsMatrix) <- c("Design","PopM","OutcomeM","Criterion")
        return(ValidSetsMatrix)
    }
)

## Method: doExperiment
# nReps: number of clinical trial replications to simulate
# simDataDir: directory for saving nReps simulated CT data, for example, 
# "C:/Yuan/Research/RogerDay/Yuan/MyThesis/ExpPlatform/Data/", the name of simulated CT data is in the format of
# "SimCTsData_S1.RData", where S1 refers to scenario 1
# If userInput=TRUE, then the program will ask for permission to proceed after printing out the results from the
# requirements-checking.
setGeneric("doExperiment",function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs,nReps,seed,simDataDir,userInput) 
    standardGeneric("doExperiment"))

# If no population model is specified, then popModelSpecs=list(NULL)
setMethod("doExperiment",signature(designSpecs="list",popModelSpecs="list",outcomeModelSpecs="list",evalSpecs="list",
    nReps="numeric",seed="OptionalNumeric",simDataDir="OptionalCharacter",userInput="logical"),
    function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs,nReps,seed,simDataDir,userInput){
        if(!is.null(seed)) set.seed(seed)
        # Check requirements
        ValidSetsMatrix <- checkRequirements(designSpecs=designSpecs,popModelSpecs=popModelSpecs,outcomeModelSpecs=outcomeModelSpecs,
            evalSpecs=evalSpecs)
        if (is.null(ValidSetsMatrix)) stop("No sets of designs, population models, outcome models and evaluation criteria can
            work together!")
        else{
            # Check user to see if he/she wants to proceed
            NValidSets <- nrow(ValidSetsMatrix)
            # Initialize "Scenarios", each scenario corresponds to a combination of a design, a population model and an 
            # outcome model
            Scenarios <- rep(1,NValidSets)
            for ( i in 2:NValidSets){
                if(any(ValidSetsMatrix[i-1,1:3]!=ValidSetsMatrix[i,1:3])) Scenarios[i] <- Scenarios[i-1]+1
                else Scenarios[i] <- Scenarios[i-1]
            }
            cat("Scenarios:","\n")
            print(cbind(Scenarios,ValidSetsMatrix))
            answer <- "Yes"
            if(userInput) answer <- readline("Proceed? (Yes or No)")
            if(answer=="No") print("Experiment is cancelled!")
            else if (answer=="Yes"){
                # Initialize the outputs from evaluation
                NScenarios <- length(unique(Scenarios))
                EvalOutputs <- vector("list",NScenarios)
                names(EvalOutputs) <- sapply(1:NScenarios,function(x) paste("Scenario",x,sep=""))
                for(ScenarioIndex in 1:NScenarios){
                    DesignSpec <- designSpecs[[(ValidSetsMatrix[Scenarios==ScenarioIndex,"Design"][1])]]
                    PopModelSpec <- popModelSpecs[[(ValidSetsMatrix[Scenarios==ScenarioIndex,"PopM"][1])]]
                    OutcomeModelSpec <- outcomeModelSpecs[[(ValidSetsMatrix[Scenarios==ScenarioIndex,"OutcomeM"][1])]]
                    EvalSpecs <- evalSpecs[(ValidSetsMatrix[Scenarios==ScenarioIndex,"Criterion"])]
                    SimCTsData <- sapply(1:nReps,function(x) sim1CT(designSpec=DesignSpec,popModelSpec=PopModelSpec,
                        outcomeModelSpec=OutcomeModelSpec))
                    if(!is.null(simDataDir)) save(SimCTsData,file=paste(simDataDir,"SimCTsData_S",ScenarioIndex,".RData",sep=""))
                    EvalOutputs[[ScenarioIndex]] <- lapply(EvalSpecs,function(x) evalDesign(evalSpec=x,simCTsData=SimCTsData))
                }
                return(EvalOutputs)
            }
            else stop("Your input is wrong! It should be either Yes or No.")
        }
    }
)
        
## Function: instantiateS4Object
# className is a character 
# "slots" is a named list with the names corresponding to the slot names
instantiateS4Object <- function(className,slots){
    Object <- new(className)
    for ( SlotName in names(slots))
        slot(Object,SlotName) <- slots[[SlotName]]
    return(Object)
} 
             
setClassUnion("Specifier", 
              c("BaseCharModelSpecifier", "PopModelSpecifier",
                "OutcomeModelSpecifier",
                "DesignSpecifier",
                "EvalSpecifier"))

