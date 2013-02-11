# rm(list=ls())
require(mvtnorm)
require(MCMCpack)

### Some utility classes and methods
## Class Union: OptionalCharacter
setClassUnion("OptionalCharacter",c("character","NULL"))

## Class Union: OptionalNumeric
setClassUnion("OptionalNumeric",c("numeric","NULL"))

## Class Union: NumericLogical
setClassUnion("NumericLogical",c("numeric","logical"))

### Classes and methods for distribution
## Class: DistrSpecifier
setClass("DistrSpecifier")

## Class Union: NumericDistrSpecifier
setClassUnion("NumericDistrSpecifier",c("numeric","DistrSpecifier"))

## Class: ContinuousDistrSpecifer
# Each subclass of this class represents specification for a type of continuous distribution
setClass("ContinuousDistrSpecifier",contains="DistrSpecifier")

## Class: DiscreteDistrSpecifer
# Each subclass of this class represents specification for a type of discrete distribution
setClass("DiscreteDistrSpecifier",contains="DistrSpecifier")

## Class: UnivarValueProbSpecifier
# This class represents specifications for a univariate discrete distribution in the format: Pr(x=value)=Prob
setClass("UnivarValueProbSpecifier",representation(Values="numeric",Probs="numeric"),
    contains="DiscreteDistrSpecifier",
    validity=function(object){
        Probs <- object@Probs
        if(length(object@Values)!=length(Probs)) stop("The number of possible values is not the same as the number of
        probabilities!")
        else if (all(Probs>=0)){
            if(!identical(sum(Probs),1)) stop("The sum of the probabilities is not equal to 1!")
            else TRUE
        }  
        else stop("All elements in 'Probs' should be non-negative!") 
    }
)

## Class: NormalSpecifier
setClass("NormalSpecifier",representation(Mean="numeric",Sd="numeric"),
    prototype=list(Mean=0,Sd=1),
    contains="ContinuousDistrSpecifier")
    
## Class: TSpecifier
setClass("TSpecifier",representation(Df="numeric",NonCentralityParam="OptionalNumeric"),
    prototype=list(NonCentralityParam=NULL),
    contains="ContinuousDistrSpecifier",
    validity=function(object){
        NCP <- object@NonCentralityParam
        if(!is.null(NCP)){
            # except for rt, the stats package requires abs(NCP) <= 37.62
            if(abs(NCP)>37.62) stop(" The absolute value of noncentrality parameter should be <= 37.62!")
        }
        else TRUE
    }
)
    
## Class: BetaSpecifier
setClass("BetaSpecifier",representation(Shape1="numeric",Shape2="numeric"),
    contains="ContinuousDistrSpecifier",
    validity=function(object){
        if(object@Shape1 <=0 | object@Shape2 <= 0) stop("Shape parameters should be >0!")
        else TRUE
    }
)
    
## Class: LogNormalSpecifier
setClass("LogNormalSpecifier",representation(MeanLog="numeric",SdLog="numeric"),
    prototype=list(MeanLog=0,SdLog=1),
    contains="ContinuousDistrSpecifier")

## Class: MultiNormalSpecifier
# Specification for the multivariate normal distribution
setClass("MultiNormalSpecifier",representation(Means="numeric",CovMatrix="matrix"),
    prototype=list(Means=c(0,0),CovMatrix=matrix(c(1,0.5,0.5,1),nrow=2)),
    contains="ContinuousDistrSpecifier",
    validity=function(object){
        Means <- object@Means
        CovMatrix <- object@CovMatrix
        if(nrow(CovMatrix)!=ncol(CovMatrix)) stop("Covariance matrix should be a squared matrix!")
        else if(length(Means)!= nrow(CovMatrix)) stop("The length of means should be the same as the number of rows
            of the covariance matrix!")
        else TRUE
    }
)
    
## Class: ContinuousUniformSpecifier
setClass("ContinuousUniformSpecifier",representation(Min="numeric",Max="numeric"),
    prototype=list(Min=0,Max=1),
    contains="ContinuousDistrSpecifier")
    
## Method: getDensity
setGeneric("getDensity",function(x,distrSpec) standardGeneric("getDensity"))

setMethod("getDensity",signature(x="numeric",distrSpec="NormalSpecifier"),
    function(x,distrSpec){
        return(dnorm(x,mean=distrSpec@Mean,sd=distrSpec@Sd))
    }
)

setMethod("getDensity",signature(x="numeric",distrSpec="TSpecifier"),
    function(x,distrSpec){
        NCP <- distrSpec@NonCentralityParam
        Df <- distrSpec@Df
        if (is.null(NCP))
        return(dt(x,df=Df))
        else return(dt(x,df=Df,ncp=NCP))
    }
)

setMethod("getDensity",signature(x="numeric",distrSpec="BetaSpecifier"),
    function(x,distrSpec){
        return(dbeta(x,shape1=distrSpec@Shape1,shape2=distrSpec@Shape2))
    }
)

setMethod("getDensity",signature(x="numeric",distrSpec="LogNormalSpecifier"),
    function(x,distrSpec){
        return(dlnorm(x,meanlog=distrSpec@MeanLog,sdlog=distrSpec@SdLog))
    }
)

setMethod("getDensity",signature(x="numeric",distrSpec="MultiNormalSpecifier"),
    function(x,distrSpec){
        return(dmvnorm(x,mean=distrSpec@Means,sigma=distrSpec@CovMatrix))
    }
)

setMethod("getDensity",signature(x="numeric",distrSpec="UnivarValueProbSpecifier"),
    function(x,distrSpec){
        Values <- distrSpec@Values
        if (!(x %in% Values)) Density <- 0
        else Density <- distrSpec@Probs[which(x==Values)]
        return(Density)
    }
)

setMethod("getDensity",signature(x="numeric",distrSpec="ContinuousUniformSpecifier"),
    function(x,distrSpec){
        return(dunif(x,min=distrSpec@Min,max=distrSpec@Max))
    }
)
   
## Method: getMean
setGeneric("getMean",function(distrSpec) standardGeneric("getMean"))

setMethod("getMean",signature(distrSpec="NormalSpecifier"),  
    function(distrSpec){
        return(distrSpec@Mean)
    }
)   

setMethod("getMean",signature(distrSpec="TSpecifier"),  
    function(distrSpec){
        NCP <- distrSpec@NonCentralityParam
        Df <- distrSpec@Df
        if (is.null(NCP)) Mean <- 0
        else if(Df<=1) Mean <- NA
        else Mean <- NCP *  sqrt(Df/2) * (gamma((Df-1)/2)/gamma(Df/2))
        return(Mean)
    }
) 

setMethod("getMean",signature(distrSpec="BetaSpecifier"),  
    function(distrSpec){
        return(distrSpec@Shape1/(distrSpec@Shape1+distrSpec@Shape2))
    }
) 

setMethod("getMean",signature(distrSpec="LogNormalSpecifier"),  
    function(distrSpec){
        return(exp(distrSpec@MeanLog+((distrSpec@SdLog)^2)/2))
    }
) 

setMethod("getMean",signature(distrSpec="MultiNormalSpecifier"),  
    function(distrSpec){
        return(distrSpec@Means)
    }
) 

setMethod("getMean",signature(distrSpec="ContinuousUniformSpecifier"),  
    function(distrSpec){
        return((distrSpec@Min+distrSpec@Max)/2)
    }
) 

setMethod("getMean",signature(distrSpec="UnivarValueProbSpecifier"),  
    function(distrSpec){  
        return(sum(distrSpec@Values * distrSpec@Probs))
    }
)

### Some utility functions
fLogit <- function(x) log(x/(1-x))

fAntiLogit <- function(x) (1+exp(-x))^(-1)

# fBinomPCI: will return the exact CI for binomial proportion
fBinomPCI <- function(k,n,alpha=0.05,range=0:1){
    lb <- try(uniroot(function(p)
        pbinom((k-1),n,p) - 1 + alpha/2, range)$root,silent=T)
    if(class(lb) == "try-error") {
        lb <- 0
        ub <- uniroot(function(p)
            pbinom(k,n,p) - alpha, range)$root
    }
    else {
        ub <- try(uniroot(function(p)
            pbinom(k,n,p) - alpha/2, range)$root,silent=T)
        if(class(ub) == "try-error") {
            lb <- uniroot(function(p)
                    pbinom((k-1),n,p) - 1 + alpha, range)$root
            ub <- 1
        }
    }
    return(c(lb=round(lb,3), ub=round(ub,3)))
}

## Class:ConcurrentTrtsData
# This class represents data from each set of concurrent treatments for a single patient
# 'TrtAllos' is a data frame which may have components/columns for the treatment name(TrtName), dosage 
# (Dose),dose level(DoseLevel), dosage unit(Unit), route (Route), time when the treatment starts
# (StartTime) and time when the treatment ends (EndTime) 
# 'TimeToOutcome' refer to the time duration between the maximum EndTime in the 'TrtAllos' and the time when an outcome occurs
# 'TimesToOutcomes' is a vector of "TimeToOutcome"  
setClass("ConcurrentTrtsData",representation(TrtAllos="data.frame",Outcomes="numeric",TimesToOutcomes="OptionalNumeric"),
    prototype=list(TimesToOutcomes=NULL),
    validity=function(object){
        TimesToOutcomes <- object@TimesToOutcomes
        if(!is.null(TimesToOutcomes)){
            if(!all(TimesToOutcomes>=0))
                stop("The following element(s) in 'TimesToOutcomes' is/are negative:","\n",paste(which(TimesToOutcomes<0), collapse=","),"\n")
            else if(length(object@Outcomes)!=length(TimesToOutcomes))
                stop("The length of 'Outcomes' is not equal to 'TimesToOutcomes'!")             
        }
        else TRUE
    } 
)

## Class: PatData
# This class represents data from a single patient
# 'ConcurrentTrtsDataList' is a list of data from each set of concurrent treatments for a single patient
# 'PatTimes' is a vector of patient-level event times. For example, the time when a patient enrolls in a clinical trial (EnrollTime), and
# the time when a patient gets off a clinical trial (OffTime)
setClass("PatData",representation(ID="OptionalNumeric",BaseChars="OptionalNumeric",ConcurrentTrtsDataList="list",
    PatTimes="OptionalNumeric"),
    prototype=list(ID=NULL,BaseChars=NULL,PatTimes=NULL),
    validity=function(object){
        ConcurrentTrtsDataList <- object@ConcurrentTrtsDataList
        if (length(ConcurrentTrtsDataList)==0) TRUE
        else{
            if(!all(Check<-sapply(ConcurrentTrtsDataList, function(x) is(x,"ConcurrentTrtsData"))))
                stop("The following element(s) in 'ConcurrentTrtsDataList' is/are not object(s) of the 'ConcurrentTrtsData' class:","\n",
                    paste(which(!Check), collapse=","),"\n" )
            else TRUE
        }
    }
)
    
## Class: CTData
# This class represents data from a single CT
# 'CTTimes' is a vector of CT-level event times. For example, the time when a two-stage clinical trial switches from the first stage to the second
# (SwitchStageTime1), and the time when a clinical trial stops (StopCTTime).
setClass("CTData",representation(PatsData="list",CTTimes="OptionalNumeric",Conclusions="NumericLogical"),
    prototype=list(CTTimes=NULL),
    validity=function(object){
        PatsData <- object@PatsData
        if (length(PatsData)==0) TRUE
        else{
            if(!all(Check<-sapply(PatsData, function(x) is(x,"PatData"))))
                stop("The following element(s) in 'PatsData' is/are not object(s) of the 'PatData' class:","\n",paste(which(!Check), collapse=","),"\n" )
            else TRUE
        }
    }
)

### Classes and Methods for Action Queue
## Class: Action
# Executing an 'Action' object = Evaluating the 'MethodCall' string
# 'OtherArgs':the arguments whose values are obtained from the method call within which the action is generated; 
# 'OtherArgs' is a named list 
# 'GlobalTime' in an 'Action' object refers to the time when the 'Action' method is called.
# In an action queue, all 'GlobalTime' should have the same reference time point.
# If defining the exact time is not necessary in the simulation, the order in which different 'Action' objects are executed can be 
# used for 'Global Time',such as 1,2,3...
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
# An Action Queque is a list of actions ordered by their 'GlobalTime'
setClass("ActionQueue",representation(ActionQ="list"),
    validity=function(object){
        if (length(object@ActionQ)!=0){
            if(! all(Check<-sapply(object@ActionQ, function(x) is(x,"Action"))))
            stop("The following element(s) in 'ActionQ' is/are not object(s) of the 'Action' class:","\n",paste(which(!Check), collapse=","),"\n" )
            else TRUE
        }
        else TRUE
    }
)    

## Method: addAction
setGeneric("addAction",function(currentActionQ,newAction) standardGeneric("addAction"))

setMethod("addAction",signature(currentActionQ="ActionQueue",newAction="Action"),
    function(currentActionQ,newAction){
        # Number of actions in the current action queue
        NActions <- length(currentActionQ@ActionQ)
        if (NActions == 0)
            currentActionQ@ActionQ <- c(currentActionQ@ActionQ, newAction)
        else { 
            CurrentGlobalTimes <- sapply(currentActionQ@ActionQ,function(x) x@GlobalTime)
            NewGlobalTime <- newAction@GlobalTime
            # New action will be the last one to be executed among the actions that have the same 
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
        
### Classes and Methods for Population Model
## Class: BaseCharModelSpecifier
setClass("BaseCharModelSpecifier",
    representation(BaseCharName="character",ConditionBaseCharNames="OptionalCharacter",RGenFun="character"),
    prototype = list(ConditionBaseCharNames = NULL))

## Method: generateBaseChar  
# This method can ONLY be dispatched within the method "generateBaseChars"  
setGeneric("generateBaseChar",function(baseCharModelSpec) standardGeneric("generateBaseChar"))
setMethod("generateBaseChar",signature(baseCharModelSpec="BaseCharModelSpecifier"),
    function(baseCharModelSpec){       
        BaseChar <- eval(parse(text=baseCharModelSpec@RGenFun))
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
            stop("The following element(s) in 'PopModelSpec' is/are not object(s) of the 'BaseCharModelSpecifier' class:","\n",paste(which(!Check),
                collapse=","),"\n" )
        # Check if the model specifications for conditioning baseline characteristics
        # are listed before those for the baseline characteristics whose distributions depend on the conditioning baseline characteristics
        Provisions <- "None"
        for ( i in 1 : length(PopModelSpec)){
            if(any(is.na(match(getRequirements(PopModelSpec[[i]]),Provisions))))
                stop("The model specification(s) for the conditioning baseline characteristic(s) is(are) not listed before
                    that for the baseline characteristic ",paste(i))
            Provisions <- c(Provisions, getProvisions(PopModelSpec[[i]]))
        }     
        return (TRUE)
    }
)
    
## Class: OptionalPopModelSpecifier
setClassUnion("OptionalPopModelSpecifier",c("PopModelSpecifier","NULL"))

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
# This method is an "Action" method
# Assume that when patients enroll in a clinical trial, the first patient-level "Action" is to generate
# patients' baseline characteristics
# nPats: the number of patients
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
setClass("DoseThresholdModelSpecifier",representation(DoseThresholdName="character"),contains="OutcomeModelSpecifier",
    validity=function(object){
        if(!any(sapply(c("ToxDoseThreshold","EfficacyDoseThreshold"),function(x) x==object@DoseThresholdName)))
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
# This class represents specification for a stochastic outcome model which directly provides probabilities of an outcome 
# at each dose for different subpopulations
# Probs is a matrix with ncol=# Tier Doses, nrow=# subpopulations, row index = subpopulation index
# The outcome is either toxicity or efficacy outcome (binary)
setClass("NoParamProbModelSpecifier",representation(Probs="matrix",TierDoses="numeric",OutcomeName="character"), 
    contains="OutcomeModelSpecifier",
    validity=function(object){
        for(i in 1:nrow(object@Probs))
            if(!all(Check<-sapply(object@Probs[i,],function(x) return(x>=0 & x<=1))))
            stop(paste("The following element(s) in 'Probs' for SubPop ",i," is/are either <0 or >1:"),"\n",
                paste(which(!Check), collapse=","),"\n")
        if(!any(sapply(c("BinaryToxicity","BinaryEfficacy"),function(x) x==object@OutcomeName))) stop("The outcome name is wrong!","\n")
        else if(ncol(object@Probs)!=length(object@TierDoses)) 
            stop("The number of probabilities in a subpopulation doesn't match the number of tier doses!","\n")
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
        else return(list(TrtAllos="Dose",BaseChars=c("SubPopIndex")))
    }
)

## Method: getProvisions
# This method is to get the provisions from an "DoseThresholdModelSpecifier" object
setMethod("getProvisions",signature(spec="DoseThresholdModelSpecifier"),
    function(spec){
        if (spec@DoseThresholdName=="ToxDoseThreshold")
        return(list(Outcomes=c("None","BinaryToxicity"),TimesToOutcomes=c("None")))
        else
        return(list(Outcomes=c("None","BinaryEfficacy"),TimesToOutcomes=c("None")))
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
        return(list(Outcomes=c("None","BinaryToxicity","BinaryEfficacy"),TimesToOutcomes=c("None")))
    }
)

# This method is to get the provisions from an "NoParamProbModelSpecifier" object
setMethod("getProvisions",signature(spec="NoParamProbModelSpecifier"),
    function(spec){
        return(list(Outcomes=c("None",spec@OutcomeName),TimesToOutcomes=c("None")))
    }
)

## Method: generateOutcomes 
# This method returns an updated "thisPatCurrentData" with the generated outcome(s)
# from this patient's most recent set of concurrent treatments. 
setGeneric("generateOutcomes",function(outcomeModelSpec,thisPatCurrentData) standardGeneric("generateOutcomes"))

setMethod("generateOutcomes",signature(outcomeModelSpec="DoseThresholdModelSpecifier",thisPatCurrentData="PatData"),
    function(outcomeModelSpec,thisPatCurrentData){
        DoseThresholdName <- outcomeModelSpec@DoseThresholdName
        DoseThreshold <- thisPatCurrentData@BaseChars[DoseThresholdName]
        ThisPatOutcome <- 0
        # Find most recent set of concurrent treatments
        NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
        Dose <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
        if (Dose > DoseThreshold) ThisPatOutcome <- 1
        if (DoseThresholdName == "ToxDoseThreshold") names(ThisPatOutcome) <- "BinaryToxicity"
        else names(ThisPatOutcome) <- "BinaryEfficacy"
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
        Dose <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
        if (Dose > ToxDoseThreshold * DeltaDeath) ThisPatOutcomes["Death"] <- 1
        if (Dose > ToxDoseThreshold) ThisPatOutcomes["BinaryToxicity"] <- 1
        thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcomes
        return(thisPatCurrentData)
    }
)
    
setMethod("generateOutcomes",signature(outcomeModelSpec="ToxEfficacyDoseThresholdsModelSpecifier",thisPatCurrentData="PatData"),
    function(outcomeModelSpec,thisPatCurrentData){
        # Find most recent set of concurrent treatments
        NConcurrents <- length(thisPatCurrentData@ConcurrentTrtsDataList)
        Dose <- thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@TrtAllos$Dose
        ThisPatOutcomes <- sapply(thisPatCurrentData@BaseChars[c("ToxDoseThreshold","EfficacyDoseThreshold")],
            function(x) ifelse(Dose>x,1,0))
        names(ThisPatOutcomes) <- c("BinaryToxicity","BinaryEfficacy")
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
        Dose <- thisPatCurrentTrtsData@TrtAllos$Dose
        if(nrow(Probs)==1) 
            ThisPatOutcome <- rbinom(1,1,prob=Probs[1,Dose==outcomeModelSpec@TierDoses])
        else {
            SubPopIndex <- thisPatCurrentData@BaseChars["SubPopIndex"]
            ThisPatOutcome <- rbinom(1,1,prob=Probs[SubPopIndex,Dose==outcomeModelSpec@TierDoses])
        }
        names(ThisPatOutcome) <- outcomeModelSpec@OutcomeName
        thisPatCurrentData@ConcurrentTrtsDataList[[NConcurrents]]@Outcomes <- ThisPatOutcome
        return(thisPatCurrentData)
    }
)
        
## Method: generatePatsOutcomes
# This method is an "Action" method 
# patsIndices are the enrollment order numbers of patients, which should be the same as the element order numbers in "PatsData" 
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
# This class represents the specification for a "A+B with dose de-escalation" design
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
# EstimationMethod: Bayes
# Switching stage rule for the two-stage CRM: switch from the initial stage when the first toxicity outcome is observed
# When "EscalationRestriction"=TRUE and "NPatsPerCohort" = 1, design will avoid 
# (1) skipping doses in escalation and (2) escalation immediately after a toxic outcome (i.e., incoherent escalation).
# When EscalationRestriction=TRUE and NPatsPerCohort > 1, design will avoid 
# (1) skipping doses in escalation and (2) escalation immediately after the probability of toxicity in the last cohort >= "TargetProb".
# If "DeltaToxProbForEarlyStop" is not NULL, the trial may be stopped early when the estimated toxicity probability for the 
# first dose > "DeltaToxProbForEarlyStop" + "TargetProb"; and "SampleSize" would be the maximum sample size
setClass("CRMSpecifier",representation(InitialProbGuesses="numeric",TierDoses="numeric",TargetProb="numeric",SampleSize="numeric",
    StartingDoseLevel="OptionalNumeric",InitialStageDoseLevels="OptionalNumeric",NPatsPerCohort="numeric",EscalationRestriction="logical",
    DeltaToxProbForEarlyStop="OptionalNumeric",OutcomeModelType="character"),
    contains="DesignSpecifier",
    prototype=list(NPatsPerCohort=1,EscalationRestriction=TRUE,DeltaToxProbForEarlyStop=NULL,OutcomeModelType="Exponential"),
    validity=function(object){
        InitialProbGuesses <- object@InitialProbGuesses
        if(length(InitialProbGuesses) > 1){
            for( i in 2:length(InitialProbGuesses)){
                if (InitialProbGuesses[i]<InitialProbGuesses[i-1])
                stop("'InitialProbGuesses' are not in non-decreasing order!")
            }
        }
        TierDoses <- object@TierDoses
        NTierDoses <- length(TierDoses)
        if(length(InitialProbGuesses)!=NTierDoses) 
            stop("The number of initial toxicity probability guesses is not the same as the number of tier doses!")
        InitialStageDoseLevels <- object@InitialStageDoseLevels
        NInitialStageDoseLevels <- length(InitialStageDoseLevels)
        StartingDoseLevel <- object@StartingDoseLevel
        SampleSize <- object@SampleSize
        if(NInitialStageDoseLevels != 0){
            if(NInitialStageDoseLevels != SampleSize) stop("The length of 'InitialStageDoseLevels' should be the same as 'SampleSize'!")
            if(max(InitialStageDoseLevels)>NTierDoses) 
                stop("The maximum dose level in 'InitialStageDoseLevels' should be no larger than the length of 'TierDoses'!")
            if(!is.null(StartingDoseLevel)){
                if(StartingDoseLevel!=NInitialStageDoseLevels[1]) print("The first dose level in 'InitialStageDoseLevels' will be used as the starting dose level!")
            }
            if(NInitialStageDoseLevels > 1){
                for(i in 2:NInitialStageDoseLevels){
                    if (InitialStageDoseLevels[i]<InitialStageDoseLevels[i-1])
                    stop("Dose levels in 'InitialStageDoseLevels' are not in non-decreasing order!")
                }
            }  
        }
        else {
            if(is.null(StartingDoseLevel)) stop("'StartingDoseLevel' and 'InitialStageDoseLevels' cannot be both NULL!")
        }   
        if(!is.null(StartingDoseLevel)){
             if(StartingDoseLevel > NTierDoses)
            stop("'StartingDoseLevel' should be no larger than the length of 'TierDoses'!")
        }
        if(is.na(match(object@OutcomeModelType,c("Exponential","Logit")))) stop("Unknown outcome model type!")
        NPatsPerCohort <- object@NPatsPerCohort
        if(SampleSize/NPatsPerCohort != round(SampleSize/NPatsPerCohort)) stop("The number of patients is not multiple of
            cohort size!")
        DeltaToxProbForEarlyStop <- object@DeltaToxProbForEarlyStop
        if(!is.null(DeltaToxProbForEarlyStop)) {
            if (DeltaToxProbForEarlyStop<0) stop("'DeltaToxProbForEarlyStop' should be nonnegative!")
        } 
        else TRUE
    })
    
## Class: CRMExpSpecifier
# This class represents specification for CRM design using one-parameter exponential outcome model 
# Model: P(toxicity)=(scaled dose)^exp(Beta)   
# Currently, if Beta prior is discrete, we only allow its specification to be an "UnivarValueProbSpecifier" object
setClass("CRMExpSpecifier",representation(BetaPriorSpec="DistrSpecifier"),
    contains="CRMSpecifier",
    validity=function(object){
        BetaPriorSpec <- object@BetaPriorSpec
        if(object@OutcomeModelType!="Exponential") stop("The assumed outcome model type is wrong!")
        else if (is.na(getMean(BetaPriorSpec))) 
            stop("Scaled doses cannot be calculated because the Beta prior mean is not applicable!")
        else if(is(BetaPriorSpec,"DiscreteDistrSpecifier")){
            if(!is(BetaPriorSpec,"UnivarValueProbSpecifier")) stop("If Beta prior is discrete, we currently only
                allow its specification to be an 'UnivarValueProbSpecifier' object!")
        } 
        else TRUE
    }
)    
    
## Class: CRMLogit1Specifier
# This class represents specification for CRM design using logit model with intercept and log(slope) as parameters. 
# Model: logit(P(toxicity))=alpha+exp(Beta)*(scaled dose) 
# We assume the priors for alpha and beta are independent 
# Currently, if a parameter's prior is discrete, we only allow its specification to be an "UnivarValueProbSpecifier" object  
setClass("CRMLogit1Specifier",representation(AlphaPriorSpec="NumericDistrSpecifier",BetaPriorSpec="DistrSpecifier"),
    contains="CRMSpecifier",
    validity=function(object){
        if(object@OutcomeModelType!="Logit") stop("The assumed outcome model type is wrong!")
        else{
            InitialProbGuesses <- object@InitialProbGuesses
            AlphaPriorSpec <- object@AlphaPriorSpec
            BetaPriorSpec <- object@BetaPriorSpec
            ClassAlphaPriorSpec <- class(AlphaPriorSpec)
            BetaPriorMean <- getMean(BetaPriorSpec)
            if(ClassAlphaPriorSpec=="numeric"){
                if(is.na(BetaPriorMean)) 
                    stop("Scaled doses cannot be calculated because the Beta prior mean is not applicable!")
                else{
                    ScaledDoses <- (log(InitialProbGuesses/(1-InitialProbGuesses))-AlphaPriorSpec)/exp(BetaPriorMean)
                    # The M2 regularity condition for the dose-toxicity response model in the appendix A of the paper (Cheung and Chappell,
                    # Biometrics,58,671-674
                    if(!all(ScaledDoses<0) & !all(ScaledDoses>0)) stop("Scaled doses should have the same sign")
                }
            }
            else{
                AlphaPriorMean <- getMean(AlphaPriorSpec)
                if(is.na(AlphaPriorMean) | is.na(BetaPriorMean))
                    stop("Scaled doses cannot be calculated because the prior mean for either Alpha or Beta is not applicable!")
            }
            if(is(BetaPriorSpec,"DiscreteDistrSpecifier")){
                if(!is(BetaPriorSpec,"UnivarValueProbSpecifier")) stop("If Beta prior is discrete, we currently only
                    allow its specification to be an 'UnivarValueProbSpecifier' object!")
            }
            else if(is(AlphaPriorSpec,"DiscreteDistrSpecifier")){
                if(!is(AlphaPriorSpec,"UnivarValueProbSpecifier")) stop("If Alpha prior is discrete, we currently only
                    allow its specification to be an 'UnivarValueProbSpecifier' object!")
            }
            else TRUE
        }
    }
)        
    
## Class: Phase2BryantDaySpecifier
# Bryant and Day Phase II trial design is described in the article, Biometrics, v51, 1372-1383, 1995
setClass("Phase2BryantDaySpecifier",representation(N1Pats="numeric",NPats="numeric",Efficacy1LL="numeric",EfficacyLL="numeric",
    NonTox1LL="numeric",NonToxLL="numeric"),
    contains="DesignSpecifier")
## Class: CRMLogit2Specifier
# This class represents specification for CRM design using re-parameterized logit model which has two parameters: the toxicity 
# probability at the starting dose level(Ps) and ratio of the true log odds ratio and the initiall assumed log odds ratio (Gamma). 
# Currently, if a parameter's prior is discrete, we only allow its specification to be an "UnivarValueProbSpecifier" object. 
setClass("CRMLogit2Specifier",representation(PsPriorSpec="DistrSpecifier",LogGammaPriorSpec="NumericDistrSpecifier"),
    contains="CRMSpecifier",
    validity=function(object){ 
         if(object@OutcomeModelType!="Logit") stop("The assumed outcome model type is wrong!")
         else{
            PsPriorSpec <- object@PsPriorSpec
            LogGammaPriorSpec <- object@LogGammaPriorSpec
            if(is(PsPriorSpec,"DiscreteDistrSpecifier")){
                if(!is(PsPriorSpec,"UnivarValueProbSpecifier")) stop("If Ps prior is discrete, we currently only
                    allow its specification to be an 'UnivarValueProbSpecifier' object!")
            }
            else if(is(LogGammaPriorSpec,"DiscreteDistrSpecifier")){
                if(!is(LogGammaPriorSpec,"UnivarValueProbSpecifier")) stop("If LogGamma prior is discrete, we currently only
                    allow its specification to be an 'UnivarValueProbSpecifier' object!")
            }
            else TRUE
         }
    }
)
      
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
        return(list(TrtAllos=c("None","Dose"),CTTimes=c("None"),PatTimes=c("None"),Conclusions=c("None","RP2D")))
    }
)

# This method is to get provisions from an "CRMSpecifier" object
# RP2DL: Recommended Phase 2 dose level
setMethod("getProvisions",signature(spec="CRMSpecifier"),
    function(spec){
        return(list(TrtAllos=c("None","Dose","Dose Level"),CTTimes=c("None"),PatTimes=c("None"),
            Conclusions=c("None","EarlyStop","RP2D","RP2DL")))
    }
)

## Method: initializeTempData
# This method is to initialize a temporary data repository for improving the computation efficiency
setGeneric("initializeTempData", function(designSpec) standardGeneric("initializeTempData"))

setMethod("initializeTempData",signature(designSpec="APlusBSpecifier"),function(designSpec) return(NULL))

setMethod("initializeTempData",signature(designSpec="CRMSpecifier"),function(designSpec) return(NULL))

## Method: generateInitialActions
# This method is to generate a list of initial actions in the order of their execution times
setGeneric("generateInitialActions", function(designSpec) standardGeneric("generateInitialActions"))

# patsIndices should be in the ascending order
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
) 

# This method will also globalize the slots of a "CRMSpecifier" object and some relevant variables
setMethod("generateInitialActions",signature(designSpec="CRMSpecifier"),
    function(designSpec){
        sapply(slotNames(designSpec),function(x) assign(x,slot(designSpec,x),pos=1))
        assign("NDoseLevels",length(TierDoses),pos=1)  ### TODO: can we avoid globbing this?
        if(OutcomeModelType=="Exponential") assign("ScaledDoses",
            exp(log(InitialProbGuesses)/exp(getMean(designSpec))),pos=1)
        else if (class(designSpec)=="CRMLogit1Specifier"){
            BetaPriorMean <- getMean(BetaPriorSpec)
            assign("Param1PriorSpec",AlphaPriorSpec,pos=1)   ### TODO: can we avoid globbing this?
            assign("Param2PriorSpec",BetaPriorSpec,pos=1)   ### TODO: can we avoid globbing this?
            if (class(AlphaPriorSpec)=="numeric")
                assign("ScaledDoses",(log(InitialProbGuesses/(1-InitialProbGuesses))-AlphaPriorSpec)/exp(BetaPriorMean),
                    pos=1)
            else
                assign("ScaledDoses",(log(InitialProbGuesses/(1-InitialProbGuesses))-getMean(AlphaPriorSpec))/exp(BetaPriorMean),
                    pos=1)
        }
        else{
            assign("Param1PriorSpec",PsPriorSpec,pos=1)
            assign("Param2PriorSpec",LogGammaPriorSpec,pos=1)
            if(is.null(StartingDoseLevel)) StartLevel <- InitialStageDoseLevels[1]
            else StartLevel <- StartingDoseLevel
            assign("StartLevel",StartLevel,pos=1)
            assign("InitialLogORGuesses",fLogit(InitialProbGuesses)-fLogit(InitialProbGuesses[StartLevel]),pos=1)
        }
        assign("DesignSpec",designSpec,pos=1)
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
            # Vector for previously assigned doses, in the ascending order of patients' enrollment times
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
            NToxsOnPreviousDose <- sum(sapply(PatsIndicesOnPreviousDose,function(x)
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"] == 1))
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

## Some functions used in the methods associated with the CRM design
# Likelihood function when the assumed outcome model is one-parameter exponential or Logit model 
# This parameter can be Beta or Ps. 
# "par" in the following functions refers to either Beta or LogOddsS (the log odds at the starting dose level)
.fLikelihood1CRM <- function(par){
    Likelihood <- 1
    if(OutcomeModelType=="Exponential"){
        for ( i in 1:length(AssignedScaledDoses))
            Likelihood <- Likelihood * ((AssignedScaledDoses[i]^exp(par))^Outcomes[i]) * ((1-AssignedScaledDoses[i]^exp(par))^(1-Outcomes[i]))
    }
    else if(class(DesignSpec)=="CRMLogit1Specifier"){
        for ( i in 1:length(AssignedScaledDoses)){
            Prob <- (1+exp(-AlphaPriorSpec-exp(par)*AssignedScaledDoses[i]))^(-1)
            Likelihood <- Likelihood * (Prob^Outcomes[i]) * ((1-Prob)^(1-Outcomes[i]))
        }
    }
    else{
       for ( i in 1:length(AssignedDoseLevels)){
            if(AssignedDoseLevels[i]==1) Prob <- fAntiLogit(par)
            else Prob <- fAntiLogit(par+exp(LogGammaPriorSpec) * InitialLogORGuesses[AssignedDoseLevels[i]])
            Likelihood <- Likelihood * (Prob^Outcomes[i]) * ((1-Prob)^(1-Outcomes[i]))
        }
    } 
    return(Likelihood)
}

# Function: .fPosterior1CRM
.fPosterior1CRM <- function(par){
    if(class(DesignSpec)=="CRMLogit2Specifier"){
        if (is(PsPriorSpec,"ContinuousDistrSpecifier"))
            return(((exp(par/2)+exp(-par/2))^(-2)) * getDensity(x=fAntiLogit(par),distrSpec=PsPriorSpec)*.fLikelihood1CRM(par))
        else
            return(getDensity(x=fAntiLogit(par),distrSpec=PsPriorSpec)*.fLikelihood1CRM(par))
    }
    else
        return(getDensity(x=par,distrSpec=BetaPriorSpec)*.fLikelihood1CRM(par))
}

# Function: .fPosteriorXparCRM
.fPosteriorXParCRM <- function(par){
        return(par*.fPosterior1CRM(par))
}

# Function: .fEstimateParCRM
.fEstimateParCRM <- function(){
    if(class(DesignSpec)=="CRMLogit2Specifier") ParPriorSpec <- PsPriorSpec
    else ParPriorSpec <- BetaPriorSpec
    if(is(ParPriorSpec,"UnivarValueProbSpecifier")){  
        if(class(DesignSpec)=="CRMLogit2Specifier")   
            Values <- fLogit(ParPriorSpec@Values)
        else Values <- ParPriorSpec@Values
        Probs <- ParPriorSpec@Probs
        # a vector of integrands for the numerator in calculating the posterior mean
        NumIntegrands <- sapply(1:length(Values),function(x){
            Values[x] * .fLikelihood1CRM(par=Values[x]) * Probs[x]
        })
        # a vector of integrands for the denominator in calculating the posterior mean
        DenIntegrands <- sapply(1:length(Values),function(x){
            .fLikelihood1CRM(par=Values[x]) * Probs[x]
        })
        return(sum(NumIntegrands)/sum(DenIntegrands))
    }
    else
        return(integrate(.fPosteriorXParCRM,-Inf,Inf)[[1]]/integrate(.fPosterior1CRM,-Inf,Inf)[[1]])
}

# Function: .fLikelihood2CRM
# Likelihood function when the assumed outcome model is two-parameter logit model
# (par1,par2) can be either (Alpha,Beta) or (LogOddsS,LogGamma), where LogOddsS is the log odds at the starting dose level
.fLikelihood2CRM <- function(par1,par2){
    Likelihood <- 1
    if(is(DesignSpec,"CRMLogit1Specifier")){
        for ( i in 1:length(AssignedScaledDoses)){
            Prob <- (1+exp(-par1-exp(par2)*AssignedScaledDoses[i]))^(-1)
            Likelihood <- Likelihood * (Prob^Outcomes[i]) * ((1-Prob)^(1-Outcomes[i]))
        }
    }
    else{
        for ( i in 1:length(AssignedDoseLevels)){
            if(AssignedDoseLevels[i]==1) Prob <- fAntiLogit(par1)
            else Prob <- fAntiLogit(par1+exp(par2) * InitialLogORGuesses[AssignedDoseLevels[i]])
            Likelihood <- Likelihood * (Prob^Outcomes[i]) * ((1-Prob)^(1-Outcomes[i]))
        }
    }
    return(Likelihood)
}

# Function: .fPosterior2CRM
.fPosterior2CRM <- function(par1,par2){  
    if(class(DesignSpec)=="CRMLogit1Specifier")
        return(getDensity(x=par1,distrSpec=AlphaPriorSpec) * getDensity(x=par2,distrSpec=BetaPriorSpec) *
            .fLikelihood2CRM(par1,par2))
    else if (is(PsPriorSpec,"ContinuousDistrSpecifier"))
        return(((exp(par1/2)+exp(-par1/2))^(-2)) * getDensity(x=fAntiLogit(par1),distrSpec=PsPriorSpec) * 
            getDensity(x=par2,distrSpec=LogGammaPriorSpec) * .fLikelihood2CRM(par1,par2))
    else
        return(getDensity(x=fAntiLogit(par1),distrSpec=PsPriorSpec) * 
            getDensity(x=par2,distrSpec=LogGammaPriorSpec) * .fLikelihood2CRM(par1,par2))
}

# Function: .fPosterior2ThetaCRM
.fPosterior2ThetaCRM <- function(theta){   
    Par1 <- theta[1]
    Par2 <- theta[2]
    return(.fPosterior2CRM(Par1,Par2))
}

# Function: .fPosterior2XPar1CRM
.fPosterior2XPar1CRM <- function(par1,par2){
    return(par1*.fPosterior2CRM(par1,par2))
}

# Function: .fPosterior2XPar2CRM
.fPosterior2XPar2CRM <- function(par1,par2){
    return(par2*.fPosterior2CRM(par1,par2))
}

# Function: .fMCMCEstimate2CRM
.fMCMCEstimate2CRM <- function(){
    if(class(DesignSpec)=="CRMLogit2Specifier"){
        PosteriorSample <- MCMCmetrop1R(fun=.fPosterior2ThetaCRM,
            theta.init=c(fLogit(InitialProbGuesses[StartLevel]),getMean(LogGammaPriorSpec)),
            mcmc=1000,tune=c(1.5,1.5),seed=193,logfun=FALSE)
        PosteriorMeans <- c(mean(PosteriorSample[,1]),mean(PosteriorSample[,2]))
    }
    else {
        PosteriorSample <- MCMCmetrop1R(fun=.fPosterior2ThetaCRM,
            theta.init=c(getMean(AlphaPriorSpec),getMean(BetaPriorSpec)),
            mcmc=1000,tune=c(1.5,1.5),seed=193,logfun=FALSE)
        PosteriorMeans <- c(mean(PosteriorSample[,1]),mean(PosteriorSample[,2]))
    }
    return(PosteriorMeans)
}
            
# Function: .fEstimate2ParsCRM
# Returns a vector of posterior means for two parameters (either (Alpha,Beta) or (LogOdds,LogGamma) respectively
.fEstimate2ParsCRM <- function(){  
    if(is(Param1PriorSpec,"UnivarValueProbSpecifier")) {
        if(class(DesignSpec)=="CRMLogit2Specifier"){
            if(is(Param2PriorSpec,"UnivarValueProbSpecifier")){
                NumeratorPar1 <- sum(sapply(fLogit(Param1PriorSpec@Values),function(par1)
                    sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2XPar1CRM(par1=par1,par2=par2)))))
                NumeratorPar2 <- sum(sapply(fLogit(Param1PriorSpec@Values),function(par1)
                    sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2XPar2CRM(par1=par1,par2=par2)))))
                Denominator <- sum(sapply(fLogit(Param1PriorSpec@Values),function(par1)
                    sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2CRM(par1=par1,par2=par2)))))
            }
            else{
                NumeratorPar1 <- integrate(f<-Vectorize(function(par2)
                    sum(sapply(fLogit(Param1PriorSpec@Values),function(par1).fPosterior2XPar1CRM(par1=par1,par2=par2)))
                ), -Inf, Inf)$value
                NumeratorPar2 <- integrate(f<-Vectorize(function(par2) 
                    sum(sapply(fLogit(Param1PriorSpec@Values),function(par1).fPosterior2XPar2CRM(par1=par1,par2=par2)))
                ), -Inf, Inf)$value
                Denominator <- integrate(f<-Vectorize(function(par2) 
                    sum(sapply(fLogit(Param1PriorSpec@Values),function(par1).fPosterior2CRM(par1=par1,par2=par2)))
                ), -Inf, Inf)$value
            }
        }
        else{
            if(is(Param2PriorSpec,"UnivarValueProbSpecifier")){
                NumeratorPar1 <- sum(sapply(Param1PriorSpec@Values,function(par1)
                    sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2XPar1CRM(par1=par1,par2=par2)))))
                NumeratorPar2 <- sum(sapply(Param1PriorSpec@Values,function(par1)
                    sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2XPar2CRM(par1=par1,par2=par2)))))
                Denominator <- sum(sapply(Param1PriorSpec@Values,function(par1)
                    sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2CRM(par1=par1,par2=par2)))))
            }
            else{
                NumeratorPar1 <- integrate(f<-Vectorize(function(par2) 
                    sum(sapply(Param1PriorSpec@Values,function(par1) .fPosterior2XPar1CRM(par1=par1,par2=par2)))), -Inf, Inf)$value
                NumeratorPar2 <- integrate(f<-Vectorize(function(par2) 
                    sum(sapply(Param1PriorSpec@Values,function(par1) .fPosterior2XPar2CRM(par1=par1,par2=par2)))), -Inf, Inf)$value
                Denominator <- integrate(f<-Vectorize(function(par2) 
                    sum(sapply(Param1PriorSpec@Values,function(par1) .fPosterior2CRM(par1=par1,par2=par2)))), -Inf, Inf)$value
            }
        }     
        PosteriorMeans <- c(NumeratorPar1/Denominator,NumeratorPar2/Denominator)
    }
    else if(is(Param2PriorSpec,"UnivarValueProbSpecifier")){
        NumeratorPar1 <- integrate(f<-Vectorize(function(par1) 
            sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2XPar1CRM(par1=par1,par2=par2)))), -Inf, Inf)$value
        NumeratorPar2 <- integrate(f<-Vectorize(function(par1) 
            sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2XPar2CRM(par1=par1,par2=par2)))), -Inf, Inf)$value
        Denominator <- integrate(f<-Vectorize(function(par1) 
            sum(sapply(Param2PriorSpec@Values,function(par2) .fPosterior2CRM(par1=par1,par2=par2)))), -Inf, Inf)$value
        PosteriorMeans <- c(NumeratorPar1/Denominator,NumeratorPar2/Denominator)
    }
    else{   
        .fDPar1 <- function(par1){
            integrate(function(par2) .fPosterior2CRM(par1,par2),lower=-Inf,upper=Inf)$value
        }
        .fN1Par1 <- function(par1){
            integrate(function(par2) .fPosterior2XPar1CRM(par1,par2),lower=-Inf,upper=Inf)$value
        }
        .fN2Par1 <- function(par1){
            integrate(function(par2) .fPosterior2XPar2CRM(par1,par2),lower=-Inf,upper=Inf)$value
        }
        Denominator <- try(integrate(Vectorize(.fDPar1), lower=-Inf,upper=Inf)$value,silent=T)
        if(class(Denominator)=="try-error") PosteriorMeans<-.fMCMCEstimate2CRM()
        else{
            NumeratorPar1 <- try(integrate(Vectorize(.fN1Par1), lower=-Inf,upper=Inf)$value,silent=T)
            if(class(NumeratorPar1)=="try-error") PosteriorMeans<-.fMCMCEstimate2CRM()
            else{
                NumeratorPar2 <- try(integrate(Vectorize(.fN2Par1), lower=-Inf,upper=Inf)$value,silent=T)
                if(class(NumeratorPar2)=="try-error") PosteriorMeans<-.fMCMCEstimate2CRM()
                else PosteriorMeans <- c(NumeratorPar1/Denominator,NumeratorPar2/Denominator)
            }
        }
    }
    names(PosteriorMeans) <- c("Par1","Par2")
    return(PosteriorMeans)
}

# Function: .fFindDoseLevelSingleStageCRM
# It will return an assigned dose based on a specified CRM design and accumulating data
.fFindDoseLevelSingleStageCRM <- function(lastAssignedDoseLevel){
    if(OutcomeModelType=="Exponential")
        Probs <- ScaledDoses^exp(.fEstimateParCRM())
    else if(class(DesignSpec)=="CRMLogit1Specifier"){
        if(is.numeric(AlphaPriorSpec))
        Probs <- (1+exp(-AlphaPriorSpec-exp(.fEstimateParCRM())*ScaledDoses))^(-1)
        else Probs <- (1+exp(-.fEstimate2ParsCRM()["Par1"]-exp(.fEstimate2ParsCRM()["Par2"])*ScaledDoses))^(-1)
    }
    else{
        if(is.numeric(LogGammaPriorSpec)) 
            Probs <- fAntiLogit(.fEstimateParCRM()+exp(LogGammaPriorSpec)*InitialLogORGuesses)
        else Probs <- fAntiLogit(.fEstimate2ParsCRM()["Par1"]+exp(.fEstimate2ParsCRM()["Par2"])*InitialLogORGuesses)
    }
    ModelBasedDoseLevel <- order(abs(Probs-TargetProb))[1]
    if(EscalationRestriction){
        if(sum(Outcomes[(length(Outcomes)-NPatsPerCohort+1):length(Outcomes)])/NPatsPerCohort >= TargetProb)
            DoseLevelToAssign <- min(ModelBasedDoseLevel,lastAssignedDoseLevel)
        else DoseLevelToAssign <- min(ModelBasedDoseLevel,lastAssignedDoseLevel+1)
    }
    else DoseLevelToAssign <- ModelBasedDoseLevel
    if (!is.null(DeltaToxProbForEarlyStop)){
        if(Probs[1] > TargetProb+DeltaToxProbForEarlyStop) DoseLevelToAssign <- 0
    }
    return(DoseLevelToAssign)
}

# Function: .fCurrentSS
# It returns current sufficient statistics as a vector
.fCurrentSS<- function(){
    NTs <-rep(0,NDoseLevels)
    NNTs <- rep(0,NDoseLevels)
    names(NTs)<-as.character(1:NDoseLevels)
    names(NNTs)<-as.character(1:NDoseLevels)
    SSTable <- table(Outcomes,AssignedDoseLevels)
    NamesTable <- colnames(SSTable)
    if(sum(Outcomes)!=0){
        NTsTable<-SSTable["1",]
        NTs[NamesTable]<-NTsTable
    }
    if(sum(Outcomes)!=length(Outcomes)){
        NNTsTable<-SSTable["0",]
        NNTs[NamesTable]<-NNTsTable
    }
    return(c(NTs,NNTs))
}

# Function: .fMatchTempData
# It will return a vector with the first element logical and the second element row index in the TempData
.fMatchTempData <- function(currentSS){
    TempData2 <- as.matrix(TempData[,-ncol(TempData)])
    Match <- apply(TempData2,1,function(x) all(currentSS==x))
    return(c(any(Match),which(Match)))
}

# Function: .f2FindDoseLevelSingleStageCRM
# Compared to .fFindDoseLevelSingleStageCRM, it will improve the computation efficiency by using the
# data from previous CT replications
# It will return an assigned dose based on a specified CRM design and accumulating data
.f2FindDoseLevelSingleStageCRM <- function(lastAssignedDoseLevel){
    if(class(try(TempData,silent=TRUE))=="try-error") return(.fFindDoseLevelSingleStageCRM(lastAssignedDoseLevel))
    else if(is.null(TempData)) {
        NextLevel <- .fFindDoseLevelSingleStageCRM(lastAssignedDoseLevel)
        TempData <- matrix(c(.fCurrentSS(),NextLevel),nrow=1)
        colnames(TempData) <- c(sapply(1:NDoseLevels,function(x) paste("NTsLevel",x,sep="")),
            sapply(1:NDoseLevels,function(x) paste("NNTsLevel",x,sep="")),"NextLevel")
        assign("TempData",TempData,pos=1)
        return(NextLevel)
    }
    else{
        Match <- .fMatchTempData(.fCurrentSS())
        if(Match[1]) return(TempData[Match[2],"NextLevel"])
        else {
            NextLevel <- .fFindDoseLevelSingleStageCRM(lastAssignedDoseLevel)
            TempData <- rbind(TempData,c(.fCurrentSS(),NextLevel))
            assign("TempData",TempData,pos=1)
            return(NextLevel)
        }
    }
}
         
# Method: allocateTrts for the CRM design
setMethod("allocateTrts",signature(designSpec="CRMSpecifier",currentCTData="CTData",currentGlobalTime="numeric",
    patsIndices="numeric"),
    function(designSpec,currentCTData,currentGlobalTime,patsIndices){  
        NPats <- length(currentCTData@PatsData)
        TierDoses <- designSpec@TierDoses
        NewAction1 <- new("Action",MethodCall="generatePatsOutcomes(outcomeModelSpec=outcomeModelSpec,patsIndices=PatsIndices,currentCTData=CurrentCTData)",
                    OtherArgs=list(PatsIndices=patsIndices),GlobalTime=currentGlobalTime+1)
        NewAction2 <- new("Action",MethodCall="checkStoppingRule(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime)",
                    OtherArgs=list(),GlobalTime=currentGlobalTime+2)
        # When the CRM design is one-stage
        if(is.null(InitialStageDoseLevels)){
            # when allocating treatments to the first patient cohort 
            if(NPats==NPatsPerCohort){ 
                ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",
                    TrtAllos=data.frame(Dose=TierDoses[StartingDoseLevel],DoseLevel=StartingDoseLevel)))
                for ( i in patsIndices)
                    currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- ThisPatConcurrentTrtsDataList
            }
            # when at least one patient cohort has been treated
            else {
                PreviousOutcomes <- sapply(1:(NPats-NPatsPerCohort),function(x) 
                    currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])
                PreviousDoseLevels <- sapply(1:(NPats-NPatsPerCohort),function(x) 
                    currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@TrtAllos$DoseLevel)
                assign("AssignedDoseLevels", PreviousDoseLevels,pos=1)
                if(class(designSpec)=="CRMExpSpecifier"|class(designSpec)=="CRMLogit1Specifier")
                    assign("AssignedScaledDoses", ScaledDoses[PreviousDoseLevels],pos=1)
                assign("Outcomes",as.numeric(PreviousOutcomes),pos=1)
                ThisPatAssignedDoseLevel <- .f2FindDoseLevelSingleStageCRM(lastAssignedDoseLevel=PreviousDoseLevels[NPats-NPatsPerCohort])
                if (ThisPatAssignedDoseLevel != 0){ 
                    ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[ThisPatAssignedDoseLevel],
                        DoseLevel=ThisPatAssignedDoseLevel)))
                    for ( i in patsIndices)
                        currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- ThisPatConcurrentTrtsDataList
                }
                else{
                    # Record the patient index where the trial is early stopped
                    currentCTData@Conclusions <- NPats-NPatsPerCohort
                    names(currentCTData@Conclusions) <- "EarlyStop"
                    # Get rid of the current patient cohort from currentCTData
                    currentCTData@PatsData <- currentCTData@PatsData[1:(NPats-NPatsPerCohort)]
                    return(list(NewCTData=currentCTData,NewActions=c(NewAction2)))
                }
            }
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
                    currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])
            PreviousDoseLevels <- sapply(1:(NPats-NPatsPerCohort),function(x) 
                    currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@TrtAllos$DoseLevel) 
            assign("AssignedDoseLevels", PreviousDoseLevels,pos=1)
            if(class(designSpec)=="CRMExpSpecifier"|class(designSpec)=="CRMLogit1Specifier")
                assign("AssignedScaledDoses", ScaledDoses[PreviousDoseLevels],pos=1)
            assign("Outcomes",PreviousOutcomes,pos=1)
            ThisPatAssignedDoseLevel <- .f2FindDoseLevelSingleStageCRM(lastAssignedDoseLevel=PreviousDoseLevels[NPats-NPatsPerCohort])
            if (ThisPatAssignedDoseLevel != 0){ 
                    ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[ThisPatAssignedDoseLevel],
                        DoseLevel=ThisPatAssignedDoseLevel)))
                    for ( i in patsIndices)
                        currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- ThisPatConcurrentTrtsDataList
                }
                else{
                    # Record the patient index where the trial is early stopped
                    currentCTData@Conclusions <- NPats-NPatsPerCohort
                    names(currentCTData@Conclusions) <- "EarlyStop"
                    # Get rid of the current patient cohort from currentCTData
                    currentCTData@PatsData <- currentCTData@PatsData[1:(NPats-NPatsPerCohort)]
                    return(list(NewCTData=currentCTData,NewActions=c(NewAction2)))
                }
        }
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
        AssignedDoses <- sapply(currentCTData@PatsData[1:NPats],function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
        LargestAssignedDoses <- max(AssignedDoses)
        # Indices for patients who are on current dose
        PatsIndicesOnCurrentDose <- (1:NPats)[AssignedDoses==CurrentDose]
        # Number of patients on current dose, which can be either A or (A+B)
        NPatsOnCurrentDose <- length(PatsIndicesOnCurrentDose)
        # Number of toxicity outcomes observed on current dose
        NToxsOnCurrentDose <- sum(rep(1,NPatsOnCurrentDose)[sapply(PatsIndicesOnCurrentDose,function(x)
            currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"] == 1)])
        if ((NPatsOnCurrentDose==A & NToxsOnCurrentDose>D) | (NPatsOnCurrentDose==APlusB & NToxsOnCurrentDose>E)){
            if (CurrentDose==StartingDose){
                currentCTData@Conclusions <- c(NA,1,0)
                # Conclusions["TooToxic"]=1 indicates all the testing doses are too toxic
                # Conclusions["StillSafe"]=1 indicates the highest doses is still safe
                names(currentCTData@Conclusions) <- c("RP2D","TooToxic","StillSafe")
                return (list(NewCTData=currentCTData))
            }
            else {
                # Dose that is one level lower than current dose
                Level1LowerDose <- TierDoses[CurrentDoseLevel-1]
                # Indices for patients who are on the dose with one level lower than current dose
                PatsIndicesOn1LevelLowerDose <- (1:NPats)[AssignedDoses==Level1LowerDose]
                # Number of patients on that dose, which can be either A or (APlusB)
                NPatsOn1LevelLowerDose <- length(PatsIndicesOn1LevelLowerDose)
                # Number of toxicity outcomes observed on that dose
                NToxsOn1LevelLowerDose <- sum(rep(1,NPatsOn1LevelLowerDose)[sapply(PatsIndicesOn1LevelLowerDose,function(x)
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"] == 1)])
                if (NPatsOn1LevelLowerDose==APlusB & NToxsOn1LevelLowerDose<=E) {
                    currentCTData@Conclusions <- c(Level1LowerDose,0,0)
                    names(currentCTData@Conclusions) <- c("RP2D","TooToxic","StillSafe")
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
            if(CurrentDose==LargestAssignedDoses){
                if(CurrentDose==HighestDose){
                    currentCTData@Conclusions <- c(NA,0,1)
                    names(currentCTData@Conclusions) <- c("RP2D","TooToxic","StillSafe")
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
                currentCTData@Conclusions <- c(CurrentDose,0,0)
                names(currentCTData@Conclusions) <- c("RP2D","TooToxic","StillSafe")
                return (list(NewCTData=currentCTData))
            }
        }
        else if(NPatsOnCurrentDose==A & NToxsOnCurrentDose<C){
            if(CurrentDose==HighestDose){
                currentCTData@Conclusions <- c(NA,0,1)
                #Conclusions["StillSafe"]=1 indicates the highest doses is still safe
                names(currentCTData@Conclusions) <- c("RP2D","TooToxic","StillSafe")
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
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])
            CurrentDoseLevels <- sapply(1:SampleSize,function(x) 
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@TrtAllos$DoseLevel)
            assign("AssignedDoseLevels",CurrentDoseLevels,pos=1)
            if(class(designSpec)=="CRMExpSpecifier"|class(designSpec)=="CRMLogit1Specifier") 
                assign("AssignedScaledDoses", ScaledDoses[CurrentDoseLevels],pos=1)
            assign("Outcomes",CurrentOutcomes,pos=1)
            assign("EscalationRestriction","FALSE",pos=1)
            RP2DL <- .f2FindDoseLevelSingleStageCRM(lastAssignedDoseLevel=CurrentDoseLevels[SampleSize])
            if (RP2DL==0) currentCTData@Conclusions <- c(NA,NA,NA,1,0)
            else currentCTData@Conclusions <- c(NA,TierDoses[RP2DL],RP2DL,0,0)
            names(currentCTData@Conclusions) <- c("EarlyStop","RP2D","RP2DL","TooToxic","StillSafe")
            rm(list=c(slotNames(designSpec),"Outcomes","DesignSpec","AssignedDoseLevels","NDoseLevels"),pos=1)
            if(class(designSpec)=="CRMExpSpecifier"|class(designSpec)=="CRMLogit1Specifier"){
                rm(list=c("ScaledDoses","AssignedScaledDoses"),pos=1)
                if(class(designSpec)=="CRMLogit1Specifier") rm(list=c("Param1PriorSpec","Param2PriorSpec"),pos=1)
            }
            else rm(list=c("StartLevel","InitialLogORGuesses","Param1PriorSpec","Param2PriorSpec"),pos=1)
            return(list(NewCTData=currentCTData))
        }   
        # When the trial is stopped early
        else if(!is.null(currentCTData@Conclusions["EarlyStop"])){
            currentCTData@Conclusions <- c(currentCTData@Conclusions,NA,NA,1,0)
            names(currentCTData@Conclusions)[2:5] <- c("RP2D","RP2DL","TooToxic","StillSafe")
            rm(list=c(slotNames(designSpec),"Outcomes","AssignedDoseLevels","DesignSpec","NDoseLevels"),pos=1)
            if(class(designSpec)=="CRMExpSpecifier"|class(designSpec)=="CRMLogit1Specifier"){
                rm(list=c("ScaledDoses","AssignedScaledDoses"),pos=1)
                if(class(designSpec)=="CRMLogit1Specifier") rm(list=c("Param1PriorSpec","Param2PriorSpec"),pos=1)
            }
            else rm(list=c("StartLevel","InitialLogORGuesses","Param1PriorSpec","Param2PriorSpec"),pos=1)
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
            currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])
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
# RP2D: Recommended Phase 2 dose 
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
## Class: EvalEarlyStopsSpecifier
# For evaluating the number of patients when a trial is early stopped, and the proportion of early stopped trials
setClass("EvalEarlyStopsSpecifier",contains="EvalSpecifier")
   
## Method: getRequirements
# This method is to get requirements from an "EvalSampleSizeSpecifier" object
setMethod("getRequirements",signature(spec="EvalSampleSizeSpecifier"),
    function(spec){
        return(list(BaseChars="None",Outcomes="None",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="None",
            Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalNToxsSpecifier" object
setMethod("getRequirements",signature(spec="EvalNToxsSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="BinaryToxicity",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="None",
            Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalRP2DSpecifier" object
setMethod("getRequirements",signature(spec="EvalRP2DSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="None",
            Conclusions="RP2D"))
    }
)

# This method is to get requirements from an "EvalProbRP2DAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalProbRP2DAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="Dose",
            Conclusions="RP2D"))
    }
)

# This method is to get requirements from an "EvalNPatsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalNPatsAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",TimesToOutcomes="None",CTTimes="None",PatTimes="None", TrtAllos="Dose",
            Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalPctPatsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalPctPatsAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="Dose",
            Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalNToxsAtEachDoseSpecifier" object
setMethod("getRequirements",signature(spec="EvalNToxsAtEachDoseSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="BinaryToxicity",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="Dose",
            Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalToxRateSpecifier" object
setMethod("getRequirements",signature(spec="EvalToxRateSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="BinaryToxicity",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="None",
            Conclusions="None"))
    }
)

# This method is to get requirements from an "EvalEarlyStopsSpecifier" object
setMethod("getRequirements",signature(spec="EvalEarlyStopsSpecifier"),
    function(spec){
         return(list(BaseChars="None",Outcomes="None",TimesToOutcomes="None",CTTimes="None",PatTimes="None",TrtAllos="None",
            Conclusions="EarlyStop"))
    }
)

## Method: evalDesign
setGeneric("evalDesign", function(evalSpec,simCTsData) standardGeneric("evalDesign"))

setMethod("evalDesign",signature(evalSpec="EvalSampleSizeSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
            SampleSizes <- sapply(simCTsData,function(x) length(x@PatsData))
            Mean <- round(mean(SampleSizes),3)
            names(Mean) <- "Mean"
            SE <- round(sd(SampleSizes)/sqrt(length(SampleSizes)),3)
            names(SE) <- "StandardError"
            EvalOutput <- list(SampleSizes=SampleSizes,Summary=c(Mean,SE))
            return(EvalOutput)
    }
)

# This method evaluates the toxicity outcomes from the first set of concurrent treatments
setMethod("evalDesign",signature(evalSpec="EvalNToxsSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NToxs <- sapply(simCTsData,function(x) sum(sapply(x@PatsData, function(y) 
            y@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])))
        Mean <- round(mean(NToxs),3)
        names(Mean) <- "Mean"
        SE <- round(sd(NToxs)/sqrt(length(NToxs)),3)
        names(SE) <- "StandardError"
        EvalOutput <- list(NToxs=NToxs,Summary=c(Mean,SE))
        return(EvalOutput)
    }
)

# This method evaluates the chosen RP2Ds from clinical trials
setMethod("evalDesign",signature(evalSpec="EvalRP2DSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
            RP2Ds <- sapply(simCTsData,function(x) as.numeric(x@Conclusions["RP2D"]))
            Mean <- round(mean(RP2Ds,na.rm=TRUE),3)
            names(Mean) <- "Mean"
            SE <- round(sd(RP2Ds,na.rm=TRUE)/sqrt(length(na.omit(RP2Ds))),3)
            names(SE) <- "StandardError"
            NNAs <- sum(is.na(RP2Ds))
            names(NNAs) <- "Number of NAs"
            EvalOutput <- list(RP2Ds=RP2Ds,Summary=c(Mean,SE,NNAs))
            return(EvalOutput)
    }
)

# This method evaluates the probability of each tier dose being chosen as RP2D  
# during the first set of concurrent treatments in the trials.
setMethod("evalDesign",signature(evalSpec="EvalProbRP2DAtEachDoseSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NReps <- length(simCTsData)
        # TierDosesC: vector of tier doses where each tier dose is coerced to be a character
        TierDosesC <- as.character(evalSpec@TierDoses)
        RP2DTable <- rep(0,length(TierDosesC))
        names(RP2DTable) <- TierDosesC
        RP2Ds <- sapply(simCTsData,function(x) as.numeric(x@Conclusions["RP2D"]))
        RP2DTable2<-table(RP2Ds)
        RP2DTable[names(RP2DTable2)] <- RP2DTable2
        RP2DTable <- c(RP2DTable,"NA"=sum(is.na(RP2Ds)))
        ProbsRP2D <- round(RP2DTable/NReps,3)
        SEProbsRP2D <- sapply(RP2DTable,function(x){
            if (x!=NReps & x!=0) return(round(sd(c(rep(1,x),rep(0,NReps-x)))/sqrt(NReps),3))
            else return(0)
        })
        names(SEProbsRP2D) <- c(TierDosesC,"NA")
        CI95s<-sapply(RP2DTable,function(x) fBinomPCI(x,NReps))
        return(list(ProbsRP2D=ProbsRP2D,SEProbsRP2D=SEProbsRP2D,CI95s=CI95s))
    }
)

# This method evaluates the number of patients allocated at each tier dose 
# during the first set of concurrent treatments in the trials.
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
        SENPats <- apply(AllNPatsMatrix,2,function(x) round(sd(x)/sqrt(NReps),3))
        return(list(AverageNPats=NPats,SENPats=SENPats, NPatsMatrix=AllNPatsMatrix))
    }
)

# This method evaluates the percentage of patients allocated at each tier dose 
# during the first set of concurrent treatments in the trials.
setMethod("evalDesign",signature(evalSpec="EvalPctPatsAtEachDoseSpecifier",simCTsData="list"),
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
        TotNPats <- sum(NPats)
        # PctPatsLin, which is calculated as Lin does in his "pmtd" program for validation purpose
        # Average total number of patients enrolled in a CT replication
        PctPatsLin <- sapply(1:length(NPats),function(x) round((NPats[x]/TotNPats)*100,3))
        names(PctPatsLin) <- TierDosesC
        PctPatsMatrix <- t(apply(AllNPatsMatrix,1,function(x) round((x/sum(x))*100,3)))
        PctPats <- apply(PctPatsMatrix,2,function(x) round(sum(x)/NReps,3))
        SEPctPats <- apply(PctPatsMatrix,2,function(x) round(sd(x)/sqrt(NReps),3))
        return(list(PctPats=PctPats,SEPctPats=SEPctPats,PctPatsLin=PctPatsLin,PctPatsMatrix=PctPatsMatrix))
    }
)

# This method evaluates the number of toxicities at each tier dose 
# during the first set of concurrent treatments in the trials.
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
        SENToxs <- apply(AllNToxsMatrix,2,function(x) round(sd(x)/sqrt(NReps),3))
        return(list(NToxs=NToxs,SENToxs=SENToxs,NToxsMatrix=AllNToxsMatrix))
    }
)

# This method calculates the overall toxicity rates 
# The output contains 3 components: the first component (ToxRates) is the toxicity rates calculated from all trials;
# the second component (Summary) contains the mean toxicity rates and standard error;
# the third component (AverageToxRateLin) is the average toxicity rate calculated as Lin does for exp. all tox. rate in his "pmtd" program
setMethod("evalDesign",signature(evalSpec="EvalToxRateSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
        NToxs <- sapply(simCTsData,function(x) sum(sapply(x@PatsData, function(y) 
            y@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])))
        SampleSizes <- sapply(simCTsData,function(x) length(x@PatsData))
        ToxRates <- round(NToxs/SampleSizes,3)
        Mean <- round(mean(ToxRates),3)
        names(Mean) <- "Mean"
        SE <- round(sd(ToxRates)/sqrt(length(ToxRates)),3)
        names(SE) <- "StandardError"
        return(list(ToxRates=ToxRates,Summary=c(Mean,SE),AverageToxRateLin=round(sum(NToxs)/sum(SampleSizes),3)))
    }
)

# This method evaluates the number of enrolled paptients when a trial is early stopped and the
# proportion of early stopped trials.
setMethod("evalDesign",signature(evalSpec="EvalEarlyStopsSpecifier",simCTsData="list"),
    function(evalSpec,simCTsData){
            # the number of enrolled paptients when a trial is early stopped
            NPats <- sapply(simCTsData,function(x) as.numeric(x@Conclusions["EarlyStop"]))
            NReps <- length(simCTsData)
            NEarlyStops <- NReps - sum(is.na(NPats))
            PropEarlyStop <- round(NEarlyStops/NReps,3)
            #Element of EarlyStops2 is 1 if early stop and 0 if no early stop
            EarlyStops2 <- sapply(NPats,function(x) ifelse(is.na(x),0,1))
            SEPropEarlyStop <- round(sd(EarlyStops2)/sqrt(NReps),3)
            # 95%CI for the proportion of early stopping
            CI95 <- fBinomPCI(NEarlyStops,NReps)
            return(list(EarlyStop=NPats,PropEarlyStop=PropEarlyStop,SEPropEarlyStop=SEPropEarlyStop,
                CI95PropEarlyStop=CI95))
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

## Method: checkInteroperability
# This method is to check interoperability among designs, population models,outcome models and evaluation criteria
setGeneric("checkInteroperability",function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs) standardGeneric("checkInteroperability"))

setMethod("checkInteroperability",signature(designSpecs="list",popModelSpecs="list",outcomeModelSpecs="list",
    evalSpecs="list"),function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs){
        NDesigns <- length(designSpecs)
        NPopModels <- length(popModelSpecs)
        NOutcomeModels <- length(outcomeModelSpecs)
        NEvalCriteria <- length(evalSpecs)
        # Check interoperability
        InteroperableDesignPopM <- NULL
        InteroperableDesignOutcomeM <- NULL
        InteroperableDesignEval <- NULL
        InteroperablePopMOutcomeM <- NULL
        InteroperablePopMEval <- NULL
        InteroperableOutcomeMEval <- NULL
        for (DesignIndex in 1:NDesigns){
            DesignSpec <- designSpecs[[DesignIndex]]
            for (PopMIndex in 1:NPopModels){
                PopModelSpec <- popModelSpecs[[PopMIndex]]
                if(any(is.na(match(getRequirements(DesignSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars))))
                    cat("The population model ",PopMIndex," cannot provide the baseline characteristics the design ", 
                        DesignIndex," requires!","\n","\n")
                else InteroperableDesignPopM <- rbind(InteroperableDesignPopM,c(DesignIndex,PopMIndex))
                for (OutcomeMIndex in 1:NOutcomeModels){
                    OutcomeModelSpec <- outcomeModelSpecs[[OutcomeMIndex]]
                    if (any(
                        is.na(match(getRequirements(OutcomeModelSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos)),
                        is.na(match(getRequirements(DesignSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes)),
                        is.na(match(getRequirements(DesignSpec)$TimesToOutcomes, getProvisions(OutcomeModelSpec)$TimesToOutcomes)))){
                        if(any(is.na(match(getRequirements(OutcomeModelSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos))))
                            cat("The design ",DesignIndex," cannot provide the treatment allocations the outcome model ", 
                                OutcomeMIndex," requires!","\n","\n")
                        if(any(is.na(match(getRequirements(DesignSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes))))
                            cat("The outcome model ",OutcomeMIndex," cannot provide the outcomes the design ", 
                                DesignIndex," requires!","\n","\n")
                        if(any(is.na(match(getRequirements(DesignSpec)$TimesToOutcomes, getProvisions(OutcomeModelSpec)$TimesToOutcomes))))
                            cat("The outcome model ",OutcomeMIndex," cannot provide the times to outcomes the design ", 
                                DesignIndex," requires!","\n","\n")
                    }
                    else InteroperableDesignOutcomeM <- rbind(InteroperableDesignOutcomeM,c(DesignIndex, OutcomeMIndex))
                    if (DesignIndex==1){
                        if(any(is.na(match(getRequirements(OutcomeModelSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars))))
                            cat("The population model ",PopMIndex," cannot provide the baseline characteristics the outcome model ", 
                                OutcomeMIndex," requires!","\n","\n")
                        else InteroperablePopMOutcomeM <- rbind(InteroperablePopMOutcomeM,c(PopMIndex, OutcomeMIndex))
                    }
                    for (EvalIndex in 1:NEvalCriteria){
                        EvalSpec <- evalSpecs[[EvalIndex]]
                        if(PopMIndex==1 & OutcomeMIndex==1){
                            if(any(
                                is.na(match(getRequirements(EvalSpec)$CTTimes, getProvisions(DesignSpec)$CTTimes)),
                                is.na(match(getRequirements(EvalSpec)$PatTimes, getProvisions(DesignSpec)$CTTimes)),
                                is.na(match(getRequirements(EvalSpec)$Conclusions, getProvisions(DesignSpec)$Conclusions)),
                                is.na(match(getRequirements(EvalSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos)))){
                                if(any(is.na(match(getRequirements(EvalSpec)$CTTimes, getProvisions(DesignSpec)$CTTimes))))
                                    cat("The design ",DesignIndex," cannot provide the CT level event times the criterion ", 
                                        EvalIndex," requires!","\n","\n") 
                                if(any(is.na(match(getRequirements(EvalSpec)$PatTimes, getProvisions(DesignSpec)$CTTimes))))
                                    cat("The design ",DesignIndex," cannot provide the patient level event times the criterion ", 
                                        EvalIndex," requires!","\n","\n") 
                                if(any(is.na(match(getRequirements(EvalSpec)$Conclusions, getProvisions(DesignSpec)$Conclusions))))
                                    cat("The design ",DesignIndex," cannot provide the conclusions the criterion ", 
                                        EvalIndex," requires!","\n","\n")   
                                if(any(is.na(match(getRequirements(EvalSpec)$TrtAllos, getProvisions(DesignSpec)$TrtAllos))))
                                    cat("The design ",DesignIndex," cannot provide the treatment allocations the criterion ", 
                                        EvalIndex," requires!","\n","\n")
                            }
                            else  InteroperableDesignEval <- rbind(InteroperableDesignEval,c(DesignIndex, EvalIndex))
                        }
                        if(DesignIndex==1 & OutcomeMIndex==1){
                            if(any(is.na(match(getRequirements(EvalSpec)$BaseChars, getProvisions(PopModelSpec)$BaseChars))))
                                cat("The population model ",PopMIndex," cannot provide the baseline characteristics the criterion ", 
                                    EvalIndex," requires!","\n","\n") 
                            else InteroperablePopMEval <- rbind(InteroperablePopMEval,c(PopMIndex, EvalIndex))
                        }
                        if(DesignIndex==1 & PopMIndex==1){
                            if(any(
                                is.na(match(getRequirements(EvalSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes)),
                                is.na(match(getRequirements(EvalSpec)$TimesToOutcomes, getProvisions(OutcomeModelSpec)$TimesToOutcomes)))){
                                if(any(is.na(match(getRequirements(EvalSpec)$Outcomes, getProvisions(OutcomeModelSpec)$Outcomes))))
                                    cat("The outcome model ",OutcomeMIndex," cannot provide the outcomes the criterion ", 
                                        EvalIndex," requires!","\n","\n") 
                                if(any(is.na(match(getRequirements(EvalSpec)$TimesToOutcomes, getProvisions(OutcomeModelSpec)$TimesToOutcomes))))
                                    cat("The outcome model ",OutcomeMIndex," cannot provide the times to outcomes the criterion ", 
                                        EvalIndex," requires!","\n","\n") 
                            }
                            else InteroperableOutcomeMEval <- rbind(InteroperableOutcomeMEval,c(OutcomeMIndex, EvalIndex))
                        }
                    }
                }
            }
        }
        # Obtain the interoperable sets of design, population model, outcome model and evaluation criterion
        InteroperableSets <- NULL
        if (!any(is.null(InteroperableDesignPopM),is.null(InteroperableDesignOutcomeM),is.null(InteroperableDesignEval),
            is.null(InteroperablePopMOutcomeM),is.null(InteroperablePopMEval),is.null(InteroperableOutcomeMEval))){
            DPIndex <- 1
            while ( DPIndex <= nrow(InteroperableDesignPopM)){
                DesignIndex <- InteroperableDesignPopM[DPIndex,1]
                PopMIndices <- InteroperableDesignPopM[InteroperableDesignPopM[,1]==DesignIndex,2]
                DPIndex <- DPIndex + length(PopMIndices)
                for(PopMIndex in PopMIndices){
                    OutcomeMIndices <- intersect(InteroperableDesignOutcomeM[InteroperableDesignOutcomeM[,1]==DesignIndex,2],
                        InteroperablePopMOutcomeM[InteroperablePopMOutcomeM[,1]==PopMIndex,2])
                    EvalIndices1 <- intersect(InteroperableDesignEval[InteroperableDesignEval[,1]==DesignIndex,2],
                                InteroperablePopMEval[InteroperablePopMEval[,1]==PopMIndex,2])
                    if(length(OutcomeMIndices)==0|length(EvalIndices1)==0) next
                    else{ 
                        for( OutcomeMIndex in OutcomeMIndices){
                            EvalIndices <- intersect(EvalIndices1,
                                InteroperableOutcomeMEval[InteroperableOutcomeMEval[,1]==OutcomeMIndex,2])
                            if(length(EvalIndices)==0) next
                            else{
                                for(EvalIndex in EvalIndices) 
                                    InteroperableSets <- rbind(InteroperableSets,c(DesignIndex,PopMIndex,OutcomeMIndex,EvalIndex))
                            }
                        }
                    }
                }
            }
        }
        if(!is.null(InteroperableSets))  colnames(InteroperableSets) <- c("Design","PopM","OutcomeM","Criterion") 
        return(InteroperableSets)
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
        InteroperableSets <- checkInteroperability(designSpecs=designSpecs,popModelSpecs=popModelSpecs,outcomeModelSpecs=outcomeModelSpecs,
            evalSpecs=evalSpecs)
        if (is.null(InteroperableSets)) stop("No sets of designs, population models, outcome models and evaluation criteria can
            work together!")
        else{
            # Check user to see if he/she wants to proceed
            NInteroperableSets <- nrow(InteroperableSets)
            # Initialize "Scenarios", each scenario corresponds to a combination of a design, a population model and an 
            # outcome model
            Scenarios <- rep(1,NInteroperableSets)
            for ( i in 2:NInteroperableSets){
                if(any(InteroperableSets[i-1,1:3]!=InteroperableSets[i,1:3])) Scenarios[i] <- Scenarios[i-1]+1
                else Scenarios[i] <- Scenarios[i-1]
            }
            cat("Scenarios:","\n")
            print(cbind(Scenarios,InteroperableSets))
            answer <- "Yes"
            if(userInput) answer <- readline("Proceed? (Yes or No)")
            if(answer=="No") print("Experiment is cancelled!")
            else if (answer=="Yes"){
                # Initialize the outputs from evaluation
                NScenarios <- length(unique(Scenarios))
                EvalOutputs <- vector("list",NScenarios)
                names(EvalOutputs) <- sapply(1:NScenarios,function(x) paste("Scenario",x,sep=""))
                for(ScenarioIndex in 1:NScenarios){
                    DesignSpec <- designSpecs[[(InteroperableSets[Scenarios==ScenarioIndex,"Design"][1])]]
                    PopModelSpec <- popModelSpecs[[(InteroperableSets[Scenarios==ScenarioIndex,"PopM"][1])]]
                    OutcomeModelSpec <- outcomeModelSpecs[[(InteroperableSets[Scenarios==ScenarioIndex,"OutcomeM"][1])]]
                    EvalSpecs <- evalSpecs[(InteroperableSets[Scenarios==ScenarioIndex,"Criterion"])]
                     # Initialize the TempData
                    assign("TempData",initializeTempData(designSpec=DesignSpec),pos=1)
                    SimCTsData <- sapply(1:nReps,function(x) sim1CT(designSpec=DesignSpec,popModelSpec=PopModelSpec,
                        outcomeModelSpec=OutcomeModelSpec))
                    if(!is.null(simDataDir)) {
                        save(SimCTsData,file=paste(simDataDir,"SimCTsData_S",ScenarioIndex,".RData",sep=""))
                        save(TempData,file=paste(simDataDir,"TempData_S",ScenarioIndex,".RData",sep=""))
                    }
                    EvalOutputs[[ScenarioIndex]] <- lapply(EvalSpecs,function(x) evalDesign(evalSpec=x,simCTsData=SimCTsData))
                }
                rm(list="TempData",pos=1)
                return(EvalOutputs)
            }
            else stop("Your input is wrong! It should be either Yes or No.")
        }
    }
)
        
## Function: specifyObject
# className is a character 
# "slots" is a named list with the names corresponding to the slot names
specifyObject <- function(className,slots){
    Object <- new(className)
    for ( SlotName in names(slots))
        slot(Object,SlotName) <- slots[[SlotName]]
    return(Object)
} 
             
             
    
