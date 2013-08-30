cat("======== createCriterionCatalog.R  ================\n")



evalPctPatsAtEachDose = new("EvalPctPatsAtEachDoseSpecifier")

evalNToxsAtEachDose = new("EvalNToxsAtEachDoseSpecifier")


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