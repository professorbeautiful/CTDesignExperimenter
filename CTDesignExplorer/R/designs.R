cat("======== designs.R  ================\n")

### Classes and Methods for design

#' A DesignSpecifier must do the following:
#' (1, fixed)  generatePatientsForAccrual
#'    Recruit patients. All at once? By cohort? At staggered/random times?
#' For each patient:
#'  (1) populationModel:  generate patient features/characteristics.
#'  (0+) (optionally) checkEligibility
#'      provides eligibilityStatus. default=TRUE
#'  (1, fixed) enrollPatient
#'  (1) Initiate treatment plan;  optionally, stratified on popModel.
#'  (1) Initiate generation of outcomes.
#'  (0+) Read interim outcomes, activate outcome-dependent treatment modifications, off-study etc.
#'  (1+) stoppingRule, a bag of actionGenerators
#'      Check stopping rule(s). Halt trial.
#'  (1, fixed) Transfer outcomes and other data to the CTdata object.
#' (1+) Generate conclusions of the trial
#' (1+) Generate summaries of the trial


setClass("DesignComponent", contains="VariableNetwork",
         slots=list(actions="ActionList"))
setClass("EligibilityRule", contains="DesignComponent")
EligibilityRule = function(eligRuleFunction) {
  eligRule = new("EligibilityRule")
  eligRule@vgList = VariableGeneratorList(
    new("VariableGenerator", ) 
  eligRule
}
setClass("DesignSpecifier", contains="DesignComponent",
         slots=list(actions="ActionList"))

###  Should be a bag of ActionGenerator objects.

placeInitialActions = function(design) {
  
}

## Method: generateInitialActions
## Method for generating a list of initial actions for an action queue
setGeneric("generateInitialActions", function(designSpec) standardGeneric("generateInitialActions"))


# This method returns a list of initial actions in the order of the time

## Method: allocateTrts
setGeneric("allocateTrts",function(designSpec,currentCTData,currentGlobalTime,patsIndices) standardGeneric("allocateTrts"))


## Method: checkStoppingRule
# It can return a new CT data or a list of new actions
setGeneric("checkStoppingRule",function(designSpec,currentCTData,currentGlobalTime) standardGeneric("checkStoppingRule"))


## Method: checkSwitchingStageRule
# It may return a new CT data
setGeneric("checkSwitchingStageRule",function(designSpec,currentCTData,currentGlobalTime) standardGeneric("checkSwitchingStageRule"))

