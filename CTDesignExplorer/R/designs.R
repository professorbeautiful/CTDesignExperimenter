cat("======== designs.R  ================\n")

### Classes and Methods for design

setClass("DesignSpecifier", contains="VariableNetwork")




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

