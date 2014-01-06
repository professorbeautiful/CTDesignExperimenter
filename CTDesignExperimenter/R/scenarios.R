cat("======== scenarios.R  ================\n")
#####
#' A Scenario contains any optional scenario inserts chosen by the user:
#' PatientAttribute
#' EligibilityCriterion 
#' ScheduleTreatment 
#' PatientAttribute 
#' OffStudyCriterion 
#' ModificationRule 
#' PatientSummary 
#' StoppingCriterion 
#' TrialSummary 
#' Most of these just create variable values,
#' but ScheduleTreatment objects generate "ApplyTreatment"  events,
#'     ModificationRule  objects generate "ModifyTreatment" events,
#'     StoppingCriterion objects generate "StopTrial"       events,
#' to be placed on the queue.
#' 

#####


#' Scenario
#' 
#' Contains all the user-chosen Insert objects, with parameter assignments.
#' 
#' An evaluation will consist of a list of scenarios, together with evaluation criteria.
#'  The scenarioEnv holds inserts (with parameter values assigned), 
#'  variablevalues, and patient data. #####
setClass("Scenario", contains="ListOfInserts")
#####
##' extractParameters #####
##' We will need this to construct interfaces.
extractParameters = function(obj){
  if(is(obj, "list"))
    lapply(obj, slot, name="parameters")
  else
    slot(obj, "parameters" )
}

# TODO
#setGeneric("extractParameters", extractParameters)

#extractParameters(defaultScenario)
# 
# setClass("CTSimulation", slots=list(
#     scenario="Scenario",
#     scenarioEnv="environment"))

