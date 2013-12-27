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
setClass("Insert", contains="Specifier")
setClass("InsertVariable", contains=c("Insert", "VariableGenerator"))
setClass("InsertEvent", contains=c("Insert", "EventGenerator"))


setClass("PatientAttribute", contains="InsertVariable")
setClass("EligibilityCriterion", contains="InsertVariable")
setClass("ScheduleTreatment", contains="InsertEvent")
setClass("OffStudyCriterion", contains="InsertVariable")
setClass("ModificationRule", contains="InsertEvent")
setClass("PatientSummary", contains="InsertVariable")
setClass("StoppingCriterion", contains="InsertEvent")
setClass("TrialSummary", contains="InsertVariable")

#' ListOfInserts
#' 
#' A list of Insert objects.
#' Generally the parameters are the defaults.
#' When a new Scenario is defined, the parameters may be changed.
setClass("ListOfInserts", contains="list",
         validity=function(object) {
           if(length(ob)==0) return(TRUE)
           whichAreInserts = sapply(object, is, "Insert")
           if(all(whichAreInserts)) return(TRUE)
           return(paste("ERROR in ListOfInserts: ", which(!whichAreInserts)))
})



#' Scenario
#' 
#' Contains all the user-chosen Insert objects, with parameter assignments.
#' 
#' An evaluation will consist of a list of scenarios, together with evaluation criteria.
#'  The scenarioEnv holds inserts (with parameter values assigned), 
#'  variablevalues, and patient data.
setClass("Scenario", contains=ListOfInserts,
                    scenarioEnv="environment"))
#####


### sim1CT  #####

runScenario = function(scenario){
  sim1CT(scenario@popModel, scenario@design, scenario@outcomeModel)
}
