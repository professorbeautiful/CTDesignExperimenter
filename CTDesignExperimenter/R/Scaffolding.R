cat("======== Scaffolding.R  ================\n")

setClass("ScaffoldEvent", 
         slots=list(name="character",
                    predecessor="character",  #ScaffoldEvent
                    successor="character",  #ScaffoldEvent
                    eventInsertType="character",
                    eventInsertSubType="character",
                    timeToNextEvent="numeric"))
scaffoldObjectNames =
    cq(BeginClinicalTrial, GeneratePatient, CheckEligibility, 
       EnrollPatient, AssignTreatmentPlan, GenerateOutcomes,
       CheckOffStudy, CheckModifications, 
       SummarizePatient, CheckStoppingRules, SummarizeTrial)
scaffoldInsertSubTypes = cq(
  ,
  patientAttribute,
  eligibilityCriterion,
  ,
  scheduleTreatment,
  patientAttribute,
  offStudyCriterion,
  modificationRule,
  patientSummary,
  stoppingCriterion,
  
)
scaffoldObjects = data.frame(stringsAsFactors=FALSE,
  row.names=scaffoldObjectNames,
  name=scaffoldObjectNames,
  insertSubType=scaffoldInsertSubTypes
)
scaffoldObjects$insertType = 
  ifelse(scaffoldObjects$insertSubType %in% cq(scheduleTreatment, modificationRule),
         "Event", "Variable")
scaffoldObjects$insertType[scaffoldObjects$insertSubType==""] = ""
scaffoldObjects$revertIf = "FALSE"
scaffoldObjects$revertTo = ""
scaffoldObjects["CheckEligibility", "revertTo"] = "GeneratePatient"
scaffoldObjects["CheckEligibility", "revertIf"] = "inEligible"
scaffoldObjects["CheckOffStudy", "revertTo"] = "AssignTreatmentPlan"
scaffoldObjects["CheckOffStudy", "revertIf"] = "notOffStudy"
scaffoldObjects["CheckOffStudy", "revertTo"] = "AssignTreatmentPlan"
scaffoldObjects["CheckOffStudy", "revertIf"] = "notOffStudy"


scafSize = nrow(scaffoldObjects)

increment = function(n) n+1

for(scaf in 1:scafSize) {
  with(scaffoldObjects[scaf, ], 
    assign(print(name), new("ScaffoldEvent",
                     name=name,
                    predecessor=ifelse(scaf>1, scaffoldObjects$name[scaf-1], ""),
                     successor=ifelse(scaf<scafSize, scaffoldObjects$name[scaf+1], ""),
                     eventInsertType=scaffoldObjects$insertType[scaf],
                     eventInsertSubType=scaffoldObjects$insertSubType[scaf],
                     timeToNextEvent=increment
    ), pos=1))
}

setClass("UserSpec", contains="VariableNetwork")
         
setClass("EligibilityCriterion", contains="UserSpec",
  validity=function(object) {
    provision = slot(object, name="provisions")
    if(!is(provision, "Variable"))
      return("eligibilityCriterion provision is not a Variable")
     if(identical(slot(object, provision)@checkDataType, is.logical))
       return("checkDataType should be is.logical")
    return(TRUE)
  }
)

### Context = Evaluation or scenario or CT or patient.
### Thus, hierarchical, with 4 levels.

setGeneric("doEvent", function(event, ...) standardGeneric("doEvent"))


defaultScenario = new("Scenario", )

setMethod("doEvent", signature=list("ScaffoldEvent", "Scenario"),
          function(event, scenario=defaultScenario, ...) {
            cat("Event ", event@name , "is happening\n")
            if(event@name == "BeginClinicalTrial") {
              cat("Must now initializeCTdata()\n")
              assign("CTdata", new.env(), pos=1)
            }
            if(event@name == "GeneratePatient") {
              cat("Must generate a patient now;  gather VG's.\n")
            }
            if(event@name == "GeneratePatient") {
              cat("Must generate a patient now;  gather VG's.\n")
            }
            if(event@insertSubType != "") {
              cat("Must generate a patient\n")
            }
            if(event@name == "BeginClinicalTrial") {
              cat("Must generate a patient\n")
            }
            if(event@successor != "")
              doEvent(get(event@successor))
          }
)
doEvent(BeginClinicalTrial)


