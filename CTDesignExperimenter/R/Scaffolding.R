cat("======== Scaffolding.R  ================\n")

setClass("ScaffoldEvent", 
         slots=list(name="character",
                    predecessor="character",  #ScaffoldEvent
                    successor="character",  #ScaffoldEvent
                    eventInsertType="character",
                    eventInsertSubType="character",
                    timeToNextEvent="function"))
scaffoldObjectNames =
    cq(BeginClinicalTrial, GeneratePatient, CheckEligibility, 
       EnrollPatient, AssignTreatmentPlan, GenerateOutcomes,
       CheckOffStudy, CheckModifications, 
       SummarizePatient, CheckStoppingRules, SummarizeTrial)
scaffoldInsertSubTypes = cq(
  ,
  PatientAttribute,
  EligibilityCriterion,
  ,
  ScheduleTreatment,
  PatientAttribute,
  OffStudyCriterion,
  ModificationRule,
  PatientSummary,
  StoppingCriterion,
  
)
scaffoldObjects = data.frame(stringsAsFactors=FALSE,
  row.names=scaffoldObjectNames,
  name=scaffoldObjectNames,
  insertSubType=scaffoldInsertSubTypes
)
scaffoldObjects$insertType = 
  ifelse(scaffoldObjects$insertSubType %in% cq(ScheduleTreatment, ModificationRule),
         "Event", "Variable")
scaffoldObjects$insertType[scaffoldObjects$insertSubType==""] = ""
scaffoldObjects$loopBackIf = "FALSE"
scaffoldObjects$loopBackTo = ""
scaffoldObjects["CheckEligibility", "loopBackTo"] = "GeneratePatient"
scaffoldObjects["CheckEligibility", "loopBackIf"] = "inEligible"
scaffoldObjects["CheckOffStudy", "loopBackTo"] = "AssignTreatmentPlan"
scaffoldObjects["CheckOffStudy", "loopBackIf"] = "notOffStudy"
scaffoldObjects["CheckOffStudy", "loopBackTo"] = "AssignTreatmentPlan"
scaffoldObjects["CheckOffStudy", "loopBackIf"] = "notOffStudy"


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


### Context = Evaluation or scenario or CT or patient.
### Thus, hierarchical, with 4 levels.

setGeneric("doEvent", function(event, ...) standardGeneric("doEvent"))


defaultScenario = new("Scenario", 
                      new("ListOfInserts"))

setMethod("doEvent", signature=list("ScaffoldEvent", "Scenario"),
          function(event, scenario=defaultScenario, ...) {
            cat("Event ", event@name , "is happening\n")
            if(event@name == "BeginClinicalTrial") {
              cat("Must now initializeCTdata()\n")
              assign("CTdata", new.env(), pos=1)
            }
            if(event@name == "GeneratePatient") {
              cat("Must generate a patient now;  
                  gather VG's and evaluate.\n")
            }
            if(event@name == "CheckEligibility") {
              cat("Gather eligibilityCriterion objects from scenario.",
               "Form a Variable Network. \n",
                  "Retrieve all VariableValues,
                  and return the conjunction with all().\n")
            }
            if(event@name == "EnrollPatient") {
              cat("Enrolling the patient; copy patient info.\n")
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


