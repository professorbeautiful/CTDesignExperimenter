cat("======== Scaffolding.R  ================\n")

setClass("ScaffoldEvent", 
         slots=list(name="character",
                    predecessor="character",  #ScaffoldEvent
                    successor="character",  #ScaffoldEvent
                    eventInsertType="character",
                    eventInsertSubType="character",
                    timeToNextEvent="function"))
scaffoldObjectNames =
    cq(BeginSimulation, BeginClinicalTrial, GeneratePatient, CheckEligibility, 
       EnrollPatient, AssignTreatmentPlan, GenerateOutcomes,
       CheckOffStudy, CheckModifications, 
       SummarizePatient, CheckStoppingRules, SummarizeTrial,
       SummarizeSimulation)
scaffoldInsertSubTypes = cq(
  DesignParameter,
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
  TrialSummary,
  SimulationSummary  ## Same as evaluation criterion?
)
scaffoldObjects = data.frame(stringsAsFactors=FALSE,
  row.names=scaffoldObjectNames,
  name=scaffoldObjectNames,
  eventInsertSubType=scaffoldInsertSubTypes
)
scaffoldObjects$eventInsertType = 
  ifelse(scaffoldObjects$eventInsertSubType %in% cq(ScheduleTreatment, ModificationRule),
         "Event", "Variable")
scaffoldObjects$eventInsertType[scaffoldObjects$eventInsertSubType==""] = ""
scaffoldObjects$loopBackIf = "FALSE"
scaffoldObjects$loopBackTo = ""
scaffoldObjects["CheckEligibility", "loopBackTo"] = "GeneratePatient"
scaffoldObjects["CheckEligibility", "loopBackIf"] = "notEligible"
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
                     eventInsertType=scaffoldObjects$eventInsertType[scaf],
                     eventInsertSubType=scaffoldObjects$eventInsertSubType[scaf],
                     timeToNextEvent=increment
    ), pos=1))
}


### Context = Evaluation or scenario or CT or patient.
### Thus, hierarchical, with 4 levels.

setGeneric("doEvent", function(event, scenario, ...) standardGeneric("doEvent"))

defaultScenario =  
                      new("ListOfInserts", list(
                        vg_liver, vg_age, ec_liver, ec_age))
getVGs = function(scenario, subType) {
   whichOnes = sapply(scenario, is, subType)
   return(scenario[whichOnes])
}
length(getVGs(scenario=defaultScenario, subType="PatientAttribute"))
length(getVGs(scenario=defaultScenario, subType="EligibilityCriterion"))
# require(R.oo)
# setConstructorS3("TrialData", 
#                  function(patientData=new.env(), NpatientsEnrolled=0) 
#                    extend(Object(), "TrialData", patientData=patientData,
#                           NpatientsEnrolled=NpatientsEnrolled)
#                    )
# trialData = TrialData()
# ll(trialData)
# trialData$NpatientsEnrolled
# trialData$patientData
# temp=new.env()
# temp$x=1
# temp$x = temp$x+1
# temp$x
# trialData$newvar = 4
###  R.oo doesn't buy us anything. And we lose tab completion.
###  We can use eval for complicated things.

increment = function(X, ENV=parent.env(), delta=1, verbose=F) {
  nameX = as.character(substitute(X))
  if(verbose) cat("nameX", str(nameX), "\n")
  assign(nameX, env=ENV, delta + get(nameX, ENV))
  if(verbose) cat("new value", get(nameX, ENV), "\n")
  invisible(NULL)
}


initializeQueue = function(firstaction=BeginClinicalTrial) {
  assign("actionQueue", new.env(), pos=1)
  assign("queueTimes", 0, env=actionQueue)
  assign("actions", list(firstaction), env=actionQueue)
}

addToQueue = function(event, time) {
  environment(addToQueue_) = actionQueue
  addToQueue_(event, time)
  cat("actionQueue$queueTimes=", actionQueue$queueTimes, "\n")
}  
addToQueue_ = function(event, time) {
  temp = ls()  ##OK
  print(temp)
  sortposition = length(queueTimes) + 1
  if(sortposition == 1) {
    queueTimes <<- 0
    actions[[1]] <<- event
  }
  else {
    while(time < queueTimes[sortposition-1]) 
      sortposition = sortposition - 1  ## should never be < 1 though.
    cat("sortposition=", sortposition, "\n")
    if(sortposition==1) {
      actions <<- c(event, actions)
      queueTimes <<- c(time, queueTimes)
    }
    else if (sortposition==(length(actions)+1)) {
      actions <<- c(actions, event)
      queueTimes <<- c(queueTimes, time)
    }  
    else {
      actions <<- c(actions[1:(sortposition-1)],
                event, actions[sortposition:length(actions)])
      queueTimes <<- c(queueTimes[1:(sortposition-1)],
                       time, queueTimes[sortposition:length(queueTimes)])
    } 
  }
  cat("length of actions is now ", length(actions), "\n")
  cat("actions saved correctly? ", identical(actions, actionQueue$actions), "\n")
}

debug(addToQueue_)
initializeQueue(firstaction="first")
actionQueue$actions
addToQueue("two", 2)
actionQueue$actions
addToQueue("three", 3)
actionQueue$actions
addToQueue("1.5", 1.5)
actionQueue$actions



setMethod("doEvent", signature=list("ScaffoldEvent", "Scenario"),
          function(event, scenario=defaultScenario, ...) {
            cat("Event ", event@name , "is happening\n")
            if(event@name == "BeginClinicalTrial") {
              cat("Must now initializeCTdata()\n")
              assign("trialData", new.env(), pos=1) #where is best?
              trialData$NpatientsEnrolled = 0
              trialData$patientData = list() ## This will hold enrolled patients.
            }
            if(event@name == "GeneratePatient") {
              cat("Must generate a candidate patient now;  
                  gather VG's and evaluate.\n")
              assign("candidatePatient", new.env(), env=trialData)
              candidateVN = VariableNetwork(vgList=getVGs(scenario, 
                                                          "PatientAttribute"))
            }
            if(event@name == "CheckEligibility") {
              cat("Gather eligibilityCriterion objects from scenario.",
               "Form a Variable Network. \n",
                  "Retrieve all VariableValues,
                  and return the conjunction with all().\n")
            }
            if(event@name == "EnrollPatient") {
              cat("Enrolling the patient; copy patient info.\n")
              increment(NpatientsEnrolled, trialData)
              trialData$patientData[trialData$NpatientsEnrolled[]] = candidateVN
              theCurrentPatient = trialData$patientData[trialData$NpatientsEnrolled[]] 
            }
            if(event@eventInsertSubType != "") {
              cat("Must generate a patient\n")
            }
            if(event@name == "BeginClinicalTrial") {
              cat("Must generate a patient\n")
            }
            if(event@successor != "")
              doEvent(get(event@successor), scenario)
          }
)

runTrial = function(scenario) {
  initializeQueue()
  doEvent(actionQueue$actions[[1]], defaultScenario)
}


