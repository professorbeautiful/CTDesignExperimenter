cat("======== Scaffolding.R  ================\n")

library(pryr)  ## Hadley Wickham utilities for S4
library(RBioinf);  help(p="RBioinf")

### First, some utility functions #####
className = function(obj) {
  if(isClass(obj)) return(obj@className)
  # is(class2="Class", obj)  returns FALSE !!
  return(class(obj))
}
increment = function(X, ENV=parent.frame(), delta=1, verbose=F) {
  nameX = as.character(substitute(X))
  if(verbose) cat("nameX", str(nameX), "\n")
  assign(nameX, env=ENV, delta + get(nameX, ENV))
  if(verbose) cat("new value", get(nameX, ENV), "\n")
  invisible(NULL)
}

now = function() {
  actionQueue$queueTimes[actionQueue$queuePointer] 
}

##' incrementNow  ####
##' 
##' From relative time to absolute time.
##' 
incrementNow = function(delta=1, ENV=actionQueue) {
  now() + delta
}


##' ScaffoldEvent ####
setClass("ScaffoldEvent", contains="Event",
         slots=list(name="character",
                    predecessor="character",  #ScaffoldEvent
                    successor="character",  #ScaffoldEvent
                    eventInsertType="character",
                    eventInsertSubType="character",
                    jumpIf="character", # boolean; condition for alternative
                    jumpTo="character", # alternative destination
                    timeToNextEvent="function"))

##' scaffoldObjectNames ####
##' 
scaffoldObjectNames = 
    cq(BeginSimulation, BeginClinicalTrial, GeneratePatient, CheckEligibility, 
       EnrollPatient, AssignTreatmentPlan, GenerateOutcomes,
       CheckOffStudy, CheckModifications, 
       SummarizePatient, CheckStoppingRules, SummarizeTrial,
       SummarizeSimulation)

scaffoldObjects = data.frame(stringsAsFactors=FALSE,
  row.names=scaffoldObjectNames,
  name=scaffoldObjectNames,
  eventInsertSubType=scaffoldInsertSubTypes
)
scaffoldObjects$eventInsertType = 
  ifelse(scaffoldObjects$eventInsertSubType %in% cq(ScheduleTreatment, ModificationRule),
         "Event", "Variable")
scaffoldObjects$eventInsertType[scaffoldObjects$eventInsertSubType==""] = ""
scaffoldObjects$jumpIf = "FALSE"
scaffoldObjects$jumpTo = ""
scaffoldObjects["CheckEligibility", "jumpTo"] = "GeneratePatient"
scaffoldObjects["CheckEligibility", "jumpIf"] = "trialData$candidatePatient$VVenv$notEligible"
scaffoldObjects["CheckOffStudy", "jumpTo"] = "AssignTreatmentPlan"
scaffoldObjects["CheckOffStudy", "jumpIf"] = "notOffStudy"
scaffoldObjects["CheckOffStudy", "jumpTo"] = "AssignTreatmentPlan"
scaffoldObjects["CheckOffStudy", "jumpIf"] = "notOffStudy"


scafSize = nrow(scaffoldObjects)

### Create the scaffold objects #####
makeScaffoldObjects <- function () {
  for(scaf in 1:scafSize) {
    with(scaffoldObjects[scaf, ], 
         assign(
           #assignInMyNamespace( # fails: Error in bindingIsLocked(x, ns)
             pos=1, #works for BUILD, but then objects are lost.
        print(name), new("ScaffoldEvent",
                       name=name,
                       predecessor=ifelse(scaf>1, scaffoldObjects$name[scaf-1], ""),
                       successor=ifelse(scaf<scafSize, scaffoldObjects$name[scaf+1], ""),
                       eventInsertType=scaffoldObjects$eventInsertType[scaf],
                       eventInsertSubType=scaffoldObjects$eventInsertSubType[scaf],
                       jumpIf=scaffoldObjects$jumpIf[scaf],
                       jumpTo=scaffoldObjects$jumpTo[scaf],
                       timeToNextEvent=increment
      )))
  }
}

makeScaffoldObjects() # For building the package.

### Context = Evaluation or scenario or CT or patient.#####
### Thus, hierarchical, with 4 levels.

## Creating a default scenario #####

defaultScenario =  #as("Scenario",
  new("ListOfInserts", list(
    vg_liver=vg_liver, vg_age=vg_age, 
    ec_liver=ec_liver, ec_age=ec_age,
    vg_clearanceRate=vg_clearanceRate, 
    vg_responseDoseThreshold=vg_responseDoseThreshold,
    vg_toxDoseThreshold=vg_toxDoseThreshold)
  )# )

getVGs = function(scenario, subType) {
  whichOnes = (sapply(scenario, slot, name="insertSubType") == subType)
  return(scenario[whichOnes])
}
length(getVGs(scenario=defaultScenario, subType="PatientAttribute"))
length(getVGs(scenario=defaultScenario, subType="EligibilityCriterion"))

#### doAction methods and functions #####
setGeneric("doAction", function(event, scenario, ...) standardGeneric("doAction"))

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

##' the action queue #####
##' 
initializeQueue = function(firstaction=BeginClinicalTrial) {
  assign("actionQueue", new.env(), pos=1)
  assign("queuePointer", 1, env=actionQueue)
  assign("queueTimes", 0, env=actionQueue)
  assign("actions", list(firstaction), env=actionQueue)
  ### With this design, the times and the actions are vectors
  ### of the same length. The event time is not packaged with the event.
  ### So we are not currently using EventAtTime.
}
# executeQueue = function(){
#   executeQueue_()
# }

executeQueue = function(verbose=TRUE, scenario=defaultScenario){
  nActions <<- length(actionQueue$actions)
  if(verbose) cat("executeQueue_: nActions = ", nActions, "\n")
  while(actionQueue$queuePointer <= nActions) {
    doAction(actionQueue$actions[[actionQueue$queuePointer]], scenario)
    increment(ENV=actionQueue, queuePointer)
    nActions <<-length(actionQueue$actions)
    if(verbose) cat("executeQueue_: nActions = ", nActions,
                    "queuePointer=", actionQueue$queuePointer, 
                   actionQueue$actions[[actionQueue$queuePointer]]@name, "\n")
  }
  if(verbose) cat("FINISHED executeQueue_")
}
#environment(executeQueue) = actionQueue

addToQueue = function(event, time=now()) {
  environment(addToQueue_) = actionQueue
  ## This was to allow debug(addToQueue_), but it didn't work.
  addToQueue_(event, time)
  cat("actionQueue$queueTimes=", actionQueue$queueTimes, "\n")
}  
addToQueue_ = function(event, time) {
  #temp = ls()  ##OK
  #print(temp)
  sortposition = length(queueTimes) + 1
  if(sortposition == 1) {
    queueTimes <<- 0
    actions[[1]] <<- event
  }
  else {
    while(time < queueTimes[sortposition-1]) 
      sortposition = sortposition - 1  ## should never be < 1 though.
 #   cat("sortposition=", sortposition, "\n")
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
#  cat("length of actions is now ", length(actions), "\n")
#  cat("actions saved correctly? ", identical(actions, actionQueue$actions), "\n")
}

debug(addToQueue_)
# By this test, it should work:
# ft=function()x; ftemp = function(E) { environment(ft) = E; ft()}; debug(ft)

# initializeQueue(firstaction="first")
# actionQueue$actions
# addToQueue("two", 2)
# actionQueue$actions
# addToQueue("three", 3)
# actionQueue$actions
# addToQueue("1.5", 1.5)
# actionQueue$actions
#####

#setClassUnion("Scenario", members=c("ListOfInserts"))
# F__k!! Fails with doAction(actionQueue$actions[[1]], scenario)
# Error in (function (classes, fdef, mtable)  : 
#             unable to find an inherited method for function ‘doAction’ for signature ‘"ScaffoldEvent", "ListOfInserts"’ 
# I'd prefer that Scenario extend ListOfInserts, but that doesn't work either.

# setMethod("doAction", signature=list("Action", "Scenario"), #####

setMethod("doAction", signature=list("Event", "ListOfInserts"), ######
          function(event, scenario=defaultScenario, ...) {
            cat("Event ", event@name , "is happening\n")
          }
)

doActionEvent = function(event, scenario=defaultScenario, ...){
  doAction(as(object=event, "Event"), scenario=defaultScenario, ...)
  doThisAction(event, scenario)
  shouldIjump = eval(parse(text=event@jumpIf))
  cat("doAction.ScaffoldEvent: shouldIjump=", shouldIjump, "\n")
  ## TODO: handle getting the value from the patient or CT VN.
  if(shouldIjump)
    addToQueue(get(event@jumpTo), 
               time=now())
  else 
    addToQueue(get(event@successor))
}
setMethod("doAction", signature=list("ScaffoldEvent", "ListOfInserts"),
          doActionEvent
)

doThisAction = function(event, scenario=defaultScenario)
  do.call(paste0("doThisAction_", event@name), list(scenario=scenario))

doThisAction_BeginClinicalTrial = function(scenario=defaultScenario) {
    cat("Must now initializeCTdata()\n")
    assign("trialData", new.env(), pos=1) #where is best?
    trialData$NpatientsEnrolled = 0
    trialData$patientData = list() ## This will hold enrolled patients.
  }
doThisAction_GeneratePatient = function(scenario=defaultScenario) {
  cat("Must generate a candidate patient now;  
                  gather VG's and evaluate.\n")
  assign("candidatePatient", new.env(), env=trialData)
  makeCandidate = function() {
    vgList = VariableGeneratorList(getVGs(scenario, 
                                          "PatientAttribute"))
    candidateVN = VariableNetwork(vgList=vgList)
    VVenv = evaluateVNoutputs(candidateVN)
    ###  Includes parameters as well as VV's.
    ## You can use a VV name in an expression as its value, e.g. numeric!  Nice!
    printVVenv(VVenv)
    trialData$candidatePatient$VVenv = VVenv
  }
  environment(makeCandidate) <- trialData$candidatePatient
  makeCandidate()
}
doThisAction_CheckEligibility = function(scenario=defaultScenario) {
  cat("Gather eligibilityCriterion objects from scenario.",
      "Form a Variable Network. ",
      "Retrieve all VariableValues,
                  and return the conjunction with all().\n")
  eligibilityVariables = VariableList(
    sapply(getVGs(scenario, "EligibilityCriterion"),
         slot, "provisions"))
  v_notEligibleVariable = Variable(name="notEligible", 
                                   description="whether patient is not eligible; used in doThisAction_CheckEligibility",
                                   checkDataType=is.logical,
                                   gitAction="none")
  # cat("....... ", eligibilityVariables)   ###   OK
  vg_notEligible = VariableGenerator(insertSubType="EligibilityCriterion",
                                    parameters=list(iAmAParameter=TRUE),
                                    requirements=eligibilityVariables,
                                    provisions=v_notEligibleVariable,
                                    generatorCode=function(){} # body is filled in below.
  )
  criteriaNames = names(eligibilityVariables)
  criteriaValueVectorText = 
    paste0("c(", paste(criteriaNames, collapse=", "), ")")
  criteriaNameVectorText = 
    paste0("c('", paste(criteriaNames, collapse="', '"), "')")
  body(vg_notEligible@generatorCode) = parse(text=paste("{
        criteriaValues = ", criteriaValueVectorText, "
        names(criteriaValues) = ", criteriaNameVectorText, "
        print(criteriaValues)
        whichViolated = which(criteriaValues == FALSE)
        notEligible = any(whichViolated)
        if(notEligible) 
          cat('Eligibility violation(s): ', 
            names(criteriaValues[whichViolated]), '\n')
        return(notEligible)
    }")
   )
  candidateVN = VariableNetwork(vgList=VariableGeneratorList(vgList=c(
    getVGs(scenario, "PatientAttribute"),
    getVGs(scenario, "EligibilityCriterion"),
    vg_notEligible=vg_notEligible
  )))
  VVenv = evaluateVNoutputs(candidateVN)
#  printVVenv(VVenv)    
  print(sapply(names(which(sapply(VVenv, is.logical))), get, env=VVenv))
  trialData$candidatePatient$VVenv = VVenv
  #print(VVenv)
}
doThisAction_EnrollPatient = function(scenario=defaultScenario) {
  cat("Enrolling the patient; copy patient info.\n")
  increment(NpatientsEnrolled, trialData)
  trialData$patientData[trialData$NpatientsEnrolled] = trialData$candidateVN
  theCurrentPatient = trialData$patientData[trialData$NpatientsEnrolled] 
}
doThisAction_AssignTreatmentPlan = function(scenario=defaultScenario) {
  cat("doThisAction_AssignTreatmentPlan", " not yet implemented\n")
}
doThisAction_GenerateOutcomes = function(scenario=defaultScenario) {
  cat("doThisAction_GenerateOutcomes", " not yet implemented\n")
}
doThisAction_CheckOffStudy = function(scenario=defaultScenario) {
  cat("doThisAction_CheckOffStudy", " not yet implemented\n")
}
doThisAction_CheckModifications = function(scenario=defaultScenario) {
  cat("doThisAction_CheckModifications", " not yet implemented\n")
}
doThisAction_SummarizePatient = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizePatient", " not yet implemented\n")
}
doThisAction_CheckStoppingRules = function(scenario=defaultScenario) {
  cat("doThisAction_CheckStoppingRules", " not yet implemented\n")
}
doThisAction_SummarizeTrial = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizeTrial", " not yet implemented\n")
}


runTrial = function(scenario=defaultScenario) {
  initializeQueue()
  executeQueue()
}

if(interactive()) runTrial()  ## skip when building.
