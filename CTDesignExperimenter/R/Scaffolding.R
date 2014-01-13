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
                    timeToNextEvent="character", #expression
                    who="character")) #expression

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
# If the candidate patient is notEligible jump back to GeneratePatient
# Otherwise continue to EnrollPatient
scaffoldObjects["CheckEligibility", "jumpTo"] = "GeneratePatient"
scaffoldObjects["CheckEligibility", "jumpIf"] = "trialData$candidatePatient$VVenv$notEligible"

# If the current patient is offStudy jump forward to SummarizePatient
# Otherwise continue to CheckModifications
scaffoldObjects["CheckOffStudy", "jumpIf"] = "trialData$currentPatient$VVenv$notOffStudy"
scaffoldObjects["CheckOffStudy", "jumpTo"] = "SummarizePatient"
# CheckModifications makes the modifications, then always returns to GenerateOutcomes

# After SummarizePatient, we do CheckStoppingRules
# continueAccrual is TRUE if all stopping rules are FALSE.
# If continueAccrual, jump back to GeneratePatient
# Otherwise continue to SummarizeTrial
scaffoldObjects["CheckStoppingRules", "jumpIf"] = "trialData$continueAccrual"
scaffoldObjects["CheckStoppingRules", "jumpTo"] = "GeneratePatient"

scaffoldObjects$timeToNextEvent = "incrementNow()"
scaffoldObjects$who = "paste('Patient #', trialData$NcurrentPatient)"
scaffoldObjects["BeginClinicalTrial", "who"] = "'Trial'"
scaffoldObjects["GeneratePatient", "who"] = "'Candidate'"
scaffoldObjects["CheckEligibility", "who"] = "'Candidate'"
scaffoldObjects["SummarizeTrial", "who"] = "'Trial'"
scaffoldObjects["SummarizeSimulation", "who"] = "'Summarize'"

  
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
                            timeToNextEvent=scaffoldObjects$timeToNextEvent[scaf],
                            who=scaffoldObjects$who[scaf]                
           )))
  }
}

makeScaffoldObjects() # For building the package.

### Context = Evaluation or scenario or CT or patient.#####
### Thus, hierarchical, with 4 levels.

v_SampleSizeMax = Variable(name="SampleSizeMax", 
         description='Upper bound for sample size', 
         checkDataType=is.numeric)

createVG_FixedSampleSizeMax = function(Nmax = 3) {
  VariableGenerator(insertSubType="DesignParameter", 
                    parameters=list(SampleSizeMax=Nmax),
                    provisions=v_SampleSizeMax, 
                    generatorCode=function(){
                      SampleSizeMax
                    }
  )
}

v_SampleSizeMaxIsReached = Variable(name="SampleSizeMaxIsReached", 
                           description='Upper bound for sample size has been reached.', 
                           checkDataType=is.logical)
vg_SampleSizeMaxIsReached = VariableGenerator(insertSubType="StoppingCriterion", 
                  requirements=VariableList(v_SampleSizeMax),
                  provisions=v_SampleSizeMaxIsReached, 
                  generatorCode=function(){
                    SampleSizeMax <= trialData$NpatientsEnrolled
                  }
)



## Creating a default scenario #####

defaultScenario =  #as("Scenario",
  new("ListOfInserts", list(
    vg_liver=vg_liver, vg_age=vg_age, 
    ec_liver=ec_liver, ec_age=ec_age,
    vg_clearanceRate=vg_clearanceRate, 
    vg_responseDoseThreshold=vg_responseDoseThreshold,
    vg_toxDoseThreshold=vg_toxDoseThreshold,
    vg_SampleSizeMax = createVG_FixedSampleSizeMax(2),
    vg_SampleSizeMaxIsReached = vg_SampleSizeMaxIsReached)
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
  assign("actionIDs", eval(parse(text=firstaction@who)), env=actionQueue)
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
    if(verbose & (actionQueue$queuePointer <= nActions)) 
      cat("executeQueue_: nActions = ", nActions,
                    "queuePointer=", actionQueue$queuePointer, 
                    actionQueue$actions[[actionQueue$queuePointer]]@name, "\n")
  }
  if(verbose) cat("FINISHED executeQueue_")
}
#environment(executeQueue) = actionQueue

addToQueue = function(event, 
                      time=eval(parse(text=event@timeToNextEvent)),
                      who=eval(parse(text=event@who))) {
  environment(addToQueue_) = actionQueue
  ## This was to allow debug(addToQueue_), but it didn't work.
  addToQueue_(event, time, who)
  cat("actionQueue$queueTimes=", actionQueue$queueTimes, "\n")
}  

addToQueue_ = function(event, time, who) {
  #temp = ls()  ##OK
  #print(temp)
  sortposition = length(queueTimes) + 1
  if(sortposition == 1) {
    queueTimes <<- 0
    actions[[1]] <<- event
    actionIDs[[1]] <<- who
  }
  else {
    while(time < queueTimes[sortposition-1]) 
      sortposition = sortposition - 1  ## should never be < 1 though.
    #   cat("sortposition=", sortposition, "\n")
    if(sortposition==1) {
      actions <<- c(event, actions)
      queueTimes <<- c(time, queueTimes)
      actionIDs <<- c(who, actionIDs)
    }
    else if (sortposition==(length(actions)+1)) {
      actions <<- c(actions, event)
      queueTimes <<- c(queueTimes, time)
      actionIDs <<- c(actionIDs, who)
    }  
    else {
      actions <<- c(actions[1:(sortposition-1)],
                    event, actions[sortposition:length(actions)])
      queueTimes <<- c(queueTimes[1:(sortposition-1)],
                       time, queueTimes[sortposition:length(queueTimes)])
      actionIDs <<- c(actionIDs[1:(sortposition-1)],
                       who, actionIDs[sortposition:length(actionIDs)])
    } 
  }
  #  cat("length of actions is now ", length(actions), "\n")
  #  cat("actions saved correctly? ", identical(actions, actionQueue$actions), "\n")
}

viewQueue=function(){
  cat("actionQueue$queuePointer = ", actionQueue$queuePointer, "\n")
  data.frame( 
    event=sapply(1:length(actionQueue$actions), function(i)actionQueue$actions[[i]]@name),  
    time=actionQueue$queueTimes,
    who=actionQueue$actionIDs)
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

####debug
doActionEvent = function(event, scenario=defaultScenario, ...){
  eventName = event@name  ## To see it in Environment tab.
  doAction(as(object=event, "Event"), scenario=defaultScenario, ...)
  doThisAction(event, scenario)
  jumpIfString = event@jumpIf   ## To see it in Environment tab.
  #browser("Stopping for jumpIf", expr=(jumpIfString!="FALSE"))
  shouldIjump = eval(parse(text=event@jumpIf))@.Data
  cat("doAction.ScaffoldEvent: shouldIjump=", shouldIjump, "\n")
  ## TODO: handle getting the value from the patient or CT VN.
  if(event@successor != "") {
    if(shouldIjump)
      nextEvent = get(event@jumpTo)
    else
      nextEvent = get(event@successor)
    addToQueue(nextEvent,     
             time=eval(parse(text=nextEvent@timeToNextEvent)),
             who=eval(parse(text=nextEvent@who))
    )
  }
}
setMethod("doAction", signature=list("ScaffoldEvent", "ListOfInserts"),
          doActionEvent
)

doThisAction = function(event, scenario=defaultScenario)
  do.call(paste0("doThisAction_", event@name), list(scenario=scenario))

doThisAction_BeginClinicalTrial = function(scenario=defaultScenario) {
  cat("Must now initializeCTdata()\n")
  assign("trialData", new.env(), pos=1) #where is best?
  vgList = VariableGeneratorList(getVGs(scenario, 
                                        "DesignParameter"))
  designVN = VariableNetwork(vgList=vgList)
  envCopy(evaluateVNoutputs(designVN), trialData)
  trialData$NpatientsEnrolled = 0
  trialData$patientData = list() ## This will hold enrolled patients.
}
## TODO:  where do we put the accrual pattern? 
# Currently in scaffoldObjects$timeToNextEvent
doThisAction_GeneratePatient = function(scenario=defaultScenario) {
  cat("Must generate a candidate patient now;  
      gather VG's and evaluate.\n")
  if(is.null(trialData$candidatePatient))
    assign("candidatePatient", new.env(), env=trialData)
  else
    rm(list=ls(envir=trialData$candidatePatient), envir=trialData$candidatePatient)
  
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
  trialData$NcurrentPatient = 0
  trialData$currentPatient = NULL
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
  body(vg_notEligible@generatorCode) = parse(text=paste(
    "{
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
  eg_VN = VariableNetwork(vgList=VariableGeneratorList(vgList=c(
    getVGs(scenario, "PatientAttribute"),
    getVGs(scenario, "EligibilityCriterion"),
    vg_notEligible=vg_notEligible
  )))
  VVenv = evaluateVNoutputs(eg_VN, 
                            trialData$candidatePatient$VVenv)
  #  printVVenv(VVenv)    
  print(sapply(names(which(sapply(VVenv, is.logical))), get, env=VVenv))
  trialData$candidatePatient$VVenv = VVenv
  #print(VVenv)
}

doThisAction_EnrollPatient = function(scenario=defaultScenario) {
  cat("Enrolling the patient; copying patient info.\n")
  increment(NpatientsEnrolled, ENV=trialData)
  trialData$NcurrentPatient = trialData$NpatientsEnrolled
  trialData$currentPatient =
    trialData$patientData[[trialData$NpatientsEnrolled]] =
      new.env()
  trialData$currentPatient$VVenv = new.env()
  ## Clone, not reference here! VVenv is an environment
#   for(vv in ls(env=trialData$candidatePatient$VVenv))
#       assign(vv, env=trialData$currentPatient$VVenv,
#         get(vv, env=trialData$candidatePatient$VVenv)
#   )
  envCopy(trialData$candidatePatient$VVenv, trialData$currentPatient$VVenv)
}

envCopy = function(envFrom, envTo, clear=TRUE, 
                   excluded=character(0), copySubEnvironments=FALSE) {
  for(vv in ls(env=envFrom) %except% excluded) {
    obj = get(vv, env=envFrom)
    if( ! (class(vv) == "environment"))
      assign(vv, env=envTo, obj)
  }
}  

doThisAction_AssignTreatmentPlan = function(scenario=defaultScenario) {
  cat("doThisAction_AssignTreatmentPlan", " not yet implemented\n")
}
doThisAction_GenerateOutcomes = function(scenario=defaultScenario) {
  cat("doThisAction_GenerateOutcomes", " not yet implemented\n")
}
doThisAction_CheckOffStudy = function(scenario=defaultScenario) {
  ###  CAUTION: we do not want to regenerate the patient values.
  ###  Just add new VGs to the VVenv, and process them.
  cat("Gather offStudyCriterion objects from scenario.",
      "Form a Variable Network. ",
      "Retrieve all VariableValues,
      and return the disjunction (union) with any().\n")
  offStudyVariables = VariableList(
    sapply(getVGs(scenario, "offStudyCriterion"),
           slot, "provisions"))
  v_notOffStudyVariable = Variable(name="notOffStudy", 
                                   description="whether patient is offStudy; used in doThisAction_CheckoffStudy",
                                   checkDataType=is.logical,
                                   gitAction="none")
  # cat("....... ", offStudyVariables)   ###   OK
  vg_notOffStudy = VariableGenerator(insertSubType="OffStudyCriterion",
                                     parameters=list(iAmAParameter=TRUE),
                                     requirements=offStudyVariables,
                                     provisions=v_notOffStudyVariable,
                                     generatorCode=function(){} # body is filled in below.
  )
  criteriaNames = names(offStudyVariables)
  criteriaValueVectorText = 
    paste0("c(", paste(criteriaNames, collapse=", "), ")")
  criteriaNameVectorText = 
    paste0("c('", paste(criteriaNames, collapse="', '"), "')")
  if(length(criteriaNames) == 0) theGeneratorBody =
    'return(FALSE)' ## if no criteria, the patient is off-study automatically.
  # TODO make checkEligibility like this too, in case no criteria.
  else
    theGeneratorBody = paste(
    "{
        criteriaValues = ", criteriaValueVectorText, "
        names(criteriaValues) = ", criteriaNameVectorText, "
        print(criteriaValues)
        whichTriggered = which(criteriaValues == FALSE)
        offStudy = any(whichTriggered)
        if(offStudy) 
          cat('offStudy rule(s) triggered: ', 
          names(criteriaValues[whichTriggered]), '\n')
        notOffStudy = ! offStudy
        return(notOffStudy)
    }")
  body(vg_notOffStudy@generatorCode) = parse(text=paste(theGeneratorBody))
  offStudyVN = VariableNetwork(vgList=VariableGeneratorList(vgList=c(
    getVGs(scenario, "OffStudyCriterion"),
    vg_notOffStudy=vg_notOffStudy  ### Adding the final assessment.
  )))
  currentPatient = trialData$currentPatient
  cat(" CHECKING: in checkEligibility, make sure we don't re-do initial variables.\n")
  cat("BEFORE\n")
  printVVenv(currentPatient$VVenv)    
  currentPatient$VVenv = evaluateVNoutputs(offStudyVN, currentPatient$VVenv)
  ## TODO: in checkEligibility, make sure we don't re-do initial variables.
  cat("AFTER\n")
  printVVenv(currentPatient$VVenv)    
#   > trialData$currentPatient$temp = "current"
#   > trialData$patientData[[1]]$temp
#   [1] "current"
}

doThisAction_CheckModifications = function(scenario=defaultScenario) {
  cat("doThisAction_CheckModifications", " not yet implemented\n")
}

doThisAction_SummarizePatient = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizePatient", " not yet implemented\n")
  ## TODO: implement updating trial summaries in trialData.
}

doThisAction_SummarizeSimulation = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizeSimulation", " not yet implemented\n")
  ## TODO: implement updating trial summaries in trialData.
}


doThisAction_CheckStoppingRules = function(scenario=defaultScenario) {
  ###  CAUTION: we do not want to regenerate the patient values.
  ###  Just add new VGs to the VVenv, and process them.
  cat("Gather StoppingRule objects from scenario.",
      "Form a Variable Network. ",
      "Retrieve all VariableValues,
      and return the conjunction with any().\n")
  stoppingRuleVariables = VariableList(
    sapply(getVGs(scenario, "StoppingCriterion"),
           slot, "provisions"))
  v_notStoppingVariable = Variable(name="continueAccrual", 
                                   description="whether study is NOT stopping",
                                   checkDataType=is.logical,
                                   gitAction="none")
  vg_notStopping = VariableGenerator(insertSubType="StoppingCriterion",
                                     parameters=list(iAmAParameter=TRUE),
                                     requirements=stoppingRuleVariables,
                                     provisions=v_notStoppingVariable,
                                     generatorCode=function(){} # body is filled in below.
  )
  criteriaNames = names(stoppingRuleVariables)
  criteriaValueVectorText = 
    paste0("c(", paste(criteriaNames, collapse=", "), ")")
  assign("criteriaValueVectorText", criteriaValueVectorText, pos=1)
  criteriaNameVectorText = 
    paste0("c('", paste(criteriaNames, collapse="', '"), "')")
  if(length(criteriaNames) == 0) theGeneratorBody =
    'return(TRUE)' ## if no criteria, the study stops automatically (N=1).
  else
    theGeneratorBody = paste(
      "{
      criteriaValues = ", criteriaValueVectorText, "
      names(criteriaValues) = ", criteriaNameVectorText, "
      print(criteriaValues)
      whichTriggered = which(criteriaValues == TRUE)
      cat('whichTriggered: ', whichTriggered, '\n')
      stopping = any(whichTriggered)
      continueAccrual = ! stopping
      # if(stopping) 
        cat(' Stopping rules(s): ', 
          names(criteriaValues[whichTriggered]), '\n')
      return(continueAccrual)
  }")
  body(vg_notStopping@generatorCode) = parse(text=paste(theGeneratorBody))
  assign("vg_notStopping", vg_notStopping, pos=1)
  stoppingVN = VariableNetwork(
    vgList=VariableGeneratorList(vgList=c(
      getVGs(scenario, "StoppingCriterion"),
      vg_notStopping=vg_notStopping
  )))
  envStopping = new.env()
  envCopy(trialData, envStopping, 
          clear=TRUE, copySubEnvironments=FALSE) # These are the defaults anyway.
  trialDataAugmented = 
    evaluateVNoutputs(stoppingVN, envVariableValues=envStopping)
  cat("trialDataAugmented:\n")
  printENV(trialDataAugmented)
  envCopy(trialDataAugmented, trialData, clear=FALSE)
  
  #   > trialData$currentPatient$temp = "current"
  #   > trialData$patientData[[1]]$temp
  #   [1] "current"   # So it works!
}

doThisAction_SummarizeTrial = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizeTrial", 
      " #patients = ", trialData$NpatientsEnrolled, "\n")
}

runTrial = function(scenario=defaultScenario) {
  options(error=recover)
  makeScaffoldObjects()
  initializeQueue()
  executeQueue()
}

scenarioNoElig = getVGs(defaultScenario, "PatientAttribute")

if(interactive()) runTrial()  ## skip when building.
