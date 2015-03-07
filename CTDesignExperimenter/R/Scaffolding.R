cat("======== Scaffolding.R  ================\n")

if(interactive()) library(pryr)  ## Hadley Wickham utilities for S4
if(interactive()) library(RBioinf);  help(p="RBioinf")

### First, some utility functions #####
className = function(obj) {
  if(isClass(obj)) return(obj@className)
  # is(class2="Class", obj)  returns FALSE !!
  return(class(obj))
}

#' increment - add to a variable, generally a counter or time.
#' 
#' increment - add to a variable, generally a counter or time.
#' @param X Variable to increment. Unquoted name.
#' @param ENV Environment where X is located. Default = parent.frame().
#' @param delta increment. Default = 1.
#' @param verbose Print detail? Default = FALSE.
#' 
#'  @details Increments and assigns new value to X.
#'   
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
                    jumpTo="character" # alternative destination
         )) #expression

##' scaffoldObjectNames ####
##' 
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
  PatientOutcome,
  OffStudyCriterion,
  ModificationRule,
  PatientSummary,
  StoppingCriterion,
  TrialSummary,
  SimulationSummary  ## Same as evaluation criterion?
)
scaffoldObjects = data.frame(stringsAsFactors=FALSE,
  row.names=scaffoldObjectNames,
  blockName=scaffoldObjectNames,
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
           print(blockName), new("ScaffoldEvent",
                            name=blockName,
                            predecessor=ifelse(scaf>1, scaffoldObjects$blockName[scaf-1], ""),
                            successor=ifelse(scaf<scafSize, scaffoldObjects$blockName[scaf+1], ""),
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

getVGs = function(scenario, subType) {
  if(is(scenario, "Scenario")) it = scenario@inserts
  else if(is(scenario, "ListOfInserts")) it = scenario
  else stop("getVGs: Incorrect class for arg scenario")
  whichOnes = (sapply(it, slot, name="insertSubType") == subType)
  return(it[whichOnes])
}

createTrialVariableNetworks = function(scenario=currentScenario,
                                       env=.GlobalEnv) {
  for(iInsertType in seq(along=scaffoldBlockNames)) {
    scaffoldInsertSubType = scaffoldInsertSubTypes[iInsertType]
    if(scaffoldInsertSubType!="") {
      scaffoldBlockName = scaffoldBlockNames[iInsertType]
      vgList = VariableGeneratorList(getVGs(scenario@inserts, 
                                            scaffoldInsertSubType))
      if(length(vgList) > 0) {
        theVN = VariableNetwork(vgList=vgList)
        assign("VN_" %&% scaffoldBlockName,
               theVN, env=env)
        ifVerboseCat("VN_" %&% scaffoldBlockName, " matrix: \n",
                     VN_AssignTreatmentPlan@requirementMatrix)
      }
    }
  } 
  VN_all = VariableNetwork(
    vgList=VariableGeneratorList(c(currentScenario@inserts)))
  assign("VN_all", VN_all, env=env)
}

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

executeQueue = function(scenario=defaultScenario){
  # Names of the VGs must be unique.
  stopifnot(length(unique(names(scenario@inserts))) == length(scenario@inserts))
  nActions <<- length(actionQueue$actions)
  ifVerboseCat("executeQueue_: nActions = ", nActions, "\n")
  while(actionQueue$queuePointer <= nActions) {
    doAction(actionQueue$actions[[actionQueue$queuePointer]], scenario)
    increment(ENV=actionQueue, queuePointer)
    nActions <<-length(actionQueue$actions)
    ifVerboseCat(  # (actionQueue$queuePointer <= nActions)) 
      "executeQueue_: nActions = ", nActions,
                    "queuePointer=", actionQueue$queuePointer, 
                    actionQueue$actions[[actionQueue$queuePointer]]@name, "\n"
    )
  }
  ifVerboseCat("FINISHED executeQueue_")
}
#environment(executeQueue) = actionQueue

addToQueue = function(event, 
                      time=eval(parse(text=event@timeToNextEvent)),
                      who=eval(parse(text=event@who))) {
  environment(addToQueue_) = actionQueue
  ## This was to allow debug(addToQueue_), but it didn't work.
  addToQueue_(event, time, who)
  ifVerboseCat("actionQueue$queueTimes=", actionQueue$queueTimes, "\n")
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
  ifVerboseCat("actionQueue$queuePointer = ", actionQueue$queuePointer, "\n")
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

setMethod("doAction", signature=list("Event", "Scenario"), ######
          function(event, scenario=defaultScenario, ...) {
            ifVerboseCat("Event ", event@name , "is happening\n")
          }
)

####debug
doActionEvent = function(event, scenario=defaultScenario, ...){
  eventName = event@name  ## To see it in Environment tab.
  doAction(as(object=event, "Event"), scenario=scenario, ...)
  doThisAction(event, scenario)
  jumpIfString = event@jumpIf   ## To see it in Environment tab.
  #browser("Stopping for jumpIf", expr=(jumpIfString!="FALSE"))
  shouldIjump = eval(parse(text=event@jumpIf))@.Data
  ifVerboseCat("doAction.ScaffoldEvent: shouldIjump=", shouldIjump, "\n")
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
setMethod("doAction", signature=list("ScaffoldEvent", "Scenario"),
          doActionEvent
)

doThisAction = function(event, scenario=defaultScenario)
  do.call(paste0("doThisAction_", event@name), list(scenario=scenario))

doThisAction_BeginClinicalTrial = function(scenario=defaultScenario) {
  ifVerboseCat("Must now initializeCTdata()\n")
  assign("trialData", new.env(), pos=1) #where is best?
  vgList = VariableGeneratorList(getVGs(scenario@inserts, 
                                        "DesignParameter"))
  designVN = VariableNetwork(vgList=vgList)
  trialData$designParameters = new.env()
  envCopy(evaluateVNoutputs(designVN), trialData$designParameters)
  trialData$trialSummaries = new.env()
  trialData$trialSummaries$NpatientsEnrolled = 0
  trialData$patientData = list() ## This will hold enrolled patients.
}
## TODO:  where do we put the accrual pattern? 
# Currently in scaffoldObjects$timeToNextEvent
doThisAction_GeneratePatient = function(scenario=defaultScenario) {
  ifVerboseCat("Must generate a candidate patient now;  
      gather VG's and evaluate.\n")
  if(is.null(trialData$candidatePatient))
    assign("candidatePatient", new.env(), env=trialData)
  else
    rm(list=ls(envir=trialData$candidatePatient), envir=trialData$candidatePatient)
  
  makeCandidate = function() {
    vgList = VariableGeneratorList(getVGs(scenario@inserts, 
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
  ifVerboseCat("Gather eligibilityCriterion objects from scenario.",
      "Form a Variable Network. ",
      "Retrieve all VariableValues,
      and return the conjunction with all().\n")
  eligibilityVariables = VariableList(
    sapply(getVGs(scenario@inserts, "EligibilityCriterion"),
           slot, "provisions"))
  v_notEligibleVariable = Variable(name="notEligible", 
                                   description="whether patient is not eligible; used in doThisAction_CheckEligibility",
                                   checkDataType=is.logical,
                                   gitAction="none")
  # ifVerboseCat("....... ", eligibilityVariables)   ###   OK
  vg_notEligible = VariableGenerator(insertSubType="EligibilityCriterion",
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
      ifVerboseCat('Eligibility violation(s): ', 
        names(criteriaValues[whichViolated]), '\n')
    return(notEligible)
    }")
  )
  eg_VN = VariableNetwork(vgList=VariableGeneratorList(
    vgList=c(
      getVGs(scenario@inserts, "PatientAttribute"),
      getVGs(scenario@inserts, "EligibilityCriterion"),
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
  ifVerboseCat("Enrolling the patient; copying patient info!!!!!!!!\n")
  increment(NpatientsEnrolled, ENV=trialData$trialSummaries)
  trialData$NcurrentPatient = trialData$trialSummaries$NpatientsEnrolled
  trialData$currentPatient =
    trialData$patientData[[trialData$trialSummaries$NpatientsEnrolled]] =
      new.env()
  trialData$currentPatient$VVenv = new.env()
  currentPatient <<- trialData$currentPatient  ### doesn't always work.
  ifVerboseCat("&1& currentPatient$VVenv contains candidatePatient? ", 
               "candidatePatient" %in% ls(envir=currentPatient$VVenv) )
  ## Clone, not reference here! VVenv is an environment
  envCopy(trialData$candidatePatient$VVenv, trialData$currentPatient$VVenv)
  ifVerboseCat("&2& currentPatient$VVenv contains candidatePatient? ", 
               "candidatePatient" %in% ls(envir=currentPatient$VVenv) , "\n")
}

envCopy = function(envFrom, envTo, clear=TRUE, 
                   excluded=character(0), 
                   copySubEnvironments=FALSE) {
  for(vv in ls(env=envFrom) %except% excluded) {
    #if(vv=="currentPatient") browser()
    #if(vv=="candidatePatient") browser()
    obj = get(vv, env=envFrom)
    if( ( !(class(obj) == "environment")) )
      assign(vv, env=envTo, obj)
  }
}  

doThisAction_AssignTreatmentPlan = function(scenario=defaultScenario) {
  treatmentVariables = VariableList(
    sapply(getVGs(scenario@inserts, "ScheduleTreatment"),
           slot, "provisions"))
  treatmentVgList = VariableGeneratorList(getVGs(scenario@inserts, 
                                          "ScheduleTreatment"))
  treatmentVN = VariableNetwork(vgList=treatmentVgList)
  trialData$currentPatient$VVenv = evaluateVNoutputs(
    treatmentVN, 
    list2env(x=c(as.list(trialData$designParameters), 
                 as.list(trialData$trialSummaries)),
             trialData$currentPatient$VVenv)
    # TODO: copy design parameter values into trialData$parameters,
    # TODO: create and update current trial summaries into trialData.    
  )
}

doThisAction_GenerateOutcomes = function(scenario=defaultScenario) {
  outcomesVgList = VariableGeneratorList(getVGs(scenario@inserts, 
                                                 "PatientOutcome"))
  outcomesVN = VariableNetwork(vgList=outcomesVgList)
  trialData$currentPatient$VVenv = evaluateVNoutputs(
    outcomesVN, 
    list2env(x=as.list(trialData$designParameters), 
             trialData$currentPatient$VVenv))
}

doThisAction_CheckOffStudy = function(scenario=defaultScenario) {
  ###  CAUTION: we do not want to regenerate the patient values.
  ###  Just add new VGs to the VVenv, and process them.
  ifVerboseCat("Gather offStudyCriterion objects from scenario.",
      "Form a Variable Network. ",
      "Retrieve all VariableValues,
      and return the disjunction (union) with any().\n")
  offStudyVariables = VariableList(
    sapply(getVGs(scenario@inserts, "offStudyCriterion"),
           slot, "provisions"))
  v_notOffStudyVariable = Variable(name="notOffStudy", 
                                   description="whether patient is offStudy; used in doThisAction_CheckoffStudy",
                                   checkDataType=is.logical,
                                   gitAction="none")
  # ifVerboseCat("....... ", offStudyVariables)   ###   OK
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
          ifVerboseCat('offStudy rule(s) triggered: ', 
          names(criteriaValues[whichTriggered]), '\n')
        notOffStudy = ! offStudy
        return(notOffStudy)
    }")
  body(vg_notOffStudy@generatorCode) = parse(text=paste(theGeneratorBody))
  offStudyVN = VariableNetwork(vgList=VariableGeneratorList(vgList=c(
    getVGs(scenario@inserts, "OffStudyCriterion"),
    vg_notOffStudy=vg_notOffStudy  ### Adding the final assessment.
  )))
  currentPatient <<- trialData$currentPatient  ## This DOES work here.
  ifVerboseCat(" CHECKING: in checkEligibility, make sure we don't re-do initial variables.\n")
  ifVerboseCat("BEFORE\n")
  ifVerboseCat(printVVenv(currentPatient$VVenv) )   
  currentPatient$VVenv = evaluateVNoutputs(offStudyVN, currentPatient$VVenv)
  ## TODO: in checkEligibility, make sure we don't re-do initial variables.
  ifVerboseCat("AFTER\n")
  ifVerboseCat(printVVenv(currentPatient$VVenv))    
#   > trialData$currentPatient$temp = "current"
#   > trialData$patientData[[1]]$temp
#   [1] "current"
}

doThisAction_CheckModifications = function(scenario=defaultScenario) {
  cat("doThisAction_CheckModifications", " not yet implemented\n")
}

doThisAction_SummarizePatient = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizePatient", " not yet implemented\n")
  ## TODO: implement updating trial summaries in trialData$trialSummaries.
}

doThisAction_SummarizeSimulation = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizeSimulation", " not yet implemented\n")
  ## TODO: implement updating simulation summaries.
}

doThisAction_CheckStoppingRules = function(scenario=defaultScenario) {
  ###  CAUTION: we do not want to regenerate the patient values.
  ###  Just add new VGs to the VVenv, and process them.
  ifVerboseCat("Gather StoppingRule objects from scenario.",
      "Form a Variable Network. ",
      "Retrieve all VariableValues,
      and return the conjunction with any().\n")
  stoppingRuleVariables = VariableList(
    sapply(getVGs(scenario@inserts, "StoppingCriterion"),
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
      ifVerboseCat('whichTriggered: ', whichTriggered, '\n')
      stopping = any(whichTriggered)
      continueAccrual = ! stopping
      # if(stopping) 
        ifVerboseCat(' Stopping rules(s): ', 
          names(criteriaValues[whichTriggered]), '\n')
      return(continueAccrual)
  }")
  body(vg_notStopping@generatorCode) = parse(text=paste(theGeneratorBody))
  assign("vg_notStopping", vg_notStopping, pos=1)
  stoppingVN = VariableNetwork(
    vgList=VariableGeneratorList(vgList=c(
      getVGs(scenario@inserts, "StoppingCriterion"),
      vg_notStopping=vg_notStopping
  )))
  envStopping = new.env()
  envCopy(trialData$designParameters, envStopping, 
          clear=TRUE, copySubEnvironments=FALSE) # These are the defaults anyway.
  trialDataAugmented = 
    evaluateVNoutputs(stoppingVN, envVariableValues=envStopping)
  ifVerboseCat("trialDataAugmented:\n")
  printENV(trialDataAugmented)
  envCopy(trialDataAugmented, trialData, clear=FALSE)
  
  #   > trialData$currentPatient$temp = "current"
  #   > trialData$patientData[[1]]$temp
  #   [1] "current"   # So it works!
}

doThisAction_SummarizeTrial = function(scenario=defaultScenario) {
  cat("doThisAction_SummarizeTrial", 
      " #patients = ", trialData$trialSummaries$NpatientsEnrolled, "\n")
}

runTrial = function(scenario=defaultScenario) {
  options(error=recover)
  #startup()
  initializeQueue()
  executeQueue(scenario=scenario)
}

if(interactive()) runTrial()  ## skip when building.
