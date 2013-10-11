cat("======== ActionGenerator.R ================\n")

  ### NOT completed!!!

#' Classes and Methods for Action Queue

#' Class: Action
#' 
# OtherArgs:the arguments whose values are obtained from the method call within which this action is generated; 
# OtherArgs is a named list 
# GlobalTime in an "Action" object refers to the time when the action method is called.
# In an action queue, all GlobalTime's should have the same reference time point.
# If no time is available in the design specification, GlobalTime can be 1,2,3...for
# first action, second action, third action, etc in an action queue and when a new action is added to
# the queue, its GlobalTime is (GlobalTime of the last action in a queue + 1)
setClass("Action", contains="VariableGenerator",
         slots=list(
           label="character",
           description="character", ### For easy reference. 
           actionGenerator="function",
           OtherArgs="list", 
           GlobalTime="numeric"))

#Generate a new patient. -> schedule event NewPatient (now)
new("Action", label="check eligibility"
      actionGenerator=function() )

Action = function(label, description, actionGenerator, OtherArgs, GlobalTime, 
                  outputVariable, generatorCode, 
                  parameters, requirements, provisions){
  new("Action", label=label, description=description, 
      actionGenerator=actionGenerator, 
      OtherArgs=OtherArgs, GlobalTime=GlobalTime, 
      outputVariable=outputVariable, generatorCode=generatorCode, 
      parameters=parameters, requirements=requirements, provisions=provisions)
}

ac_newPatient = Action(label="generate new patient",
                       description="Generate a new patient.", 
                       actionGenerator
                       

myDesign = ActionList(list=list(
  ac_newPatient, ac_compute_eligibility, ac_enrollPatient,
  ac_scheduleTreatments, 
                        )

# New patient event (N)  ->  check eligibility (N) -> variable value
# -> event if(eligible) accrue event (N)
# Accrue -> modify variable patientCount
# -> initialize treatment assignment
# -> schedule treatment event
# Treatment event 	-> generate and store outcome for patient
# -> Check stopping rule    	-> Trial-stopped-event
# -> Generate a new patient
# Trial-stopped-event -> generate and store conclusion for the trial.
# -> generate and store summary statistics for the trial.


## Method: getOtherArgs
setGeneric("getOtherArgs",function(action) standardGeneric("getOtherArgs"))

setMethod("getOtherArgs",signature(action="Action"),
          function(action){
            OtherArgs <- action@OtherArgs 
            NArgs <- length(OtherArgs)
            if (NArgs != 0){
              for ( i in 1:length(OtherArgs))
                assign(names(OtherArgs)[i],OtherArgs[[i]],envir=parent.frame())
            }
          }
)


setClass("ActionList", contains="list")

ActionList = function(actions=NULL) {
  if(is.null(actions)) actions=list()
  acList = new("ActionList", actions)
  if(length(actions==0)
     | all(sapply(actions, is, class2="Action")))
    return(acList)
  stop("ActionList: not all are actions.")
}

setClass("ScheduleEventAction",
         slots=list(eventName="character",
         delay="numeric"))

setClass("ScheduleRepeatedEventAction",
         slots=list(eventName="character",
         delay="numeric",
         timeInterval="numeric"))

setClass("ActionGenerator", contains="Specifier",
         slots=list(
           outputActions="ActionList", generatorCode="function" 
         )
         ,
         validity=function(object) { # has to be "object"
             return(TRUE)
           }
)

ActionGenerator = function(parameters=list(), provisions, 
                             requirements=NULL,
                             outputVariable, generatorCode) {
  if(missing(provisions)) provisions=list(outputVariable)
  if(missing(outputVariable)) outputVariable=provisions
  vg = new("ActionGenerator", 
           parameters=parameters,
           provisions=provisions,
           requirements=requirements,
           generatorCode=generatorCode,
           outputVariable=outputVariable)
  if(is(requirements, "Variable"))requirements=list(requirements)
  environment(vg@generatorCode) = new.env()
  if(length(parameters) > 0)
    environment(vg@generatorCode) = list2env(parameters, new.env())
  vg
}


setMethod("print", "VariableGenerator", function(x){
  cat("    output: ", x@provisions, "\n")
  for (req in x@requirements) {
    cat("       req: ", req, "\n") ### Omits all but the Variable name slot.
  }
}
)          


