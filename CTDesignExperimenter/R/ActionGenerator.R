cat("======== ActionGenerator.R ================\n")

  ### NOT completed!!!

# An Event is like a Variable.
# An EventGenerator is like a VariableGenerator, except:
#    - generatorCode can cause events to go on the queue
#' Classes and Methods for Event Queue

#' Class: Action
#' 

setClass("Action", contains="Variable")
## Just a name, description, and possibly an EventLevel, patient or CT
## which governs if it finds its variables in a patient's data repository 
## or in the CT record.
##  EventLevel in c("patient", "clinicaltrial")

setClass("ActionGenerator", contains="VariableGenerator",
                  slots=list(
           label="character",
           description="character", ### For easy reference. 
           actionGenerator="function",
           OtherArgs="list", 
           GlobalTime="numeric"))

#Generate a new patient. -> schedule event NewPatient (now)
Action = function(label, description, actionGenerator, OtherArgs, GlobalTime, 
                  outputVariable, generatorCode, 
                  parameters, requirements, provisions){
  new("Action", label=label, description=description, 
      actionGenerator=actionGenerator, 
      OtherArgs=OtherArgs, GlobalTime=GlobalTime, 
      outputVariable=outputVariable, generatorCode=generatorCode, 
      parameters=parameters, requirements=requirements, provisions=provisions)
}

# ac_newPatient = Action(label="generate new patient",
#                        description="Generate a new patient.", 
#                        actionGenerator=new("ActionGenerator",
#                                            function()))

# new("Action", label="check eligibility",
#         actionGenerator=function() {})
#                        
# 
# myDesign = ActionList(list=list(
#   ac_newPatient, ac_compute_eligibility, ac_enrollPatient,
#   ac_scheduleTreatments
# ))

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
setGeneric("getOtherArgs",
           function(action) standardGeneric("getOtherArgs"))

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


