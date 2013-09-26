cat("======== ActionGenerator.R ================\n")

  ### NOT completed!!!

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


