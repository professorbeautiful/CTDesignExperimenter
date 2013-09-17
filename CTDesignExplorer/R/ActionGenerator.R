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

setClass("ActionGenerator", contains="Specifier",
         slots=list(
           outputActions="ActionList"
           ### this will be the provision. At first, a string. Later, a Variable.
           , generatorCode="function" 
           ### Arguments are the requirements,
           ### which are VariableValues.
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


