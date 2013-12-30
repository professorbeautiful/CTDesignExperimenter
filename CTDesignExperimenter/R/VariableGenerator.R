cat("======== VariableGenerator.R ================\n")

setClass("VariableGenerator", contains="Specifier",
         slots=list(
           outputVariable="Variable"
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

VariableGenerator = function(parameters=list(), provisions, 
                             requirements=NULL,
                             outputVariable, generatorCode) {
  if(missing(provisions)) provisions=outputVariable
  if(missing(outputVariable)) outputVariable=provisions
  vg = new("VariableGenerator", 
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


