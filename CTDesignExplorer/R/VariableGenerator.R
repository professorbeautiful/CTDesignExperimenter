setClass("VariableGenerator", contains="Specifier",
         slots=list(
           outputVariable="Variable"
           ### this will be the provision. At first, a string. Later, a Variable.
           , generatorCode="function" 
           ### Arguments are the requirements,
           ### which are VariableValues.
         ),
         validity=function(object) { # has to be "object"
           if(length(object@requirements)==0) 
             return(TRUE)## For now.
           else {
             for(req in object@requirements){
               if(!is(req, "VariableGenerator"))
                 return(paste0("VariableGenerator: error: ",
                               "req not a VariableGenerator:\n",
                               req))
             }
             return(TRUE)
           }
         }
)
VariableGenerator = function(parameters=list(), provisions, 
                             requirements=list(),
                             outputVariable, generatorCode) {
  vg = new("VariableGenerator", 
           parameters=parameters,
           provisions=provisions,
           requirements=requirements,
           generatorCode=generatorCode,
           outputVariable=outputVariable)
  environment(vg@generatorCode) = list2env(parameters, new.env())
  vg
}

setMethod("print", "VariableValue",
  function(x)
    cat("Variable: ", x@variable@.Data, "  Value: ", x@value, "\n"))
