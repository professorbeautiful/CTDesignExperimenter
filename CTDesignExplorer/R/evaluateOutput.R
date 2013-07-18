
#' Evaluate a generator expression.
#' 
#' \code{evaluateOutput} 
#' \param generator A SimpleVariableGenerator 
#' \param input Input "requirements" list of VariableValue objects.

evaluateOutput = function(generator) {
  ## Make the generatorCode function available.
  if(!is(generator, "VariableGenerator"))
    stop("evaluateOutput: generator should be a VariableGenerator")
  if(length(extractRequirements(generator)) > 0)
    for(gen in extractRequirements(generator)) {
      genOutput = evaluateOutput(gen)
      genOutputList = list(genOutput@value)
      names(genOutputList) = gen@provisions
      environment(generator@generatorCode) = 
        list2env(genOutputList, environment(generator@generatorCode))
    }
  print(ls(envir=environment(generator@generatorCode)))
  value = generator@generatorCode()
  return(new("VariableValue", variable=generator@outputVariable, value=value))
}
