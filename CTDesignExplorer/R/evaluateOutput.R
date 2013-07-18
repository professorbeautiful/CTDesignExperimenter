
#' Evaluate a generator expression.
#' 
#' \code{evaluateOutput} 
#' \param generator A SimpleVariableGenerator 
#' \param input Input "requirements" list of VariableValue objects.

evaluateOutput = function(generator) {
  ## Make the generatorCode function available.
  if(!is(generator, "VariableGenerator"))
    stop("evaluateOutput: generator should be a VariableGenerator")
  valueList = list()
  if(length(extractRequirements(generator)) > 0)
    for(gen in extractRequirements(generator)) {
      valueList = c(valueList,  evaluateOutput(gen))
#       for(vv in genOutput) {
#         genOutputList = list(vv@value)
#        names(genOutputList) = gen@provisions
#         names(genOutputList) = gen@provisions
    }
  if(length(valueList) > 0)
    environment(generator@generatorCode) = 
    list2env(valueList, environment(generator@generatorCode))
  print(ls(envir=environment(generator@generatorCode)))
  value = generator@generatorCode()
  #thisOutput = new("VariableValue", variable=generator@outputVariable, value=value)
  valueList = c(value, valueList)
  names(valueList)[[1]] = generator@provisions[[1]]
  return(valueList)
}
