
#' Evaluate a generator expression.
#' 
#' \code{evaluateOutput} 
#' \param generator A SimpleVariableGenerator 
#' \param input Input "requirements" list of VariableValue objects.

#setGeneric("evaluateOutput", )
removeGeneric("evaluateOutput")
evaluateOutput = function(generator, alreadyDone=list()) {
  ## Make the generatorCode function available.
  if(!is(generator, "VariableGenerator"))
    stop("evaluateOutput: generator should be a VariableGenerator")
  valueList = alreadyDone
  if(length(extractRequirements(generator)) > 0)
    for(gen in extractRequirements(generator)) {
      ###  If we have not already computed the answer,
      ###  don't do it again.
      if( ! (gen@provisions %in% names(alreadyDone)))
        valueList = c(valueList,  
                    evaluateOutput(gen, alreadyDone=valueList))
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
  names(valueList)[[1]] = generator@provisions@name
  return(valueList)
}
