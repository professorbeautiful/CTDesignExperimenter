
#' Evaluate a generator expression.
#' 
#' \code{evaluateOutput} 
#' \param generator A SimpleVariableGenerator 
#' \param input Input "requirements" list of VariableValue objects.

#setGeneric("evaluateOutput", )
#removeGeneric("evaluateOutput")

sameVar = function(v1, v2) {
  if(!is(v1, "Variable")) stop(paste0("sameVar: v1 should be a Variable", v1))
  if(!is(v2, "Variable")) stop(paste0("sameVar: v2 should be a Variable", v2))
  if(v1@name != v2@name) return(FALSE)
  if(v1@description != v2@description) return(FALSE)
  if(v1@dataType != v2@dataType) return(FALSE)
  return(TRUE)
}

sameVar(responseDoseThreshold@requirements, clearanceRateVariable)

findGeneratorNames = function(var, env=.GlobalEnv, menu=TRUE) {
  # perhaps Variable should be a virtual Class, and 
  # each Variable a subClass.
  # VariableGenerator an instance of a subClass.
  # In the meantime...
  vgList = ls(envir=env)[
    sapply(ls(envir=env), function(obname) {
      ob = get(obname, envir=env)
      if(is(ob, "VariableGenerator"))
        if(sameVar(var, ob@provisions))
          TRUE else FALSE
      else FALSE
    })]
  return(vgList)
}

findGeneratorName = function(var, env=.GlobalEnv, menu=TRUE) {
  vgList = findGeneratorNames (var, env)
  if(length(vgList)==0) return (NULL)
  if(length(vgList)>1) {
    if(menu) vgList = vgList[menu(choices=vgList) ]
    else stop(paste("More than one VG to choose from: ", vgList))
  }
  return (vgList[[1]])  ### or get(vgList[[1]], envir=env)
}

findGenerator = function(var, env=.GlobalEnv, menu=FALSE) {
    get(findGeneratorName(var, env, menu), envir=env)
}
      
findGeneratorName(responseDoseThreshold@requirements)
findGeneratorName(responseDoseThreshold@provisions)


evaluateOutput = function(generator, envir=.GlobalEnv, alreadyDone=list()) {
  ## First, make the generatorCode function available.
  if(!is(generator, "VariableGenerator"))
    stop("evaluateOutput: generator should be a VariableGenerator")
  valueList = alreadyDone
  reqs = extractRequirements(generator)
  if(length(reqs) > 0) {
    if(is(reqs, "Variable")) reqs = list(reqs)
    for(req in reqs) {
      ###  If we have not already computed the answer,
      ###  don't do it again.
      if( ! (req@name %in% names(alreadyDone))) {
        gen = findGenerator(req)
        valueList = c(valueList,
                      evaluateOutput(gen, envir=env, alreadyDone=valueList))
        #       for(vv in genOutput) {
        #         genOutputList = list(vv@value)
        #        names(genOutputList) = gen@provisions
        #         names(genOutputList) = gen@provisions
      }
    }
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
debug(evaluateOutput)
evaluateOutput(responseDoseThreshold)


