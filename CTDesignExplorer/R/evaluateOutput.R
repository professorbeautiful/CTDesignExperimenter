cat("======== evaluateOutput.R ================\n")

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
  if(v1@checkDataType != v2@checkDataType) return(FALSE)
  return(TRUE)
}

#  PUT INTO examples... sameVar(vg_responseDoseThreshold@requirements, v_clearanceRate)

findGeneratorNames = function(var, env=parent.frame(), menu=TRUE) {
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

findGeneratorName = function(var, env=parent.frame(), menu=TRUE) {
  vgList = findGeneratorNames (var, env)
  if(length(vgList)==0) return (NULL)
  if(length(vgList)>1) {
    if(menu) vgList = vgList[menu(choices=vgList) ]
    else stop(paste("More than one VG to choose from: ", vgList))
  }
  return (vgList[[1]])  ### or get(vgList[[1]], envir=env)
}

findGenerator = function(var, env=parent.frame(), menu=FALSE) {
    get(findGeneratorName(var, env, menu), envir=env)
}
      
##To Examples:  findGeneratorName(vg_responseDoseThreshold@requirements)
##To Examples:  findGeneratorName(vg_responseDoseThreshold@provisions)

evaluateGeneratorOutput = function(generator, envir=parent.frame(), alreadyDone=list()) {
  ## First, make the generatorCode function available.
  if(!is(generator, "VariableGenerator"))
    stop("evaluateOutput: generator should be a VariableGenerator")
  environment(generator@generatorCode) = envir  #function arg
  if(length(generator@parameters) > 0)
    environment(generator@generatorCode) = 
    list2env(generator@parameters, environment(generator@generatorCode))
  ifVerboseCat("generatorCode_envir", ls(envir=environment(generator@generatorCode)))
  value = generator@generatorCode()
  #thisOutput = new("VariableValue", variable=generator@outputVariable, value=value)
  #  valueList = c(value, valueList)
  #  names(valueList)[[1]] = generator@provisions@name
  VariableValue(value, generator@provisions)
}


evaluateVNoutputs = function(vN, alreadyDone=list()) {
  ## First, make the generatorCode function available.
  envVariableValues = new.env()
  if(!is(vN, "VariableNetwork"))
    stop("evaluateVNoutputs: args should be a VariableNetwork")
  iM = incidenceMatrix(vN)
  iM = try(permuteToUpperTriangular(iM))
  if(is(iM, "try-error")) {
    cat("evaluateVNoutputs: iM try-error \n")
    browser()
  }
  for(vName in rownames(iM)) { 
    ifVerboseCat("Processing node ", vName)
    ifVerboseCat(union(vN@allRequirementNames, names(vN@provisions)))
    if(vName %in% union(vN@allRequirementNames, names(vN@provisions))) {
      ifVerboseCat("Skipping Variable ", vName)
    }   else {
      #      vg = findGenerator(vName, env=parent.frame())
      ifVerboseCat("Processing vg ", vName)
      vg = vN@vgList[[vName]]
      value = evaluateGeneratorOutput(generator=vg, envir=envVariableValues)
      provName = vg@provisions@name
      ifVerboseCat("Assigning ", value@.Data, " to ", provName)
      
      if(length(provName) > 1) stop("more than one provision name")
      assign(provName, value, envir=envVariableValues, inherits=FALSE)
    }
  }
  ifVerboseCat(ls(envir=envVariableValues))
  return(envVariableValues)
}
#


incidenceMatrix(vNexample)
permuteToUpperTriangular(..())  ### permute to upper triangular.
vNvalueEnv = evaluateVNoutputs(vNexample)
ls(env=vNvalueEnv)

printVVenv = function(env) {
  for(vv in ls(env=env)) {
    vv = get(vv, env=env)
    catn(vv@variable@name, "=", vv)
  }
}

printVVenv(vNvalueEnv)


