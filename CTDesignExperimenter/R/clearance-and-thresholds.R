cat("======== clearance-and-thresholds.R  ================\n")


vg_clearanceRate = VariableGenerator(
  parameters=list(clearanceLocation=6,
                  clearanceSD=1),
  provisions=v_clearanceRate,
  requirements=VariableList(list()),
  #outputName="clearanceRate", 
  ### Maybe we don't need outputName? Use the object name, right?
  generatorCode=function() {
    #We don't really need Multiplier. Just testing out the parameter idea.
    #      if(missing(Multiplier)) Multiplier = 1
    return(exp(rnorm(1, mean=log(clearanceLocation), sd=clearanceSD))
    )  
  }
)

# To Examples: evaluateOutput(vg_clearanceRate)
# To Examples: 


evaluateVNoutputs(VariableNetwork(vgList=VariableGeneratorList(vg_clearanceRate)))
######################

vg_responseDoseThreshold  = VariableGenerator(
  parameters=list(responseLoc=0, responseSD=0.01),
  requirements=VariableList(v_clearanceRate),
  provisions=v_responseDoseThreshold, 
  generatorCode=function() { 
    cat("clearanceRate=", clearanceRate, "\n")
    clearanceRate * 
      exp(rnorm(1, responseLoc, responseSD))
  }
)


#evaluateOutput(vg_responseDoseThreshold)

vg_toxDoseThreshold  = VariableGenerator(
  parameters=list(toxLoc=0.5, toxSD=0.1),
  requirements=VariableList(v_clearanceRate),
  provisions=v_toxDoseThreshold, 
  generatorCode=function() { 
    clearanceRate * 
      exp(rnorm(1, toxLoc, toxSD))
  }
)

# evaluateOutput(vg_toxDoseThreshold)

vNpharm = VariableNetwork(vgList=VariableGeneratorList(
  vgList=list(
    vg_clearanceRate=vg_clearanceRate, 
    vg_responseDoseThreshold=vg_responseDoseThreshold, 
    vg_toxDoseThreshold=vg_toxDoseThreshold))
)  #### you need the names?

tempEnv = evaluateVNoutputs(vNpharm)
printNetworkOutputs = function(networkEnv) {
  for(v in ls(env=networkEnv)) {
    vv = get(v, env=networkEnv)
    catn(paste(v, vv))
  }   ### Includes parameters.
}
printNetworkOutputs(tempEnv)
