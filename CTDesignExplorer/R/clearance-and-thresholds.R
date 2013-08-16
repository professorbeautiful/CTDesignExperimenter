vg_clearanceRate = VariableGenerator(
  parameters=list(clearanceLocation=6,
                  clearanceSD=1),
  provisions=v_clearanceRate,
  #outputName="clearanceRate", 
  ### Maybe we don't need outputName? Use the object name, right?
  generatorCode=function() {
    #We don't really need Multiplier. Just testing out the parameter idea.
    #      if(missing(Multiplier)) Multiplier = 1
    return(exp(rnorm(1, mean=log(clearanceLocation), sd=clearanceSD))
    )  
  }
)

evaluateOutput(vg_clearanceRate)
######################

vg_responseDoseThreshold  = VariableGenerator(
  parameters=list(responseLoc=0, responseSD=0.01),
  requirements=v_clearanceRate,
  provisions=v_responseDoseThreshold, 
  generatorCode=function() { 
    cat("clearanceRate=", clearanceRate, "\n")
    clearanceRate * 
      exp(rnorm(1, responseLoc, responseSD))
  }
)


evaluateOutput(vg_responseDoseThreshold)

v_toxDoseThreshold = new("Variable",
                         name="toxDoseThreshold",
                         description="threshold for a toxicity event",
                         dataType="numeric")
vg_toxDoseThreshold  = VariableGenerator(
  parameters=list(toxLoc=0.5, toxSD=0.1),
  requirements=v_clearanceRate,
  provisions=v_toxDoseThreshold, 
  generatorCode=function() { 
    clearanceRate * 
      exp(rnorm(1, toxLoc, toxSD))
  }
)

evaluateOutput(vg_toxDoseThreshold)
