clearanceRate = VariableGenerator(
  parameters=list(clearanceLocation=6,
                  clearanceSD=1),
  provisions=v_clearanceRateVariable,
  #outputName="clearanceRate", 
  ### Maybe we don't need outputName? Use the object name, right?
  generatorCode=function() {
    #We don't really need Multiplier. Just testing out the parameter idea.
    #      if(missing(Multiplier)) Multiplier = 1
    return(exp(rnorm(1, mean=log(clearanceLocation), sd=clearanceSD))
    )  
  }
)

evaluateOutput(clearanceRate)
######################

responseDoseThreshold  = VariableGenerator(
  parameters=list(responseLoc=0, responseSD=0.01),
  requirements=v_clearanceRateVariable,
  provisions=v_responseDoseThreshold, 
  generatorCode=function() { 
    cat("clearanceRateVariable=", clearanceRateVariable, "\n")
    clearanceRateVariable * 
      exp(rnorm(1, responseLoc, responseSD))
  }
)


evaluateOutput(responseDoseThreshold)

v_toxDoseThreshold = new("Variable",
                         name="toxDoseThresholdVariable",
                         description="threshold for a toxicity event",
                         dataType="numeric")
toxDoseThreshold  = VariableGenerator(
  parameters=list(toxLoc=0.5, toxSD=0.1),
  requirements=v_clearanceRateVariable,
  provisions=v_toxDoseThreshold, 
  generatorCode=function() { 
    clearanceRateVariable * 
      exp(rnorm(1, toxLoc, toxSD))
  }
)

evaluateOutput(toxDoseThreshold)
