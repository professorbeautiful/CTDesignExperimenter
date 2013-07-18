clearanceRate = VariableGenerator(
  parameters=list(clearanceLocation=6,
                  clearanceSD=1),
  provisions=list("clearanceRate"),
  outputVariable=clearanceRateVariable,
  #outputName="clearanceRate", 
  ### Maybe we don't need outputName? Use the object name, right?
  generatorCode=function() {
    #We don't really need Multiplier. Just testing out the parameter idea.
    #      if(missing(Multiplier)) Multiplier = 1
    return(exp(rnorm(1, mean=log(clearanceLocation), sd=clearanceSD))
    )  
  }
)

print(evaluateOutput(clearanceRate))

######################

toxDoseThreshold  = VariableGenerator(
  parameters=list(toxLoc=1, toxSD=0.2),
  requirements=list(clearanceRate),
  provisions=list("toxDoseThreshold"), 
  outputVariable=new("Variable",
                     "toxDoseThresholdVariable",
                     description="threshold for a toxicity event",
                     dataType="numeric"),
  generatorCode=function() { 
    cat("clearanceRate:\n", clearanceRate)
    cat("\ntoxLoc:\n", toxLoc)
    cat("\ntoxSD:\n", toxSD)
    clearanceRate * 
      exp(rnorm(1, toxLoc, toxSD))
  }
)


print(evaluateOutput(toxDoseThreshold))

