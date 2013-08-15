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
#     cat("clearanceRate:\n", clearanceRate)
#     cat("\nresponseLoc:\n", responseLoc)
#     cat("\nresponseSD:\n", responseSD, "\n")
    clearanceRate * 
      exp(rnorm(1, responseLoc, responseSD))
  }
)



toxDoseThreshold  = VariableGenerator(
  parameters=list(toxLoc=0, toxSD=0.01),
  requirements=list(clearanceRate),
  provisions=list("toxDoseThreshold"), 
  outputVariable=new("Variable",
                     "toxDoseThresholdVariable",
                     description="threshold for a toxicity event",
                     dataType="numeric"),
  generatorCode=function() { 
    #     cat("clearanceRate:\n", clearanceRate)
    #     cat("\ntoxLoc:\n", toxLoc)
    #     cat("\ntoxSD:\n", toxSD, "\n")
    clearanceRate * 
      exp(rnorm(1, toxLoc, toxSD))
  }
)

