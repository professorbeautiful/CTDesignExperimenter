smoketest = function(){
  
  print(evaluateOutput(clearanceRate))
  print(evaluateOutput(responseDoseThreshold))
	print(evaluateOutput(toxDoseThreshold))

	getPopModelOutputs(
	  new("PopModel", requirements=list(responseDoseThreshold, toxDoseThreshold))
	)
	print(allRequirements(toxDoseThreshold))  ### OK 
	print(extractRequirements(toxDoseThreshold))

	popModel = new("PopModel", requirements=list(responseDoseThreshold, toxDoseThreshold))
	print(lapply(popModel@requirements, `@`, "provisions" ))
	print(class(popModel@requirements[[1]]))  ### VariableGenerator
	  ### Should it be Variable, not VariableGenerator?
}
