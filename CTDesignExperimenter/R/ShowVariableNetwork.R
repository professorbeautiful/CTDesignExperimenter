### ShowVariableNetworks



vgList = VariableGeneratorList(getVGs(currentScenario@inserts, 
                                      "DesignParameter"))
designVN = VariableNetwork(vgList=vgList)

candidateCounts = apply(designVN@candidates, 1, sum)

clashingVariables = names(candidateCounts)[candidateCounts > 1]
satisfiedVariables = names(candidateCounts)[candidateCounts == 1]

unsatisfiedVariables = designVN@requirements  ### requirements not satisfied internally
#check
identical(names(unsatisfiedVariables), names(candidateCounts)[candidateCounts == 0]

