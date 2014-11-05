### ShowVariableNetworks



vgList = VariableGeneratorList(getVGs(currentScenario@inserts, 
                                      "DesignParameter"))
designVN = VariableNetwork(vgList=vgList)

designVN@vgList
designVN@  allProvisions="list",
designVN@           allProvisionNames
designVN@           provisionMap
designVN@           allRequirements
designVN@           allRequirementNames
designVN@           requirementMap
designVN@           requirementMatrix="ANY",
designVN@           howManyNeedMe
designVN@           candidates


candidateCounts = apply(designVN@candidates, 1, sum)

clashingVariables = names(candidateCounts)[candidateCounts > 1]

unsatisfiedVariables = designVN@requirements  ### requirements not satisfied internally
#check
identical(names(unsatisfiedVariables), names(candidateCounts)[candidateCounts == 0]

