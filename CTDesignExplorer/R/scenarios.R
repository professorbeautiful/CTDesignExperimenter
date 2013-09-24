cat("======== scenarios.R  ================\n")

setClass("Scenario", contains="VariableNetwork",
         slots=list(popModel="PopulationModel",
                    design="DesignSpecifier",
                    outcomeModel="OutcomeModel"))

### sim1CT

runScenario = function(scenario){
  sim1CT(scenario@popModel, scenario@design, scenario@outcomeModel)
}
