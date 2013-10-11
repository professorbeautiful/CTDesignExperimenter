cat("======== scenarios.R  ================\n")

setClass("Scenario", contains="VariableNetwork",
         slots=list(popModel="PopulationModel",
                    design="DesignSpecifier",
                    outcomeModel="OutcomeModel",
                    scenarioEnv))
#  The scenarioEnv holds all parameters, variablevalues, and patient data.


### sim1CT

runScenario = function(scenario){
  sim1CT(scenario@popModel, scenario@design, scenario@outcomeModel)
}
