require("CTDesignExperimenter")
options(shiny.trace=TRUE)

scaffoldObjectNames = scaffoldObjects[[1]]

source("makeTree.R")

reloadScenario = function() {  
  require(shinysky)
  scenarioTree <<- makeTree("full", currentScenario)
  myTreeObj <<- jstree.obj(scenarioTree)
  myTree <<- jstree("jstree1", myTreeObj)
}


## Start with current Scenario.
currentScenario = defaultScenario  
reloadScenario()

experimentTable = data.frame(sampleSize=NA)
addScenarioToExperiment = function(scenario){
  experimentTable[nrow(experimentTable)+1, ] = NA
}

