require("CTDesignExperimenter")
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

