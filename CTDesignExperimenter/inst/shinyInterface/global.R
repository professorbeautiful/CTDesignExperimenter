require("CTDesignExperimenter")
require(shinysky) 
options(shiny.trace=FALSE)

scaffoldObjectNames = scaffoldObjects[[1]]

source("makeTree.R")

reloadScenario = function() {  
  scenarioTree <<- makeTree(scenario=currentScenario, "full")
  # length(scenarioTree) is 13
  myTreeObj <<- myjstree.obj(scenarioTree)  # shiny.tag
  # class attributes are added in myjstree.obj
  # length(myTreeObj[[3]][[1]]) is 13
  myTree <<- jstree("jstreeScenario", myTreeObj)  # shiny.tag.list
}
currentScenario = defaultScenario  
#reloadScenario()
  
experimentTable = data.frame(sampleSize=NA)


