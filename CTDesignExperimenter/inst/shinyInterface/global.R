require("CTDesignExperimenter")
require(shinysky)
options(shiny.trace=TRUE)

scaffoldObjectNames = scaffoldObjects[[1]]

source("makeTree.R")

reloadScenario = function() {  
  scenarioTree <<- makeTree("full", currentScenario)
  # length(scenarioTree) is 13
  myTreeObj <<- myjstree.obj(scenarioTree)  # shiny.tag
  # class attributes are added in myjstree.obj
  # length(myTreeObj[[3]][[1]]) is 13
  myTree <<- jstree("jstree1", myTreeObj)  # shiny.tag.list
  
#   vgNodes = unlist( 
#     traverse(myTree, callback = 1, searchTerm = "vg_")
#     )
#   vgNodeIndices = sapply(strsplit(vgNodes, x = " "), ''
#   for(node in vgNodes) {
#     locationVector = strsplit(vgNodes)
#     myTree[[locationVector]] <-
#       tagAppendAttributes(myTreeTemp[[locationVector]], class="CLASS")
#   }
  # length(unlist(myTree)) is 192.  Very useful names! Gives depth.
  # as.vector(unlist(myTree))
}

# table(unlist(myTree)[grep("name", names(unlist(myTree)))])
# div   head     li   link script     ul 
#   1      3     60      1      3     18 
#unlist(myTree)[which(unlist(myTree) == "link") + (0:3)]
#unlist(myTree)[which(unlist(myTree) == "") + (0:3)]

# 
#  unlist(myTree)[(grep("(ec|vg)_", unlist(myTree)))]  ### 11 vg or ec.
## All are children.children.children.children.children
#  unlist(myTree)[(grep("v_", unlist(myTree)))]  #NONE.
# Using opm:traverse

# traverse = function(li, func) {
#   if(is.list(li) return(lapply()))
# }

## Start with current Scenario.
currentScenario = defaultScenario  
reloadScenario()

experimentTable = data.frame(sampleSize=NA)
addScenarioToExperiment = function(scenario){
  experimentTable[nrow(experimentTable)+1, ] = NA
  rownames(experimentTable) [nrow(experimentTable)] = scenario@name
}

