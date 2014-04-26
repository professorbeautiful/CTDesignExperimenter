makeTree = function() {
  scenarioTree = 
    sapply(scaffoldObjectNames, simplify=F, function(x) list(inserts=list()))
  #    rep(list(list(inserts=character(0))), 13)
  #  as.list(scaffoldObjectNames)
 # names(scenarioTree) = scaffoldObjectNames
  # rep("inserts", length(scaffoldObjectNames)
  #                            , function(x)list(x))
  for(insertName in names(defaultScenario)) {
    insert = defaultScenario[[insertName]]
    scafBlockName = scaffoldObjects$name[
      scaffoldObjects$eventInsertSubType == insert@insertSubType]
    thisBranchNum = which(scaffoldObjectNames==scafBlockName)
    scenarioTree[[thisBranchNum]]$inserts = c(
      scenarioTree[[thisBranchNum]]$inserts, insertName)  
 }
 for(i in 1:length(scenarioTree))
   if(length(scenarioTree[[i]]$inserts)==0) 
     scenarioTree[[i]] = scaffoldObjectNames[i]
 scenarioTree
}
makeTree()
length(..())
