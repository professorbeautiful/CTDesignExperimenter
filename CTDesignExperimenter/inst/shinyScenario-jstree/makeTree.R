makeTree = function() {
  scenarioTree = 
    sapply(scaffoldObjectNames, simplify=F, function(x) list())
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
    scenarioTree[[thisBranchNum]] = c(
      scenarioTree[[thisBranchNum]], insertName)  
 }
 for(i in 1:length(scenarioTree)){
   if(length(scenarioTree[[i]])==0) 
     scenarioTree[[i]] = paste(scaffoldObjectNames[i], " (0)")
   else
     names(scenarioTree)[[i]] = paste(names(scenarioTree)[[i]], 
                                      " (", length(scenarioTree[[i]]), ")")
 }
 scenarioTree
}
makeTree()
length(..())
