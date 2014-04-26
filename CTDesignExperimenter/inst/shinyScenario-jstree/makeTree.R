insertSubTree = function(insertName, insertStyle) {
  if(insertStyle=="string")
    insertName
  else {  ### extract vg components.
    vg = get(insertName)
    info = list(paste("name:", vg@outputVariable@name),
         paste("code:", paste(body(vg@generatorCode), 
                              collapse="; "))         
      )
    info = list(info)
    names(info) = insertName
    info
  }
}

makeTree = function(insertStyle="string") {
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
      scenarioTree[[thisBranchNum]], 
      insertSubTree(insertName, insertStyle))  
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
makeTree("full")
length(..())

extractEntry = function(L1=3, L2=4, start=jstree.obj(scenarioTree)) {
  nodeLevel1 = ## 3rd in scaffold
    start[["children"]][[1]][[L1]]
  ul_node = nodeLevel1[["children"]][[1]][[2]] ### unnumbered list of entries.
  nodeLevel2 = ul_node[["children"]][[1]][[L2]]
  entry = nodeLevel2[["children"]][[1]]
  entry
}
extractEntry()
