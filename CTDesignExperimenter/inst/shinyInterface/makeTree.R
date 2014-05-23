insertVGSubTree = function(insertName, insertStyle="simple") {
  vg = get(insertName)
  if(insertStyle=="name-only")
    return(insertName)
  else if(insertStyle=="simple"){  ### extract vg components.
    info = list(paste("outputname:", vg@outputVariable@name),
         paste("code:", paste(body(vg@generatorCode), 
                              collapse="; "))         
      )
    info = list(info)
    names(info) = insertName
    return(info)    
  } else { ## show variables and parameters
    outputVarInfo = 
      paste("output:", capture.output(vg@outputVariable))
    codeInfo = paste("code:", 
                     gsub(fixed=TRUE, "{;", "{",
                          paste(printFunctionBody(vg@generatorCode), 
                                     collapse="; ")))
    info = list(outputVarInfo, codeInfo)
    if(length(vg@parameters > 0)){
      parameterInfo = vg@parameters #sapply(vg@parameters, printParameter)
      info = c(info, parameterInfo)
    }
    info = list(info)
    names(info) = insertName
    return(info)    
  }
}

makeTree = function(insertStyle="simple", 
                    scenario=defaultScenario) {
  scenarioTree = 
    sapply(scaffoldObjectNames, simplify=F, function(x) list())
  scenarioMap = data.frame(insertName=names(scenario@inserts))
  rownames(scenarioMap) = scenarioMap$insertName
  scenarioMap$blockIndex = NA
  scenarioMap$insertIndex = NA
  #    rep(list(list(inserts=character(0))), 13)
  #  as.list(scaffoldObjectNames)
  # names(scenarioTree) = scaffoldObjectNames
  # rep("inserts", length(scaffoldObjectNames)
  #                            , function(x)list(x))
  for(insertName in names(scenario@inserts)) {
    insert = scenario@inserts[[insertName]]
    blockIndex = which(scaffoldObjects$eventInsertSubType == insert@insertSubType)
    scenarioMap[insertName, "blockIndex"] = blockIndex
    scafBlockName = scaffoldObjects$name[blockIndex]
    thisBranchNum = which(scaffoldObjectNames==scafBlockName)
    cat(thisBranchNum, " ")
    scenarioTree[[thisBranchNum]] = c(
      scenarioTree[[thisBranchNum]], 
      insertVGSubTree(insertName, insertStyle)) 
    scenarioMap[insertName, "insertIndex"] = length(scenarioTree[[thisBranchNum]])
    
  }
  for(i in 1:length(scenarioTree)){
    if(length(scenarioTree[[i]])==0) 
      scenarioTree[[i]] =  " (0)"
    else
      names(scenarioTree)[[i]] = paste(names(scenarioTree)[[i]], 
                                       " (", length(scenarioTree[[i]]), ")")
  }
  print(attributes(scenarioTree))
  assign("scenarioMap", scenarioMap, pos=1)
  attributes(scenarioTree) = c(attributes(scenarioTree), scenarioMap=scenarioMap)
  scenarioTree
}


extractEntry = function(L1=3, L2=4, start=jstree.obj(scenarioTree)) {
  nodeLevel1 = ## 3rd in scaffold
    start[["children"]][[1]][[L1]]
  ul_node = nodeLevel1[["children"]][[1]][[2]] ### unnumbered list of entries.
  nodeLevel2 = ul_node[["children"]][[1]][[L2]]
  entry = nodeLevel2[["children"]][[1]]
  entry
}


# extractEntry()[[1]]
# Validation of scenarioMap:
# for(vg in rownames(scenarioMap) )
#   catn(vg, extractEntry(scenarioMap[vg, "blockIndex"], scenarioMap[vg, "insertIndex"])[[1]])



# The following is wrong, and probably unnecessary.
# extractEntryNew = function(L=c(3,4), start=jstree.obj(scenarioTree)) {
#   nodeLevel = start[["children"]][[1]]
#   if(length(L) == 0) 
#     return(nodeLevel)
#   return(extractEntryNew(L[-1], 
#                          nodeLevel[[ L[1] ]] [["children"]][[1]] ))
# }