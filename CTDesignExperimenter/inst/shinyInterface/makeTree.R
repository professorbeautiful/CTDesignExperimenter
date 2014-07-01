insertVGSubTree = function(insertName, insertStyle="simple") {
  vg = get(insertName)
  if(insertStyle=="name-only")
    return(insertName)
  else if(insertStyle=="simple"){  ### extract vg components.
    info = list(paste("outputname:", vg@outputVariable@name),
         paste(":", paste(body(vg@generatorCode), 
                              collapse="; "))         
      )
    info = list(generator=info)
    names(info) = insertName
    return(info)    
  } else { ## show variables and parameters
    needed = sapply(vg@requirements, 
                          function(v)paste0("needs:",
                                           capture.output(v)))
    #names(needed) = "needed"
    info = as.list(needed)
    codeInfo = paste("code:", 
                     gsub(fixed=TRUE, "{;", "{",
                          paste(printFunctionBody(vg@generatorCode), 
                                     collapse="; ")))
    info = c(info, generator=codeInfo)
    outputVarInfo = 
      paste0("provides:", capture.output(vg@outputVariable))
    info = c(info, list(outputVarInfo))
    if(length(vg@parameters > 0)){
      parameterInfo = #vg@parameters 
        paste0("param: ", names(vg@parameters), "=", unlist(vg@parameters))
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
#     if(length(scenarioTree[[i]])==0) 
#       scenarioTree[[i]] =  " (0)"
#     else
      names(scenarioTree)[[i]] = paste(names(scenarioTree)[[i]], 
                                       " (", length(scenarioTree[[i]]), ")")
#       if(length(scenarioTree[[i]])==0)
#         scenarioTree[[i]] [[1]] = "NO INSERTS HERE"
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

# extractEntry()[[1]]   #  responseDoseThreshold
# Validation of scenarioMap:
# for(vg in rownames(scenarioMap) )
#   catn(vg, extractEntry(scenarioMap[vg, "blockIndex"], scenarioMap[vg, "insertIndex"])[[1]])
# OK.

findInsertInScenario = function( L1=3, L2=4, scenario) {
  if(missing(scenario)) {
    loadLatestScenario()
    scenario = latestScenario
  }
  blockName = scaffoldObjectNames[L1]
  insertSubType = scaffoldInsertSubTypes[L1]
  if( insertSubType == "")
    stop("findInsertInScenario: insertSubType not found")
  insertCount = 0
  for(insertNum in seq(along=scenario@inserts)) {
    if(scenario@inserts[[insertNum]]@insertSubType == insertSubType) {
      insertCount = insertCount + 1
      if(insertCount == L2)
        return(scenario@inserts[[insertNum]])
    }
  }
  return(NULL)
}
# findInsertInScenario()

myjstree.obj = 
function (x, addLevelClass=TRUE, addLevelType=TRUE, addIndex=TRUE, level=0, index="0") 
{
  handle <- function(ind, theList, level) {
    name <- names(theList)[[ind]]
    if (!is.null(name)) {
      level=level+1
      index = paste0(index, "_", ind)
      a <- tags$li(list(name, 
                        myjstree.obj(theList[[ind]], 
                                     addLevelClass=addLevelClass,
                                     addLevelType=addLevelType,
                                     addIndex=addIndex,
                                     level=level,
                                     index=index
                        )))
    }
    else {
      a <- tags$li(theList[[ind]])
    }
    ### This line is added to shinysky:::jstree.obj
    if(addLevelClass)
      a <- tagAppendAttributes(a, class=paste0("treeclass_", level))
    if(addLevelType) {
      a <- tagAppendAttributes(a, type=paste0("level_", level))
      a <- tagAppendAttributes(a, rel=paste0("level_", level))
    }
    if(addIndex)
      a <- tagAppendAttributes(a, index=index)
    # using the jstree plugin "types".
    #a <- tagAppendAttributes(a, `data-jstree`='{icon:"www/BLOCK.gif"}')
    # direct approach doesnt work either
    return(a)
  }
  if (is.list(x)) {
    ind <- seq(along=x)
    res <- lapply(ind, handle, x, level=level)
    return(tags$ul(res))
  }
  else {
    x
  }
}
