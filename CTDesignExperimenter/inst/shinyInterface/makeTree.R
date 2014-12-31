insertVGSubTree = function(insert, insertStyle="full") {
  if(insertStyle=="simple"){  ### extract vg components.
    info = list(paste("outputname:", vg@outputVariable@name),
         paste(":", paste(body(insert@generatorCode), 
                              collapse="; "))         
      )
    #info = list(generator=info)
    info = list(info)
    names(info) = insert@filename
    return(info)    
  } else { ## "full" ---  show variables and parameters
    needed = sapply(insert@requirements, 
                          function(v)paste0("needs:",
                                           capture.output(v)))
    #names(needed) = "needed"
    info = as.list(needed)
    codeInfo = paste("code:", 
                     gsub(fixed=TRUE, "{;", "{",
                          paste(printFunctionBody(insert@generatorCode), 
                                     collapse="; ")))
    codeInfo = gsub('"', '\\\\"', codeInfo)
    
    #info = c(info, generator=codeInfo)
    info = c(info, codeInfo)
    outputVarInfo = 
      paste0("provides:", capture.output(insert@outputVariable))
    info = c(info, list(outputVarInfo))
    if(length(insert@parameters > 0)){
      parameterInfo = #insert@parameters 
        paste0("param: ", names(insert@parameters), "=", unlist(insert@parameters))
      info = c(info, parameterInfo)
    }
    info = list(info)
    names(info) = insert@filename
    return(info)    
  }
}

makeTree = function(scenario=defaultScenario, insertStyle="full") {
  if(is.null(names(scenario@inserts))) 
    names(scenario@inserts@.Data) = paste0("vg_", 1:length(scenario@inserts)) 
  ##  This is a bizarre aspect of S4 classes.
  ##  As soon as you assign an attribute to a .Data slot,
  ##  it is removed and reattached to the parent instance!
  ##  Ah, true of names attribute and other arbitrary attributes, 
  ##  but not true of length attribute, and (probably)  dim attribute.
  ## Thus, after the previous assignment, names(scenario@inserts@.Data) is still NULL,
  ## but names(scenario@inserts)  has been set!
  
  for(iInsert in seq(along=scenario@inserts)) {
    if(names(scenario@inserts)[[iInsert]] == "") {
      if(scenario@inserts[[iInsert]]@filename == "") 
        names(scenario@inserts)[[iInsert]] = scenario@inserts[[iInsert]]@filename
      else
        names(scenario@inserts)[[iInsert]] = paste0("vg_", iInsert)
    }
  }
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
    blockIndex = try(which(scaffoldObjects$eventInsertSubType == insert@insertSubType))
    if(class(blockIndex) == 'try-error')
      browser("makeTree blockIndex error")
    scenarioMap[insertName, "blockIndex"] = blockIndex
    scafBlockName = scaffoldObjects$name[blockIndex]
    thisBranchNum = which(scaffoldObjectNames==scafBlockName)
    #cat(thisBranchNum, " ")
    scenarioTree[[thisBranchNum]] = c(
      scenarioTree[[thisBranchNum]], 
      insertVGSubTree(insert, insertStyle)) 
    names( scenarioTree[[thisBranchNum]]) [length( scenarioTree[[thisBranchNum]])] =
      insertName
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
  # print(attributes(scenarioTree))
  assign("scenarioMap", scenarioMap, pos=1)
  attributes(scenarioTree) = c(attributes(scenarioTree), scenarioMap=scenarioMap)
  scenarioTree
}

# debug(makeTree)

extractTreeText = function(L1=3, L2=4, L3=1, start=myTreeObj) {
  nodeLevel1 = ## 3rd in scaffold
    start[["children"]][[1]][[L1]]
  ul_node = nodeLevel1[["children"]][[1]][[2]] ### unnumbered list of entries.
  nodeLevel2 = ul_node[["children"]][[1]][[L2]]
  entry = nodeLevel2[["children"]][[1]]
  if(is.null(L3)) return(entry)
  ul_node = entry[[2]]
  nodeLevel3 = ul_node[["children"]] [[1]] [[L3]]
  return(nodeLevel3[["children"]] [[1]] [[2]])
}

# extractTreeText(L3=NULL)[[1]]   #  Correct answer: responseDoseThreshold
# extractTreeText()   #  Correct answer: "needs:  clearanceRate  ( is.double(x) & (x >= 0) )"

# extractTreeObject = function(L1=3, L2=4, L3=1, start=myTreeObj) {
#   treeText = extractTreeText(L1,L2,L3,start)
#   if(is.null(L3))
#   return()
# }

# Validation of scenarioMap:
# for(vg in rownames(scenarioMap) )
#   catn(vg, extractEntry(scenarioMap[vg, "blockIndex"], scenarioMap[vg, "insertIndex"])[[1]])
# OK.

findObjectInScenario = function( index="0_3_4_1", scenario) {
  levels = strsplit(index, split="_") [[1]] [-1]
  L1 = as.numeric(levels[1])
  L2 = as.numeric(levels[2])
  L3 = as.numeric(levels[3])
  if(missing(scenario)) {
    loadLatestScenario()
    scenario = latestScenario
  }
  blockName = scaffoldObjectNames[L1]
  insertSubType = scaffoldInsertSubTypes[L1]
  if( insertSubType == "")
    stop("findInsertInScenario: insertSubType not found")
  ## The following works ONLY if the scenario insert order within block is not changed.
  insertCount = 0
  for(insertNum in seq(along=scenario@inserts)) {
    if(scenario@inserts[[insertNum]]@insertSubType == insertSubType) {
      insertCount = insertCount + 1
      if(insertCount == L2) {
        theInsert = scenario@inserts[[insertNum]]
        if(is.na(L3))  ### It's an insert
          return(theInsert)
        else {
          nodeText = makeTree(currentScenario)[[L1]][[L2]][[L3]] 
          #  NO LONGER WORKS:  extractTreeText(L1, L2, L3, makeTree(scenario))
          #           slotOrder = cq(needs="requirements", code="generatorCode",
          #                          provides="outputVariable", param="parameters")
          #           objectList = c(list(),theInsert@requirements, 
          #                          gsub('"', '\\"', body(theInsert@generatorCode)),
          #                          theInsert@outputVariable, theInsert@parameters)
          #           return(objectList[[L3]])
          if(length(grep("^code:", nodeText)) == 1)
            return(theInsert@generatorCode)
          else if(length(grep("^provides:", nodeText)) == 1)
            return(theInsert@outputVariable)
          else if(length(grep("^needs:", nodeText)) == 1) {
            #             whichOne = match(gsub("needs: ", "", nodeText),
            #                              lapply(theInsert@requirements, capture.output))
            #             # return(theInsert@requirements[[whichOne]])
            return(theInsert@requirements[[L3]])  ### Needs come first!
          }
          else if(length(grep("^param:", nodeText)) == 1) {
            paramName = gsub("=.*", "", 
                             gsub("param: ", "", nodeText) )
            whichOne = match(paramName, names(theInsert@parameters) )
            return(theInsert@parameters[whichOne])              
          }
        }
        return(NULL)
      }
    }
  }
}

# findObjectInScenario("0_3_4")
# findObjectInScenario("0_3_4_1")
# findObjectInScenario("0_3_4_2")

myjstree.obj = 
  function (x, prefix="", addLevelClass=TRUE, addLevelType=TRUE, addIndex=TRUE, level=0, index="0") 
  {
    handle <- function(ind, theList, level) {
      name <- names(theList)[[ind]]
      if (!is.null(name)) {
        level=level+1
        index = paste0(prefix, index, "_", ind)
        a <- tags$li(list(name, 
                          myjstree.obj(theList[[ind]], 
                                       prefix=prefix,
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
        a <- tagAppendAttributes(a, class=paste0(prefix, "treeclass_", level))
      if(addLevelType) {
        a <- tagAppendAttributes(a, type=paste0(prefix, "level_", level))
        a <- tagAppendAttributes(a, rel=paste0(prefix, "level_", level))
      }
      if(addIndex)
        a <- tagAppendAttributes(a, index=index)
      
      ### Adaptation for shinyTree 
      ## a <- tagAppendAttributes(a, `data-jstree`='{&quot;opened&quot;: true}')

      # using the jstree plugin "types".
      #a <- tagAppendAttributes(a, `data-jstree`='{icon:"www/BLOCK.gif"}')
      # direct approach doesnt work either
      return(a)
    }  ## end of handle()
    if (is.list(x)) {
      ##  attr(x, 'stopened') = TRUE
      ind <- seq(along=x)
      res <- lapply(ind, handle, x, level=level)
      return(tags$ul(res))
#       return(tagAppendAttributes(tags$ul(res), 
#                                  `data-jstree`='{&quot;opened&quot;: true}'))
    }
    else {
      x
    }
  }


