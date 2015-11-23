cat("=========== VariableNetwork.R ===========\n")

#' ListOfInserts.class, a class extending "list".
#' 
#' A list of Insert objects.
#' Generally the parameters are the defaults.
#' When a new Scenario is defined, the parameters may be changed.
#' 
#' This will need modification to include EventGenerators
setClass("ListOfInserts", contains="list")

validity_ListOfInserts = function(object) {
  if(length(object)==0) return(TRUE)
  whichAreInserts = sapply(object, is, "Insert")
  if(all(whichAreInserts)) return(TRUE)
  return(paste("ERROR in ListOfInserts: ", which(!whichAreInserts)))
}
setValidity(Class="ListOfInserts", validity_ListOfInserts)

setClass("VariableGeneratorList", contains="ListOfInserts")
###  Might contain event generators.


# We could assign the return value of setClass to a constructor,
# and define Validity, as in the following commented-out code.
# However, we prefer to handle it so that a call to the
# constructor with an args that is a single VG *not* in a list
# will not fail.

# ListOfInsertsValidity = function(object){
#   for(vg in object@.Data){
#     if(!is(vg, "VariableGenerator"))
#       return("ListOfInserts: "
#              %&% "validity error: not all "
#              %&% " components are VariableGenerator")
#   }
#   return(TRUE)
# }
# setValidity("ListOfInserts", ListOfInsertsValidity) 

#' ListOfInserts.constructor
#' 
#' @param vgList  A list of VariableGenerator objects, or a single VariableGenerator. 
#' @return  A ListOfInserts, validated as to the components
ListOfInserts = function(vgList=NULL) {
  if(is.null(vgList)) return(new("ListOfInserts", list()))
  theNames = names(vgList) ## You must assign names to the list.
  if(is.null(theNames))
    theNames = paste0("vg", 1:length(vgList))
  if(is(vgList, "VariableGenerator"))
    vgListObj = new("ListOfInserts", list(vgList))
  else
    vgListObj = new("ListOfInserts", vgList)
  ifVerboseCat(names(vgListObj))
  names(vgListObj@.Data) = theNames
  ### validity
  for(vg in vgListObj@.Data){
    if(!is(vg, "VariableGenerator"))
      stop("ListOfInserts: "
           %&% "validity error: not all "
           %&% " components are VariableGenerator")
  }
  vgListObj
}

VariableGeneratorList = ListOfInserts

setClass("VariableNetwork", contains="Specifier",
         slots=list(vgList="ListOfInserts",
                    allProvisions="list",
                    allProvisionNames="character",
                    provisionMap="list",
                    allRequirements="list",
                    allRequirementNames="character",
                    requirementMap="list",
                    requirementMatrix="ANY",
                    howManyNeedMe="ANY",
                    candidates="ANY")
)


getRequirementNames = function(vg){ 
  if(is.null(vg@requirements)) return(NULL)
  if(is(vg@requirements, "Variable"))
    return(list(vg@requirements))
  ## Why as.vector() is SOMETIMES needed (to strip the name(s)) is a huge puzzle.
  as.vector(sapply(vg@requirements, slot, name="name"))
}

#' getReqsOrProvs
#' 
#' Returns the names of the variables required or provided by this VG.
#' 
#' @param vg Variable Generator.
#' @param slotName Either "requirements" or "provisions"
#' 
getReqsOrProvs = function(vg, slotName=c("requirements", "provisions"))  {
  if(is(vg, 'Scenario')) vg = vg@inserts
  if(is(vg, class2 = 'list') )
    return(
      ldply(lapply(vg, getReqsOrProvs, slotName=slotName))
      )
  contents = slot(vg, slotName)
  if(length(contents) == 0) return( NULL)
  else if(class(contents) == "Variable") 
    names = contents@name
  else if(class(contents) == "VariableList") {
    names = sapply(contents,  slot, "name")
  }
  else browser()
  #         withNames(vg@requirements, 
  #               rep(vg@name, length(vg@requirements)))
  returnval = data.frame(name=names, vgName=vg@filename)
  return(returnval)
}

VariableNetwork = function(vgList=NULL, varNetworkList=NULL){
  cat("VariableNetwork REVISED\n")
  if(is.null(vgList)) vgList = ListOfInserts()
  if(is(vgList, "VariableGenerator")) vgList = ListOfInserts(vgList)
  if(is(vgList, "list")) vgList = ListOfInserts(vgList)
  if(!is.null(varNetworkList)) {
    if(is(varNetworkList, "VariableNetwork")) varNetworkList = list(varNetworkList)
    for(vN in varNetworkList) {
      if(!is(vN, "VariableNetwork")) 
        stop("varNetworkList should be a list of VariableNetworks")
      vgList = c(vgList, vN@vgList@.Data)
    }
    if(any(duplicated(names(vgList)))) 
      browser("VariableNetwork: all vg names must be unique")
    vgList = ListOfInserts(vgList=vgList)
  }
  network = new("VariableNetwork", vgList=vgList,
                timestamp=Sys.time(),
                author=Sys.getenv("USER")
  )
  
#   provisions = c(sapply(vgList, function(vg) unlist(vg@provisions)))
#   provisions = provisions[!sapply(provisions, is.null)]
#   if(!isTRUE(all(sapply(provisions, class) == "Variable")))
#     browser("VariableNetwork: not all provisions are variables")
#   provisions = unique(provisions)   
#   provisions = new("VariableList", provisions)
#   if(length(provisions) == 0) provisions = NULL
#   network@provisions = provisions  ###### to provide an outside view
#   vgNames = names(vgList)
#   #  names(network@vgList) = vgNames ### no such slot
# 
#   getProvisionNames = function(variables) {
#     if(is(variables, "Variable")) return(variables@name)
#     if(is(variables, "VariableList")) 
#       return(sapply(variables,
#                     slot, name="name"))
#     NULL
#   }
#   allProvisionNames = sapply(provisions, 
#                              getProvisionNames)
#   names(allProvisionNames) = rep(
#     vgNames, 
#     times=sapply(vgList, 
#                 function(vg) length(vg@provisions)))
#   allProvisions = sapply(network@vgList,
#                          function(vg) vg@provisions)
#   names(allProvisions) = names(allProvisionNames)
#   provisionMap = as.list(allProvisionNames)
  #browser()
#   provisionMap = sapply(network@vgList, 
#     function(vg) {
#       if(is(vg@provisions, "Variable")
#         vg@provisions@name
#       else
#         sapply(vg@provisions@.Data, slot, name="name"))
#   #   provisionEdges = data.frame(VarGen=names(provisionMap), Variable=provisionMap,
  #                               stringsAsFactors=FALSE)
  provisionMapList = lapply(vgList, getReqsOrProvs, slot="provisions")  ### what if there are no reqs?.
  provisionMap = Reduce(rbind, provisionMapList)
  ## This will create a single data.frame with names "name" and "vgName."
  #  browser(expr=(length(vgList)==5))
  if(length(provisionMapList) > 0)
    names(provisionMapList) = names(provisionMapList)
  allProvisions = lapply(vgList, slot, name="provisions")
  allProvisionNames = provisionMap[, "name"]
  names(allProvisionNames) =  provisionMap[, "vgName"]
  allProvisions = Reduce( c, lapply(vgList, slot, name="provisions"))
  if(length(allProvisions) > 1) {
    for(r in seq(along=allProvisions))
      attr(allProvisions[[r]]@checkDataType, 'srcref') <- NULL
    ## You have to remove this srcref attribute for 'identical' to work in unique.list().
    allProvisions = unique_list(allProvisions)
    ## each provision counted only once.
    allProvisionNames = sapply(allProvisions, function(req)req@name)
  }  
  ## but if 2 different Provision variables have the same name, we need to know that. 
  
  uniqueProvisionNames = unique(allProvisionNames)
  if(length(uniqueProvisionNames) != length(allProvisionNames)) {
    browser("length(uniqueProvisionNames) != length(allProvisionNames")
  }
  network@provisions = allProvisions
  network@allProvisionNames = allProvisionNames
  network@provisionMap = provisionMap
  
  
  #############  
  ### TODO:  deal with the no-reqs case.
  requirementMapList = lapply(vgList, getReqsOrProvs, slotName="requirements") 
  requirementMapList = Filter(Negate(is.null), requirementMapList)
  if(length(uniqueRequirementNames) == 0) {
    candidates = list()
    requirementMatrix = matrix(nrow=0, ncol=0)
    allRequirements=list()
    allRequirementNames=character(0)
    requirementMatrix = matrix(nrow=0,ncol=0)
    howManyNeedMe = numeric(0)
    candidates = matrix(nrow=0,ncol=0)
    howManyNeedMe = requirementMatrix
    candidates = list()
    candidateCounts = numeric(0)
  } else {
    
    allRequirements = Filter(Negate(is.null), allRequirements)
    ### what if there are no reqs?.
    requirementMap = Reduce(rbind, requirementMapList)
    ## This will create a single data.frame with names name and vgName.
    #  browser(expr=(length(vgList)==5))
    if(length(requirementMapList) > 0)
      names(requirementMapList) = names(vgList)
    allRequirements = lapply(vgList, slot, name="requirements")
    allRequirementNames = requirementMap[, "name"]
    if(length(allRequirements) > 1)
      for(r in seq(along=allRequirements))
        attr(allRequirements[[r]]@checkDataType, 'srcref') <- NULL
    ## You have to remove this srcref attribute for 'identical' to work in unique_list().
    uniqueRequirements = unique_list(allRequirements)
    ## each requirement counted only once.
    uniqueRequirementNames = sapply(allRequirements, 
                                    function(req)
                                      ifelse(is.null(req),
                                             NULL, req@name))
    
    ## but if 2 different requirement variables have the same name, we need to know that. 
    
    uniqueRequirementNames = unique(allRequirementNames)
    if(length(uniqueRequirementNames) != length(allRequirementNames)) {
      browser("length(uniqueRequirementNames) != length(allRequirementNames")
    }
    #  requirementMatrix = outer(vN@vgList, vN@allRequirements, isRequiredHere)
    # outer( uniqueRequirementNames, names(vgList), `%in%`)
    
    
    #    if(is.null(allRequirements) | length(allRequirements) == 0) {
    
    requirementMatrix = matrix(FALSE, nrow=length(uniqueRequirementNames), 
                               ncol=length(vgNames))
    #colnames(requirementMatrix) = sub('^(I_[^_]*)(_.*)', '\\1', vgNames, perl=T)
    colnames(requirementMatrix) = vgNames
    rownames(requirementMatrix) = uniqueRequirementNames
    for(iRow in 1:nrow(requirementMap))
      requirementMatrix[
        requirementMap$name[iRow], 
        requirementMap$vgName[iRow]  ] = TRUE
    #browser()
    ### From Variables to VGs.
    #   allVariables = union(provisions, unique(allRequirements))
    #   allVariableNames = sapply(allVariables, function(v) v@name)
    # We need a check: no req Variable name should be repeated.
    # TODO here.
    howManyNeedMe = apply(requirementMatrix, 1, sum)
    candidates = outer(allRequirementNames, allProvisionNames, "==")
    dimnames(candidates) = list(allRequirementNames, vgNames)
    candidateCounts = apply(candidates, 1, sum)
    network@requirements = VariableList(allRequirements[candidateCounts==0])
    ### These are internally unmet requirements.  ### External view!
    network@allRequirements = allRequirements
    network@allRequirementNames = allRequirementNames
    network@requirementMap = requirementMap
    network@requirementMatrix = requirementMatrix
    network@howManyNeedMe = howManyNeedMe
    network@candidates = candidates
  }
  network
}
# debug(VariableNetwork)

#' getNetworkConnections
#' 
#' NOT USED?
#' @return A data frame with three columns: vg, prov, req.
#'  
getNetworkConnections = function(vgList, verbose=FALSE){
  triples = 
    lapply(names(vgList), function(vgName){
      vg = vgList[[vgName]]
      ifVerboseCat("vgName", vgName)
      if(length(vg@requirements)==0) return(NULL)
      reqList = if(is(vg@requirements, "Variable")) 
        list(vg@requirements) else vg@requirements
      ifVerboseCat("Length(reqList)=", length(reqList))
      if(length(reqList)==0) NULL
      else {
        reqNames=sapply(reqList, function(req) req@name) 
        ifVerboseCat("reqNames", reqNames)
        data.frame(vg=vgName,
                   prov=vg@provisions@name, req=reqNames)
      }
    })
  triples = triples[!sapply(triples, is.null)]
  triples = sapply(triples, as.matrix)
  triples = try(as.data.frame(t(triples)))
  if(class(triples) == "try-error") browser("Problem with s.data.frame(t(triples)")
  names(triples) = cq(vg, prov, req)
  triples
}



isRequiredHere = function(vg, req) {
  if(length(vg@requirements)==1) return(identical(vg@requirements, req))
  any(sapply(vg@requirements, FUN=identical, y=req))
}


permuteToUpperTriangular = function(M, whenToStop=3, verbose=F) {
  if(! is.matrix(M)) {catn("M"); print(M);
                      warning("permuteToUpperTriangular: M is not a matrix")
                      return(M)
  }
  isUpperTriangular = function(M) sum(M[row(M)>=col(M)])==0
  if(isUpperTriangular(M)) return(M)
  if(verbose) print(colSums(M))
  startNodes = which(colSums(M)==0)
  startNodeNames = rownames(M)[startNodes]
  if(verbose) print(startNodes)
  if(verbose) print(startNodeNames)
  if(length(startNodes)==0) stop("Cannot rotate")
  newOrder = c(startNodes, (1:nrow(M)) %except% startNodes)
  if(verbose) catn("newOrder=", newOrder)
  M = M[newOrder, newOrder]
  if(isUpperTriangular(M)) return(M)
  lowerRight.in = M[-c(1:length(startNodes)), -c(1:length(startNodes))] 
  lowerRight = permuteToUpperTriangular( lowerRight.in, whenToStop=whenToStop,verbose=verbose)
  if(verbose) print(lowerRight)
  M = M[c(startNodeNames, rownames(lowerRight)), 
        c(startNodeNames, rownames(lowerRight))]
  M
}


incidenceMatrix =  function(vN) {  ## migrate to VariableNetwork
  #if(length(vN@allRequirements) == 0) 
  #  return(NULL) ## or matrix(nrow=0,ncol=0))
  allProvisionNames = vN@allProvisionNames
#   provisionEdges = data.frame(VarGen=names(allProvisionNames), 
#                               Variable=allProvisionNames,
#                               stringsAsFactors=FALSE)  
  provisionEdges = vN@provisionMapReduced
  requirementNameList = sapply(vN@vgList, #  or, vN@requirementNameList just as good?
                           function(vg)
                             sapply(vg@requirements, slot, name="name"))
  requirementMatrix = vN@requirementMatrix
  pairs = expand.grid(rownames(requirementMatrix), stringsAsFactors=FALSE,
                      colnames(requirementMatrix))
  reqEdges = pairs[which(c(requirementMatrix)), ]
  if(ncol(reqEdges) == 2)
    colnames(reqEdges) = c("Variable", "VarGen")
  
  allVars = union(reqEdges$Variable, provisionEdges$Variable)
  nVars = length(allVars)
  allVGs = union(reqEdges$VarGen, provisionEdges$VarGen)
  nVG = length(allVGs)
  allNodes = c(allVars, allVGs)
  incidenceMatrix = matrix(0, nrow = nVars+nVG, ncol = nVars+nVG)
  dimnames(incidenceMatrix) = list(allNodes, allNodes)  
  for(r in 1:nrow(reqEdges)) 
    incidenceMatrix[reqEdges[r,1], reqEdges[r,2]] = 1
  for(r in 1:nrow(provisionEdges)) 
    incidenceMatrix[provisionEdges[r,1], provisionEdges[r,2]] = 1
  return(incidenceMatrix)
}



#### incidenceMatrix looks good!
#### fix it up... make it a DAG
matpow = function(M, p=2) eval(parse(text=paste(rep("M",p),collapse="%*%")))  
matsum = function(M)eval(parse(text=paste("matpow(M,", 
                                          1:nrow(M), ")", collapse="+")))
hasCycles = function(M) sum(diag(matsum(M))) > 0

# vNexampleWithCycles = VariableNetwork(
#   vgList=ListOfInserts(
#     vg1=VariableGenerator(provisions=vA, requirements=VariableList(
#       new("Variable", ))), )
# M = incidenceMatrix(vNexampleWithCycles)
# try(permuteToUpperTriangular(M))   #### should throw error.
# hasCycles(M)

# permuteToUpperTriangular(M)
# hasCycles(M)

if(interactive()){  ### for testing purposes
  vA = new("Variable",name="vA",description="vA",checkDataType=is.double)
  vB = new("Variable",name="vB",description="vB",checkDataType=is.double)
  vC = new("Variable",name="vC",description="vC",checkDataType=is.double)
  vD = new("Variable",name="vD",description="vD",checkDataType=is.double)
  vE = new("Variable",name="vE",description="vE",checkDataType=is.double)
  
  
  vgListExample = list(
    vg1=VariableGenerator(provisions=vA, generatorCode=function(){vB * vC},
                          requirements=VariableList(list(vB,vC))),
    vg2=VariableGenerator(provisions=vB, generatorCode=function(){vC+2},
                          requirements=VariableList(list(vC))),
    vg3=VariableGenerator(provisions=vC, generatorCode=function(){vD+3},
                          requirements=VariableList(list(vD))),
    vg4=VariableGenerator(provisions=vD, generatorCode=function(){4},
                          requirements=VariableList(list())),
    vg5=VariableGenerator(provisions=vE, generatorCode=function(){vA + 5},
                          requirements=VariableList(list(vA)))
  )
  
  make_vNexample = function()
    VariableNetwork(vgList=ListOfInserts(vgListExample))
  
  ### .... in progress...
  # getPopulationModelOutputs = function(popModel, alreadyDone=list()) {
  #   allVGs = function(pModel) {
  #    
  #     valueList = alreadyDone
  #     for(gen in popModel@requirements) {
  #       if( ! (gen@provisions %in% names(alreadyDone)))
  #         valueList = c(valueList,  
  #                       evaluateOutput(gen, alreadyDone=valueList))
  #     }
  #     return(valueList) 
  #   }
  #   ####
  # }
  # pmTempConn = getPMconnections(pmTemp, verbose=F)
  
}


pmEnv = function(pm) list2env(pm@vgList, new.env())

