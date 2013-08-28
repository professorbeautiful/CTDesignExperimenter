cat("=========== VariableNetwork.R ===========\n")


setClass("VariableGeneratorList", contains="list")

# VariableGeneratorListValidity = function(object){
#   for(vg in object@.Data){
#     if(!is(vg, "VariableGenerator"))
#       return("VariableGeneratorList: "
#              %&% "validity error: not all "
#              %&% " components are VariableGenerator")
#   }
#   return(TRUE)
# }
# setValidity("VariableGeneratorList", VariableGeneratorListValidity) 

VariableGeneratorList = function(vgList=NULL) {
  if(is.null(vgList)) return(new("VariableGeneratorList", list()))
  theNames = names(vgList)
  if(is.null(theNames))
    theNames = paste0("vg", 1:length(vgList))
  if(is(vgList, "VariableGenerator"))
    vgListObj = new("VariableGeneratorList", list(vgList))
  else
    vgListObj = new("VariableGeneratorList", vgList)
  print(names(vgListObj))
  names(vgListObj@.Data) = theNames
  ### validity
  for(vg in vgListObj@.Data){
    if(!is(vg, "VariableGenerator"))
      stop("VariableGeneratorList: "
             %&% "validity error: not all "
             %&% " components are VariableGenerator")
  }
  vgListObj
}

setClass("VariableNetwork", contains="Specifier",
         slots=list(vgList="VariableGeneratorList",
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
  sapply(vg@requirements, slot, name="name")
}

VariableNetwork = function(vgList=NULL, varNetworkList=NULL){
  if(is.null(vgList)) vgList = list()
  if(is(vgList, "VariableGenerator")) vgList = list(vgList)
  if(!is.null(varNetworkList)) {
    if(is(varNetworkList, "VariableNetwork")) varNetworkList = list(varNetworkList)
    for(vN in varNetworkList) {
      if(!is(vN, "VariableNetwork")) 
        stop("varNetworkList should be a list of VariableNetworks")
      vgList = c(vgList, vN@vgList@.Data)
    }
    if(any(duplicated(names(vgList)))) 
      stop("VariableNetwork: all vg names must be unique")
    vgList = VariableGeneratorList(vgList=vgList)
  }
  network = new("VariableNetwork", vgList=vgList)

  provisions = c(lapply(vgList, function(vg) vg@provisions))
  provisions = provisions[!sapply(provisions, is.null)]
  provisions = unique(provisions)   ###### outside view!
  provisions = new("VariableList", provisions)
  if(length(provisions) == 0) provisions = NULL
  network@provisions = provisions  ###### outside view!
  vgNames = names(vgList)
  #  names(network@vgList) = vgNames ### no such slot
  allProvisionNames = sapply(network@vgList,
                          function(vg) vg@provisions@name)
  names(allProvisionNames) = vgNames
  allProvisions = sapply(network@vgList,
                      function(vg) vg@provisions)
  names(allProvisions) = vgNames
  provisionMap = lapply(network@vgList, function(vg)
    vg@provisions@name)
  names(provisionMap) = vgNames
#   provisionEdges = data.frame(VarGen=names(provisionMap), Variable=provisionMap,
#                               stringsAsFactors=FALSE)
  getReqs = function(vg) (vg@requirements)
  requirementMap = sapply(network@vgList, getReqs)
  ### From VGs to Variables.
  allRequirements = unique(unlist(requirementMap))
  if(!is.null(allRequirements) & !is.list(allRequirements)) 
    allRequirements = list(allRequirements)
  ## each requirement counted only once.
  allRequirementNames = sapply(allRequirements, function(req)req@name)
  
  ## but if 2 requirements have the same name, we need to know that. 
  
  requirementList = sapply(vgList, getRequirementNames)
  uniqueRequirementNames = unique(unlist(requirementList))
  #  requirementMatrix = outer(vN@vgList, vN@allRequirements, isRequiredHere)
  outer( uniqueRequirementNames, names(vgList), `%in%`)
  requirementMatrix = matrix(FALSE, nrow=length(uniqueRequirementNames), 
                             ncol=length(names(vgList)))
  colnames(requirementMatrix) = names(vgList)
  rownames(requirementMatrix) = uniqueRequirementNames
  for(vgName in names(vgList)) 
    requirementMatrix [ , vgName] = 
     uniqueRequirementNames %in% requirementList[[vgName]]
  if(is.matrix(requirementMatrix)) {
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
  } else {
    requirementMatrix = matrix(nrow=0, ncol=0)
    howManyNeedMe = requirementMatrix
    candidates = list()
    candidateCounts = numeric(0)
  }
#  browser()
     #  ifVerboseCat("candidates for reqs: ", candidateCounts)
     #   for(slotName in names(getSlots(x=getClass("VariableNetwork"))) )
     #     eval(parse(text=paste0("`@<-`(network, ", slotName, 
     #                            ", get(\"", slotName, "\"))")))
     #   #`@<-`(network, slotName, get(slotName))
  if(is.null(allRequirements)) {
    allRequirements=list()
    allRequirementNames=character(0)
    requirementMatrix = matrix(nrow=0,ncol=0)
    howManyNeedMe = numeric(0)
    candidates = matrix(nrow=0,ncol=0)
  }
  network@requirements = VariableList(allRequirements[candidateCounts==0])
  ### These are internally unmet requirements.  ### External view!
  network@allProvisions = allProvisions
  network@allProvisionNames = allProvisionNames
  network@provisionMap = provisionMap
  network@allRequirements = allRequirements
  network@allRequirementNames = allRequirementNames
  network@requirementMap = requirementMap
  network@requirementMatrix = requirementMatrix
  network@howManyNeedMe = howManyNeedMe
  network@candidates = candidates
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
  triples = as.data.frame(t(triples))
  names(triples) = cq(vg, prov, req)
  triples
}


vA = new("Variable",name="vA",description="vA",dataType="double")
vB = new("Variable",name="vB",description="vB",dataType="double")
vC = new("Variable",name="vC",description="vC",dataType="double")
vD = new("Variable",name="vD",description="vD",dataType="double")
vE = new("Variable",name="vE",description="vE",dataType="double")


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

vNexample = VariableNetwork(vgList=VariableGeneratorList(vgListExample)) 

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

pmEnv = function(pm) list2env(pm@vgList, new.env())


# pmTempConn = getPMconnections(pmTemp, verbose=F)

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
  if(length(vN@allRequirements) == 0) return(NULL) ## or matrix(nrow=0,ncol=0))
  allProvisionNames = vN@allProvisionNames
  provisionEdges = data.frame(VarGen=names(allProvisionNames), 
                              Variable=allProvisionNames,
                              stringsAsFactors=FALSE)  
  requirementList = sapply(vN@vgList, 
                           function(vg)
                             sapply(vg@requirements, slot, name="name"))
  requirementMatrix = vN@requirementMatrix
  pairs = expand.grid(rownames(requirementMatrix), stringsAsFactors=FALSE,
                      colnames(requirementMatrix))
  reqEdges = pairs[which(c(requirementMatrix)), ]
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
#   vgList=VariableGeneratorList(
#     vg1=VariableGenerator(provisions=vA, requirements=VariableList(
#       new("Variable", ))), )
# M = incidenceMatrix(vNexampleWithCycles)
# try(permuteToUpperTriangular(M))   #### should throw error.
# hasCycles(M)

# permuteToUpperTriangular(M)
# hasCycles(M)
