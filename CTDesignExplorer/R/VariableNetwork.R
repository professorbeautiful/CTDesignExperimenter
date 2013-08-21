cat("=========== VariableNetwork.R ===========\n")

setClass("VariableGeneratorList", contains="list")

VariableGeneratorListValidity = function(object){
  for(vg in object@.Data){
    if(!is(vg, "VariableGenerator"))
      return("VariableGeneratorList: "
             %&% "validity error: not all "
             %&% " components are VariableGenerator")
  }
  return(TRUE)
}
setValidity("VariableGeneratorList", VariableGeneratorListValidity) 

VariableGeneratorList = function(vgList) {
  theNames = names(vgList)
  print(theNames)
  if(is.null(theNames))
    theNames = paste0("vg", 1:length(vgList))
  print(theNames)
  if(is(vgList, "VariableGenerator"))
    vgListObj = new("VariableGeneratorList", list(vgList))
  else
    vgListObj = new("VariableGeneratorList", vgList)
  print(names(vgListObj))
  names(vgListObj@.Data) = theNames
  vgListObj
}

setClass("VariableNetwork", contains="Specifier",
         slots=list(vgList="VariableGeneratorList",
                    allRequirements="list",
                    allRequirementNames="character",
                    requirementMap="list",
                    requirementMatrix="matrix",
                    howManyNeedMe="numeric",
                    candidates="matrix")
)

setClass("PopulationModel", contains="VariableNetwork")

setClass("OutcomeModel", contains="VariableNetwork")

setClass("Scenario", contains="VariableNetwork")

requirementNames = function(vg){ 
  if(is.null(vg@requirements)) return(NULL)
  if(is(vg@requirements, "Variable"))
    return(list(vg@requirements))
  sapply(vg@requirements, slot, name="name")
}

VariableNetwork = function(vgList, varNetworkList=NULL){
  network = new("VariableNetwork", vgList=vgList)
  if(is(vgList, "VariableGenerator")) vgList = list(vgList)
  provisions = c(lapply(vgList, function(vg) vg@provisions))
  provisions = provisions[!sapply(provisions, is.null)]
  provisions = unique(provisions)   ###### outside view!
  provisions = new("VariableList", provisions)
  if(length(provisions) == 0) provisions = NULL
  network@provisions = provisions
  vgNames = names(vgList)
  #  names(network@vgList) = vgNames ### no such slot
  provisionNames = sapply(network@vgList,
                          function(vg) vg@provisions@name)
  names(provisionNames) = vgNames
  provisions = sapply(network@vgList,
                      function(vg) vg@provisions)
  names(provisions) = vgNames
  getReqs = function(vg) (vg@requirements)
  requirementMap = sapply(network@vgList, getReqs)
  ### From VGs to Variables.
  allRequirements = unique(unlist(requirementMap))
  if(!is.list(allRequirements)) allRequirements = list(allRequirements)
  ## each requirement counted only once.
  allRequirementNames = sapply(allRequirements, function(req)req@name)
  ## but if 2 requirements have the same name, we need to know that. 
  provisionMap = sapply(network@vgList, function(vg)
    vg@provisions@name)
  provisionEdges = data.frame(VarGen=names(provisionMap), Variable=provisionMap,
                              stringsAsFactors=FALSE)
  requirementList = sapply(vgList, requirementNames)
  #  requirementMatrix = outer(vN@vgList, vN@allRequirements, isRequiredHere)
  requirementMatrix = sapply(names(requirementList), 
                             function(vg) unlist(requirementList) 
                             %in% requirementList[[vg]])
  rownames(requirementMatrix) = unlist(requirementList)
  
  ### From Variables to VGs.
  #   allVariables = union(provisions, unique(allRequirements))
  #   allVariableNames = sapply(allVariables, function(v) v@name)
  # We need a check: no req Variable name should be repeated.
  # TODO here.
  howManyNeedMe = apply(requirementMatrix, 1, sum)
  candidates = outer(allRequirementNames, provisionNames, "==")
  dimnames(candidates) = list(allRequirementNames, vgNames)
  candidateCounts = apply(candidates, 1, sum)
  print(candidateCounts)
#   for(slotName in names(getSlots(x=getClass("VariableNetwork"))) )
#     eval(parse(text=paste0("`@<-`(network, ", slotName, 
#                            ", get(\"", slotName, "\"))")))
#   #`@<-`(network, slotName, get(slotName))
  network@requirements = VariableList(allRequirements[candidateCounts==0])
  network@allRequirements = allRequirements
  network@allRequirementNames = allRequirementNames
  network@requirementMap = requirementMap
  network@requirementMatrix = requirementMatrix
  network@howManyNeedMe = howManyNeedMe
  network@candidates = candidates
   network
}
#debug(VariableNetwork)



#' getNetworkConnections
#' 
#' NOT USED?
#' @return A data frame with three columns: vg, prov, req.
#'  
getNetworkConnections = function(vgList, verbose=FALSE){
  triples = 
    lapply(names(vgList), function(vgName){
      vg = vgList[[vgName]]
      if(verbose) catn("vgName", vgName)
      if(length(vg@requirements)==0) return(NULL)
      reqList = if(is(vg@requirements, "Variable")) 
        list(vg@requirements) else vg@requirements
      if(verbose) catn("Length(reqList)=", length(reqList))
      if(length(reqList)==0) NULL
      else {
        reqNames=sapply(reqList, function(req) req@name) 
        if(verbose) catn("reqNames", reqNames)
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


vA = new("Variable",name="vA",description="vA",dataType="??")
vB = new("Variable",name="vB",description="vB",dataType="??")
vC = new("Variable",name="vC",description="vC",dataType="??")
vD = new("Variable",name="vD",description="vD",dataType="??")
vgListExample = list(
  vg1=VariableGenerator(provisions=vA, generatorCode=function(){},
                    requirements=VariableList(list(vB,vC))),
  vg2=VariableGenerator(provisions=vB, generatorCode=function(){},
                    requirements=VariableList(list(vA))),
  vg3=VariableGenerator(provisions=vA, generatorCode=function(){},
                    requirements=VariableList(list(vD))))
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
