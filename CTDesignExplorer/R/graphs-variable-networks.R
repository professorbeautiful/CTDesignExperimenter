cat("=============  graphs-variable-networks.R ============\n")

new("PopulationModel", vgList=VariableGeneratorList(vg_clearanceRate))
is(list(vg_clearanceRate), "VariableGeneratorList")  #F
is(vg_clearanceRate, "VariableGenerator")    #T
is(VariableGeneratorList(vg_clearanceRate), "VariableGeneratorList") #T

vg_clearanceRate@requirements
`@`(vg_clearanceRate, "requirements")  ### OK
lapply((VariableGeneratorList(vg_clearanceRate)), function(vg) vg@requirements) #OK

pmTemp = PopulationModel(vgList=VariableGeneratorList(
  list(vg_clearanceRate=vg_clearanceRate, 
       vg_toxDoseThreshold=vg_toxDoseThreshold, 
       vg_responseDoseThreshold=vg_responseDoseThreshold)))
### The VGs have to be named! (for list2env)
names(pmTemp@vgList)

evaluateOutput(vg_responseDoseThreshold)
pmTemp@vgList[["vg_clearanceRate"]]@parameters$clearanceLocation = 60
evaluateOutput(vg_responseDoseThreshold, 
               env=list2env(pmTemp@vgList, new.env())  )
vg_clearanceRate@parameters$clearanceSD=0
evaluateOutput(vg_clearanceRate) 
pmTemp@vgList$vg_clearanceRate@parameters$clearanceSD=0
evaluateOutput(pmTemp@vgList$vg_clearanceRate, 
               env=list2env(pmTemp@vgList, new.env())  )
evaluateOutput(vg_clearanceRate, 
               env=list2env(pmTemp@vgList, new.env())  )

########
library(gRbase)

pmEnv = function(pm) list2env(pm@vgList, new.env())


# pmTempConn = getPMconnections(pmTemp, verbose=F)

isRequiredHere = function(vg, req) {
  if(length(vg@requirements)==1) return(identical(vg@requirements, req))
  any(sapply(vg@requirements, FUN=identical, y=req))
}


rotateStarts = function(M, whenToStop=3, verbose=F) {
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
  lowerRight = rotateStarts( lowerRight.in, whenToStop=whenToStop,verbose=verbose)
  if(verbose) print(lowerRight)
  M = M[c(startNodeNames, rownames(lowerRight)), 
        c(startNodeNames, rownames(lowerRight))]
  M
}

incidenceMatrix =  function(vN) {  ## migrate to VariableNetwork
  provisionMap = sapply(vN@vgList, function(vg)
    vg@provisions@name)
  provisionEdges = data.frame(VarGen=names(provisionMap), Variable=provisionMap,
                              stringsAsFactors=FALSE)  
  requirementList = sapply(vN@vgList, function(vg)sapply(vg@requirements, slot, name="name"))
#  requirementMatrix = outer(vN@vgList, vN@allRequirements, isRequiredHere)
  requirementMatrix = sapply(names(requirementList), 
                             function(vg) unlist(requirementList) 
                             %in% requirementList[[vg]])
  rownames(requirementMatrix) = unlist(requirementList)
#   requirementMatrix = sapply(vN@vgList, function(vg) 
#     sapply(vN@allRequirements, isRequiredHere, vg=vg))
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

M = incidenceMatrix(vN)
rotateStarts(M)   #### should throw error.
hasCycles(M)

M["vg2", "vB"] = 0
M["vg3", "vA"] = 0
M["vg3", "vC"] = 1
rotateStarts(M)
hasCycles(M)
  


#######  ### My bipartite graph:

library(ggplot2)
  


#library("bipartite")
    
  #### For use wth Rgraphviz--  which seems really limited!! Most 
#   provEdgeNames = apply(provisionEdges, 1, paste,collapse="~")
#   reqEdgeNames = apply(reqEdges, 1, paste,collapse="~")
#   reformatEdgeNames = function(edgeNames) 
#     paste0("~", gsub(perl=T,'(\\w*)~(\\w*)', '\\2:\\1', edgeNames))
#   theDAG = eval(parse(text="dag(" %&% 
#              paste0(
#                reformatEdgeNames(c(provEdgeNames, reqEdgeNames))
#                     , sep="", collapse=", ")
#              %&% ")"))
#   nodes(theDAG)
#   graph::edges(theDAG)
#   edgeNames(theDAG)
#   edgeAttrs=list( #label=edgeLabels,
#     col=c(withNames("blue", provEdgeNames), withNames("red", reqEdgeNames))
#     ,lty=c(withNames(1, provEdgeNames), withNames(2, reqEdgeNames))
#   )
#   nodeAttrs=list( #label=edgeLabels,
#     # color= bordercolor.  NOT col!
#     color=withNames(ifelse(regexpr("vg", nodes(theDAG))==1, "blue", "red"), nodes(theDAG))
#     ,textCol=withNames(ifelse(regexpr("vg", nodes(theDAG))==1, "blue", "red"), nodes(theDAG))
#     ,fill=withNames(ifelse(regexpr("vg", nodes(theDAG))==1, "yellow", "green"), nodes(theDAG))
#   )
#   library("Rgraphviz")
#   DAGexamplelayout = layoutGraph(theDAG, edgeAttrs=edgeAttrs, nodeAttrs=nodeAttrs)
#   #plot(DAGexamplelayout)
#   renderGraph(DAGexamplelayout,
#               graph.pars=list(edges=edgeAttrs, nodes=nodeAttrs))
#   #   dagList(unlist(apply(PMconn, 2, function(conn) 
# #   as.formula(paste0("~", conn[2], ":", conn[3])))))
# }
# 
# DAGexample = makeVNDAG(vNexample)
# graph::nodes(DAGexample)
# graph::edges(DAGexample)
# edgeNames(DAGexample)
# names(edgeNames(DAGexample))
# edgeAttrs=list( #label=edgeLabels,
#                 color=withNames("blue", edgeNames)
# )
# 
# 
# 
# 
# shapes = c("circle", "circle", "circle")
# colors = c("red","green","blue")
# fontsize=50
# names(shapes) = nodes(DAGexample)
# names(colors) = nodes(DAGexample)
# fontcolors = colors
# 
# DAGexample@nodeData  # not helpful
# nodes(DAGexample) # the node names
# graph::edges(DAGexample) # the edge list
# # igraph::edges(DAGexample) # the edge list-- DIFFERENT
# edgesAsMatrix = cbind(unlist(graph::edges(DAGexample)),
#                       rep(names(graph::edges(DAGexample)),
#                           sapply(graph::edges(DAGexample), length),
#                       ) )
# edgeNames = apply(edgesAsMatrix[,2:1], 1, paste, sep="~", collapse="~")
# pmTempConn = getPMconnections(pmTemp, verbose=F)
# edgeLabels = pmTempConn["vg", ]
# names(edgeLabels) = edgeNames
#   
# edgeNames(DAGexample)
# names(edgeNames(DAGexample))
# edgeAttrs=list( label=edgeLabels,
#                 labelJust=withNames("c", edgeNames),
#                 fontcolor=withNames("green", edgeNames),
#                 color=withNames("blue", edgeNames)
# )
# # library("igraph)")
# library("Rgraphviz")
# DAGexamplelayout = layoutGraph(DAGexample, edgeAttrs=edgeAttrs)
# renderGraph(DAGexamplelayout)   
# 
# #http://mae.ucdavis.edu/dsouza/Lectures/Rgraphviz.pdf
# # http://stackoverflow.com/questions/16570887/rgraphviz-edge-labels-outside-plotting-region
# ## try igraph, neato, SNA
# DAGexamplelayout
# nodeRenderInfo(DAGexamplelayout)
# names(nodeRenderInfo(DAGexamplelayout))
# edgeRenderInfo(DAGexamplelayout)
# names(edgeRenderInfo(DAGexamplelayout))
# graph.par(list(nodes=list(
#   cex=1, textCol="red", 
#   labelJust="r", shape="ellipse"
#   )))
# renderGraph(DAGexamplelayout)   
# 
# plot(new=T,
#      DAGexample,
#      attrs=list(graph=list( layout="dot")),
# #     attrs=list(graph=list(rankdir="LR", layout="dot")),
#      edgeAttrs=list(label=edgeLabels,
#                     fontsize=withNames(fontsize, edgeNames)),
#      nodeAttrs=list(
#        color=colors,
#        fontcolor=fontcolors,
#        fontsize=withNames(fontsize, nodes(DAGexample)),
#        shape=shapes)
# )  
# 
# `@`(vg_toxDoseThreshold, "requirements") 
# 
# #          validity=function(object) {
# #            if(!is.list(object@requirements))
# #              return("PopulationModel requirements should be a list.")
# #            if(all(
# #   	sapply(object@requirements, function(req)
# #       is(req, "VariableGenerator") | is(req,"PopulationModel"))))
# # 	     return(TRUE)
# #          }
# # )
