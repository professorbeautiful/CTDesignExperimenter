cat("=============  graphs-variable-networks.R ============\n")

# new("PopulationModel", vgList=VariableGeneratorList(vg_clearanceRate))
# is(list(vg_clearanceRate), "VariableGeneratorList")  #F
# is(vg_clearanceRate, "VariableGenerator")    #T
# is(VariableGeneratorList(vg_clearanceRate), "VariableGeneratorList") #T
# 
# vg_clearanceRate@requirements
# `@`(vg_clearanceRate, "requirements")  ### OK
# lapply((VariableGeneratorList(vg_clearanceRate)), function(vg) vg@requirements) #OK
# 
# pmTemp = PopulationModel(vgList=VariableGeneratorList(
#   list(vg_clearanceRate=vg_clearanceRate, 
#        vg_toxDoseThreshold=vg_toxDoseThreshold, 
#        vg_responseDoseThreshold=vg_responseDoseThreshold)))
# ### The VGs have to be named! (for list2env)
# 
# names(pmTemp@vgList)
# 
# evaluateVNoutputs(pmTemp)
# 
# pmTemp@vgList[["vg_clearanceRate"]]@parameters$clearanceLocation = 60


# evaluateGeneratorOutput(vg_responseDoseThreshold, 
#                env=list2env(pmTemp@vgList, new.env())  )
# vg_clearanceRate@parameters$clearanceSD=0
# evaluateGeneratorOutput(vg_clearanceRate) 
# pmTemp@vgList$vg_clearanceRate@parameters$clearanceSD=0
# evaluateGeneratorOutput(pmTemp@vgList$vg_clearanceRate, 
#                env=list2env(pmTemp@vgList, new.env())  )
# evaluateGeneratorOutput(vg_clearanceRate, 
#                env=list2env(pmTemp@vgList, new.env())  )

########
#library(gRbase)
#######  ### My bipartite graph:
#library(ggplot2)
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
