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


pmTempConn = getPMconnections(pmTemp, verbose=F)

makePMDAG =  function(PMconn) {
  dagList(unlist(apply(PMconn, 2, function(conn) 
  as.formula(paste0("~", conn[2], ":", conn[3])))))
}
pmTempDAG = makePMDAG(pmTempConn)

shapes = c("circle", "circle", "circle")
colors = c("red","green","blue")
fontsize=50
names(shapes) = nodes(pmTempDAG)
names(colors) = nodes(pmTempDAG)
fontcolors = colors

pmTempDAG@nodeData  # not helpful
nodes(pmTempDAG) # the node names
graph::edges(pmTempDAG) # the edge list
# igraph::edges(pmTempDAG) # the edge list-- DIFFERENT
edgesAsMatrix = cbind(unlist(graph::edges(pmTempDAG)),
                      rep(names(graph::edges(pmTempDAG)),
                          sapply(graph::edges(pmTempDAG), length),
                      ) )
edgeNames = apply(edgesAsMatrix[,2:1], 1, paste, sep="~", collapse="~")
edgeLabels = pmTempConn["vg", ]
names(edgeLabels) = edgeNames
  
edgeNames(pmTempDAG)
names(edgeNames(pmTempDAG))
edgeAttrs=list( label=edgeLabels,
                labelJust=withNames("c", edgeNames),
                fontcolor=withNames("green", edgeNames),
                color=withNames("blue", edgeNames)
)
# library("igraph)")
library("Rgraphviz")
pmTempDAGlayout = layoutGraph(pmTempDAG, edgeAttrs=edgeAttrs)
renderGraph(pmTempDAGlayout)   

#http://mae.ucdavis.edu/dsouza/Lectures/Rgraphviz.pdf
# http://stackoverflow.com/questions/16570887/rgraphviz-edge-labels-outside-plotting-region
## try igraph, neato, SNA
pmTempDAGlayout
nodeRenderInfo(pmTempDAGlayout)
names(nodeRenderInfo(pmTempDAGlayout))
edgeRenderInfo(pmTempDAGlayout)
names(edgeRenderInfo(pmTempDAGlayout))
graph.par(list(nodes=list(
  cex=1, textCol="red", 
  labelJust="r", shape="ellipse"
  )))
renderGraph(pmTempDAGlayout)   

plot(new=T,
     pmTempDAG,
     attrs=list(graph=list( layout="dot")),
#     attrs=list(graph=list(rankdir="LR", layout="dot")),
     edgeAttrs=list(label=edgeLabels,
                    fontsize=withNames(fontsize, edgeNames)),
     nodeAttrs=list(
       color=colors,
       fontcolor=fontcolors,
       fontsize=withNames(fontsize, nodes(pmTempDAG)),
       shape=shapes)
)  

`@`(vg_toxDoseThreshold, "requirements") 

#          validity=function(object) {
#            if(!is.list(object@requirements))
#              return("PopulationModel requirements should be a list.")
#            if(all(
#   	sapply(object@requirements, function(req)
#       is(req, "VariableGenerator") | is(req,"PopulationModel"))))
# 	     return(TRUE)
#          }
# )
