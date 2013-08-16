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
  if(is(vgList, "VariableGenerator"))
    new("VariableGeneratorList", list(vgList))
  else
    new("VariableGeneratorList", vgList)
}
setClass("PopulationModel", contains="Specifier",
         slots=list(vgList="VariableGeneratorList")
)
PopulationModel = function(vgList, popModelList=NULL){
  ## TODO:  fold in other PopulationModels: 2nd arg
  pm = new("PopulationModel", vgList=vgList)
  if(is(vgList, "VariableGeneratorList")) {
    requirements = c(lapply(vgList, function(vg) vg@requirements))
    requirements = requirements[!sapply(requirements, is.null)]
    requirements = unique(requirements)
    requirements = 
                          new("VariableList", requirements)
    if(length(requirements) == 0) requirements = NULL
    pm@requirements = requirements
  }
  else if(is(vgList, "VariableGenerator"))
    pm@requirements = vgList@requirements
  if(is(vgList, "VariableGeneratorList")) {
    provisions = c(lapply(vgList, function(vg) vg@provisions))
    provisions = provisions[!sapply(provisions, is.null)]
    provisions = unique(provisions)
    provisions = 
      new("VariableList", provisions)
    if(length(provisions) == 0) provisions = NULL
    pm@provisions = provisions
  }
  else if(is(vgList, "VariableGenerator"))
    pm@provisions = vgList@provisions
  pm
}

new("PopulationModel", vgList=VariableGeneratorList(clearanceRate))

is(list(vg_clearanceRate), "VariableGeneratorList")  #F
is(vg_clearanceRate, "VariableGenerator")    #T
is(VariableGeneratorList(vg_clearanceRate), "VariableGeneratorList") #T

vg_clearanceRate@requirements
`@`(vg_clearanceRate, "requirements")  ### OK
lapply((VariableGeneratorList(vg_clearanceRate)), function(vg) vg@requirements) #OK

PopulationModel(vgList=VariableGeneratorList(vg_clearanceRate))


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

library(gRbase)

pmEnv = function(pm) list2env(pm@vgList, new.env())
pmDAG =   dagList(
    (unlist(
      lapply(pmTemp@vgList, function(vg){
        reqList = if(is(vg@requirements, "Variable")) 
          list(vg@requirements) else vg@requirements
        if(length(reqList)>0) {
          reqNames=sapply(reqList, function(req) req@name) 
          sapply(FUN=as.formula, 
                 paste0("~", vg@provisions@name, ":", reqNames))
        }
      })
    ))
  )
pmDAG@nodeData  # not helpful
nodes(pmDAG) # the node names
shapes = c("circle", "circle", "circle")
colors = c("red","green","blue")
fontsize=rep(40,3)
names(fontsize) = nodes(pmDAG)
names(shapes) = nodes(pmDAG)
names(colors) = nodes(pmDAG)
fontcolors = colors
plot(new=T,
     pmDAG,
     nodeAttrs=list(
       color=colors,
       fontcolor=fontcolors,
       fontsize=fontsize,
       shape=shapes)
#     lblString=
)  

`@`(toxDoseThreshold, "requirements") 

#          validity=function(object) {
#            if(!is.list(object@requirements))
#              return("PopulationModel requirements should be a list.")
#            if(all(
# 		sapply(object@requirements, function(req)
#       is(req, "VariableGenerator") | is(req,"PopulationModel"))))
# 	     return(TRUE)
#          }
# )

setMethod("print", "PopulationModel",
          function(x) {
            x = as(object=x, Class="PopulationModel")
            cat("PopulationModel:\n ") 
            for(req in x@requirements) print(req)
            invisible(x)
          }
)
# We are only interested in requirements

###3 .... in progress...
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
