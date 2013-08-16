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
  pm
}

new("PopulationModel", vgList=VariableGeneratorList(clearanceRate))

is(list(clearanceRate), "VariableGeneratorList")  #F
is((clearanceRate), "VariableGenerator")    #T
is(VariableGeneratorList(clearanceRate), "VariableGeneratorList") #T

clearanceRate@requirements
`@`(clearanceRate, "requirements")  ### OK
lapply((VariableGeneratorList(clearanceRate)), function(vg) vg@requirements) #OK

PopulationModel(vgList=VariableGeneratorList(clearanceRate))

debug(PopulationModel)

pmTemp = PopulationModel(vgList=VariableGeneratorList(
  list(clearanceRate, toxDoseThreshold, responseDoseThreshold)))

`@`(clearanceRate, "requirements") ### NULL

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
