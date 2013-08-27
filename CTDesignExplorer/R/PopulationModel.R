cat("======== PopulationModel.R================\n")

setClass("PopulationModel", contains="VariableNetwork")

PopulationModel = function(vgList, varNetworkList=NULL) {
  return(as(VariableNetwork(vgList, varNetworkList), "PopulationModel"))
}
    
setMethod("print", "PopulationModel",
          function(x) {
            x = as(object=x, Class="PopulationModel")
            cat("PopulationModel:\n ") 
            for(req in x@requirements) print(req)
            invisible(x)
          }
)
