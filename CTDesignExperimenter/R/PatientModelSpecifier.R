cat("======== PatientModelSpecifier.R================\n")

setClass("PatientModelSpecifier", contains="VariableNetwork")

PatientModelSpecifier = function(vgList=NULL, varNetworkList=NULL) {
  return(as(VariableNetwork(vgList, varNetworkList), "PatientModelSpecifier"))
}
    
setMethod("print", "PatientModelSpecifier",
          function(x) {
            x = as(object=x, Class="PatientModelSpecifier")
            cat("PatientModelSpecifier:\n ") 
            for(req in x@requirements) print(req)
            invisible(x)
          }
)
