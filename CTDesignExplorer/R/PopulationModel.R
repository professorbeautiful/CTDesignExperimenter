setClass("PopulationModel", contains="Specifier",
         validity=function(object) {
           if(!is.list(object@requirements))
             return("PopulationModel requirements should be a list.")
           if(all(
		sapply(object@requirements, function(req)
      is(req, "VariableGenerator") | is(req,"PopulationModel"))))
	     return(TRUE)
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
