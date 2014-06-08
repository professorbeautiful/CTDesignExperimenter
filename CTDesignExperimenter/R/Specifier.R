cat("======== Specifier ================\n")

##' Specifier
##' 
##' Superclass of VariableGenerator, VariableNetwork
setClass("Specifier", 
         contains="SwapItem", 
         slots=list(
           parameters="list", 
           requirements="Variables",
           provisions="Variables"),
         prototype=list(parameters=list(), 
                        requirements=NULL,
                        provisions=NULL)
)


