cat("======== Specifier ================\n")

##' Specifier
##' 
##' Superclass of VariableGenerator, VariableNetwork
setClass("Specifier", 
         #       contains="character", ### maybe a name will be desired.
         slots=list(
           parameters="list", 
           requirements="Variables",
           provisions="Variables"),
         prototype=list(parameters=list(), 
                        requirements=NULL,
                        provisions=NULL)
)


