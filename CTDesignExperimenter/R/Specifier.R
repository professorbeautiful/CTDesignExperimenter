cat("======== Specifier ================\n")

##' SwapItem
##' 
##' Class for objects to be saved in the local or shared CTDEswapmeet.
##' description Provided by the author no later than when the object is saved.
##' other_fields Filled automatically when the object is saved.
##' 
setClass("SwapItem",
         slots=list(
           description="character",
           author="character",
           timestamp="POSIXct",
           filename="character")
)


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


