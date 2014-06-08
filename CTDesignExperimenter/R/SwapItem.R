cat("======== SwapItem ================\n")

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

