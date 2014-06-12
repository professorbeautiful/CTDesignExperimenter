cat("======== SwapItem ================\n")

##' SwapItem
##' 
##' Class for objects to be saved in the local or shared CTDEswapmeet.
##' description Provided by the author no later than when the object is saved.
##' other_fields Filled automatically when the object is saved.
##' 
setClass("SwapItem",
         slots=list(
           name="character",
           description="character",
           author="character",
           timestamp="POSIXct",
           filename="character"),
         prototype=prototype(
           name="NEED NAME",
           description="NEED DESCRIPTION",
           author=Sys.getenv("USER"),
           timestamp=as.POSIXct(NA), ## assigned when saved
           filename="")  ## assigned when saved
)

