cat("======== Specifier ================\n")

##' SwapItem
##' 
##' Class for objects to be saved in the local or shared CTDEswapmeet.
##' 
setClass("SwapItem",
         slots=list(
           author="character",
           timestamp="POSIXct",
           description="character",
           filename="character")
)

makeItemFileName = function(item) {
  if(!is(item, "SwapItem"))
    stop("makeItemFileName: item should be a SwapItem")
  # NOTE: this sets the item's timestamp field!
  item@timestamp = Sys.time()
  prefix = switch(class(item),
                  VariableGenerator="I_" %&% item@outputVariable@name,
                  Variable="V_" %&% item@name,
                  Scenario="S_" %&% item@name,
                  ListOfInserts="L_" %&% "ListOfInserts", 
                  Criterion="C_" %&% item@name,
                  "UNKNOWN_item_type")
  filename = paste0(prefix, "_", Sys.getenv("USER"), 
                    "_", as.numeric(item@timestamp), ".R")
  return(filename)
}

makeItemFilePath = function(item, dir) {
  if(missing(dir)) {
    if(!exists(x = "swapDir", where = 1)) {
      dir = "../CTDEswapmeet"
      assign('dir', dir, pos=1)
    }
  }
  return(dir %&% "/" %&% makeItemFileName(item))
}


if(gitAction=="write")
           writeVariableFile(newVariable) # use default folder swapmeet.
         if(gitAction=="push") {
           writeVariableFile(newVariable)
           pushSwapMeetFiles()
         }
         
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


