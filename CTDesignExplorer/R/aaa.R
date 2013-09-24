

### Some utility classes and methods
## Class Union: NumericLogical
setClassUnion("NumericLogical",c("numeric","logical"))

print(getClass("NumericLogical"))
## Class Union: OptionalNumeric
setClassUnion("OptionalNumeric",c("numeric","NULL"))
print(getClass("OptionalNumeric"))

## Class Union: OptionalCharacter
setClassUnion("OptionalCharacter",c("character","NULL"))

### utilities borrowed from mvbutils

`%&%` = function (a, b)
        paste(a, b, sep = "")

`%except%` = function (vector, condition) 
	vector[match(vector, condition, 0) == 0]

"cq" = function (...) 
{
    as.character(sapply(as.list(match.call(expand.dots = TRUE))[-1], 
        as.character))
}

as.cat = function (x) 
{
  stopifnot(is.character(x))
  oldClass(x) <- "cat"
  x
}

catn=function(...) cat(..., "\n")

withNames =
  function(x, n) {temp = data.frame(x=x,n=n);
                  x = temp$x;
                  n = temp$n;
                  names(x) <- n; 
                  x}

ifVerboseCat = function(...){
  #print(paste0("ifVerboseCat: sys.call(-1)=", sys.call(-1)))
  f=try(as.character(parse(text=sys.call(-1)))[1] )
  if(class(f) == "try-error") return(invisible(NULL))
  if(!exists("verboseOptions")) verboseOptions = logical(0)
  if(is.na(verboseOptions[f])) {
    verboseOptions[f] <- TRUE
    assign("verboseOptions", verboseOptions, pos=1, immediate=TRUE)
    #ifVerboseCat(verboseOptions)
  }
  if(verboseOptions[f]) 
    catn(f, ": ", ...)
  invisible(NULL)
}

clear = function(){
  answer <- repeat {
    cat("Delete ALL files in .GlobalEnv?\n  (cannot be undone): ")
    answer <- readline()
    answer <- gsub("(\\w)", "\\U\\1", answer, perl=T)
    answer <- pmatch(answer, c("YES",  "NO", "N"))
      if (!is.na(answer)) {
        if(answer %in% 1)  
        rm(list=ls(all=T, pos=1), pos=1)
      else
        cat("Aborted. No objects deleted.\n")
      return(invisible(NULL))
    }
  }
}


#' from Yuanyuan
#' ## Function: instantiateS4Object
# className is a character 
# "slots" is a named list with the names corresponding to the slot names
instantiateS4Object <- function(className,slots){
  Object <- new(className)
  for ( SlotName in names(slots))
    slot(Object,SlotName) <- slots[[SlotName]]
  return(Object)
} 

### other utilities
### inclusive , includes the endpoints
"%between%" = function(x, range) { (x<=range[2] & x>=range[1])}