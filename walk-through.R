walk.through <- function() {
  tb <- unlist(.Traceback)
  if(is.null(tb)) stop("no traceback to use for debugging")
  assign("debug.fun.list", matrix(unlist(strsplit(tb, "\\(")), nrow=2)[1,], envir=.GlobalEnv)
  lapply(debug.fun.list, function(x) debug(get(x)))
  print(paste("Now debugging functions:", paste(debug.fun.list, collapse=",")))
}

unwalk.through <- function() {
  lapply(debug.fun.list, function(x) undebug(get(as.character(x))))
  print(paste("Now undebugging functions:", paste(debug.fun.list, collapse=",")))
  rm(list="debug.fun.list", envir=.GlobalEnv)
}