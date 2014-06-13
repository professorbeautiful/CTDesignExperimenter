## See http://stackoverflow.com/questions/3466599/dputting-an-s4-object
dput2 <- function (x,
                   file = "",
                   control = c("keepNA", "keepInteger", "showAttributes")){
  if (is.character(file))
    if (nzchar(file)) {
      file <- file(file, "wt")
      on.exit(close(file))
    }
  else file <- stdout()
  opts <- .deparseOpts(control)
  if (isS4(x)) {
    cat("new(\"", class(x), "\"\n", file = file, sep = "")
    for (n in slotNames(x)) {
      cat("    ,", n, "= ", file = file)
      dput2(slot(x, n), file = file, control = control)
    }
    cat(")\n", file = file)
    invisible()
  } else if(length(grep('@',capture.output(str(x)))) > 0){
    if(is.list(x)){
      cat("list(\n", file = file, sep = "")
      for (i in 1:length(x)) {
        if(!is.null(names(x))){
          n <- names(x)[i]
          if(n != ''){
            cat("    ,", n, "= ", file = file)
          }
        }
        if(i > 1) cat(", ", file = file)
        dput2(x[[i]], file = file, control = control)
      }
      cat(")\n", file = file)
      invisible()
    } else {
      stop('S4 objects are only handled if they are contained within an S4 object or a list object')
    }
  }
  else .Internal(dput(x, file, opts))
}

dget2 = function(file) {  ## NOT USED?
  object = dget(file)
  assign(object@name, object, pos=1)
}