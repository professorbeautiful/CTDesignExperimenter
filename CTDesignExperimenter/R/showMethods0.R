### if f is not provided, the entire list of all generics is obtained by getGenerics().
###  But when you feed in each one, using Recall, fdef is by default 
###  getGeneric(f, where=topenv(parent.frame())) 

# showMethods(class="Action", where="package:CTDesignExplorer")  #### works; finds the methods in the package
# showMethods(class="Action", where=topenv(parent.frame()))  #### works; finds nothing
# showMethods(class="Action")  #### fails; dumps lots of S4 generics as if they were not.

showMethods0 =
function (f = character(), .where = topenv(parent.frame()), classes = NULL, 
          includeDefs = FALSE, inherited = !includeDefs, showEmpty, 
          printTo = stdout(), fdef = getGeneric(f, where = .where),
          verbose=FALSE) 
{
  if(verbose){
    catn("showMethods0 ", f)
    sc = (sys.call())
    print(sc)
    if(length(f) > 0)
      getGeneric(f, where = .where)  ## you cannot print fdef.
    if(missing(.where)) catn(".where is missing")
    else print(.where)
  }
  if (missing(showEmpty)) 
    showEmpty <- !missing(f)
  if (identical(printTo, FALSE)) 
    con <- textConnection(NULL, "w")
  else con <- printTo
  if (is(f, "function")) {
    fdef <- f
    if (missing(.where)) 
      .where <- environment(f)
    f <- deparse(substitute(f))
    if (length(f) > 1L) 
      f <- paste(f, collapse = "; ")
  }
  if (!is(f, "character")) 
    stop(gettextf("first argument should be the names of one of more generic functions (got object of class %s)", 
                  dQuote(class(f))), domain = NA)
  if (length(f) == 0L) {
#    f <- if (missing(.where))  ### This is the bug. If missing, should use the default value.
#      getGenerics()
#    else getGenerics(.where)
    f <- getGenerics(.where)
    
  }
  if (length(f) == 0L) 
    cat(file = con, "no applicable functions\n")
  else if (length(f) > 1L) {
    for (ff in f) {
      #ffdef <- getGeneric(ff, where = .where)
      ffdef <- getGeneric(ff, where = .where)
      if (missing(.where)) {
        if (isGeneric(ff)) 
          Recall(ff, classes = classes, includeDefs = includeDefs, 
                 inherited = inherited, showEmpty = showEmpty, 
                 printTo = con, fdef = ffdef)
      }
      else if (isGeneric(ff, .where)) {
        Recall(ff, .where = .where, classes = classes, 
               includeDefs = includeDefs, inherited = inherited, 
               showEmpty = showEmpty, printTo = con, fdef = ffdef)
      }
    }
  }
  else {
#     out <- paste0("\nFunction \"", f, "\":\n")
#     if (!is(fdef, "genericFunction")) 
#       cat(file = con, out, "<not an S4 generic function>\n")
#     else 
    if (is(fdef, "genericFunction")) {
      out <- paste0("\nFunction \"", f, "\":\n")
    .showMethodsTable(fdef, includeDefs, inherited, 
                           classes = classes, showEmpty = showEmpty, printTo = con)
    }
  }
  if (identical(printTo, FALSE)) {
    txtOut <- textConnectionValue(con)
    close(con)
    txtOut
  }
  else invisible(printTo)
}
environment(showMethods0) = environment(showMethods)
# undebug(showMethods0)
showMethods0(classes="Action", verbose=F)