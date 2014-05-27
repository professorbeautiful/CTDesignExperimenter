## traverse over tree using recursion

traverse = function(tree, 
                    callback=c(
                      'tree',
                      'as.vector(attr(tree, "index"))',
                      'list(level=level, index=as.vector(attr(tree, "index")), value=tree)',
                      'paste(paste(collapse=",", as.vector(attr(tree, "index"))), tree)'
                      )
                    , searchTerm=NULL, level=1, index=numeric(0)
) {
  if(is.numeric(callback))
    callback = eval(formals(traverse)$callback)[callback]
  callback = callback[1]
  if(is.list(tree)){
    answer = list()
    for(i in seq(along=tree)){
      nextanswer = traverse(tree[[i]], callback=callback,
                            searchTerm=searchTerm,
                            level=level+1, 
                            index=c(index,i)
                            )
     if(!is.null(nextanswer)) {
        attr( nextanswer, "index") = c(index,i)
        if(!is.null(searchTerm))
        #  return(nextanswer)
        answer = list(answer, nextanswer)
     }
    }
    #if(is.null(searchTerm))
    answer[sapply(answer, length) == 0] = NULL
    if(all(sapply(answer, is.null)) )
       return(NULL)
    if(is.list(answer) & length(answer) == 1)
      return(answer[[1]])
    return(answer)
  }
  else {
    attr( tree, "index") = index
    answer = try(eval(parse(text=callback)))
    if(class(answer) ==  "try-error")
      return(NULL)
    if(is.null(searchTerm))
      return(answer)
    if(length(grep(searchTerm, tree)) > 0) 
      return(answer)
    return(NULL)
  }
}
