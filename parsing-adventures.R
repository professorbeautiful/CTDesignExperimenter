..=function().Last.value

parsed = parse(text="x=1;for(x in 1:5) b=4")
parsed[[1]][[2]]  ### x
is.name(parsed[[1]][[2]])  ### TRUE; x is a name.
is.language(parsed[[1]][[2]])  ### also TRUE!
## so should be possible to traverse and extract all names.
parsed[[2]][[3]][1]
str(..())
# language `:`()    Of course-- it's a function!
parse(text="x=1;for(x in 1:5) b=4")[[2]][[3]][1]
# `:`()
as.character(..())
# [1] ":"
parsed[[2]][[1]]  ### `for`
is.name(..())  ### also a name!
parsed[[2]][[1]]  ### `for`


### nice try... won't work unless we throw away 
### basic language elements and things from base,
#### like `for`
is.function(`for`)
#[1] TRUE
extractNames = function(ex) {
  if(length(ex)==1){
    cat("ex=", ex, "\n")
    if(is.name(ex) 
       & !(find(as.character(ex))=="package:base"))
      return(try(paste("name:", as.character(ex))))
    else return(NULL)
  }
  cat("Calling extractNames on \n")
  print(ex)
  cat(" with length ",   length(ex), "\n")
  return(unlist(sapply(ex, extractNames)))
}
extractNames(parse(text="x=1;for(x in 1:5) b=4"))