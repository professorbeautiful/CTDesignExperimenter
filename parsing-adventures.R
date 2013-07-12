..=function().Last.value

parse(text="x=1;for(x in 1:5) b=4")[[1]][[2]]  ### x
is.name(..())  ### TRUE; x is a name.
## so should be possible to traverse and extract all names.
parse(text="x=1;for(x in 1:5) b=4")[[2]][[3]][1]
str(..())
# language `:`()
parse(text="x=1;for(x in 1:5) b=4")[[2]][[3]][1]
# `:`()
as.character(..())
# [1] ":"