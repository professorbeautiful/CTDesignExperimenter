typeof(7)
#[1] "double"
is(7, "numeric")
#[1] TRUE
is(7, "integer")
# [1] FALSE
is.integer(7)
# [1] FALSE
is(7, "double")
# [1] FALSE
is.double(7)
# [1] TRUE
# Hmm, a little confusing.

identical(as(7, "double"), 7)
# [1] TRUE

is(as(7, "double"), "double")
# [1] FALSE
#OBOY. Hard to explain this to my python-loving friends.

# So "double" is not a class:
showClass("double")
# Error in getClass(Class) : “double” is not a defined class

# So, what does as(7, "double") do?
# Shouldnt it throw an error, since the Class arg is not a class name?
  class(as(7, "double"))
  #[1] "numeric
# Here is the story, inside as():
  if (.identC(Class, "double")) 
    Class <- "numeric"

showClass("numeric")
So we learn that unlike "double", "numeric" is a class; 
it has 2 subclasses:  integer and ordered. But not double.