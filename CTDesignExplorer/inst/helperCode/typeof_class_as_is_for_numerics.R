typeof(7)
#[1] "double"

is(7, "numeric")
#[1] TRUE

class(7)
#[1] "numeric"

is(7, "double")
#[1] FALSE
is(7, "integer")
#[1] FALSE

is(as(7, "double"), "double")
# [1] FALSE
# OBOY

class(as(7, "double"))
#[1] "numeric

typeof(7L)
[1] "integer"