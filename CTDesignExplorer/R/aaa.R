

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
