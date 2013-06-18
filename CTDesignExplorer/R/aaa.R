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
