##   From help file for setMethod
## Need to define the track2 class first.  From setClass help.
## A simple class with two slots
# track2 <- setClass("track2",
#                   representation(x="numeric", y="numeric"))
setClass("track2",
                  representation(x="numeric", y="numeric"))
## an object from the class
# t1 <- track2(x = 1:10, y = 1:10 + rnorm(10))
t1 <- new("track2", x = 1:10, y = 1:10 + rnorm(10))
## A class extending the previous, adding one more slot
setClass("track2Curve",
                       representation(smooth = "numeric"),
                       contains = "track2")

## an object containing a superclass object
t1s <- new("track2Curve", t1, smooth = 1:10)

require(graphics)
## methods for plotting track2 objects (see the example for \link{setClass})
##
## First, with only one object as argument:
setMethod("plot", signature(x="track2", y="missing"),
          function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...)
)
## Second, plot the data from the track2 on the y-axis against anything
## as the x data.
setMethod("plot", signature(y = "track2"),
          function(x, y, ...) plot(x, slot(y, "y"), ...)
)
## and similarly with the track2 on the x-axis (using the short form of
## specification for signatures)
setMethod("plot", "track2",
          function(x, y, ...) plot(slot(x, "y"), y,  ...)
)
t1 <- new("track2", x=1:20, y=(1:20)^2)

tc1 <- new("track2Curve", t1)
slot(tc1, "smooth") <- smooth.spline(slot(tc1, "x"), slot(tc1, "y"))$y #$
plot(t1)
plot(qnorm(ppoints(20)), t1)
## An example of inherited methods, and of conforming method arguments
## (note the dotCurve argument in the method, which will be pulled out
## of ... in the generic.
setMethod("plot", c("track2Curve", "missing"),
          function(x, y, dotCurve = FALSE, ...) {
            plot(as(x, "track2"))
            if(length(slot(x, "smooth") > 0))
              lines(slot(x, "x"), slot(x, "smooth"),
                    lty = if(dotCurve) 2 else 1)
          }
)
## the plot of tc1 alone has an added curve; other uses of tc1
## are treated as if it were a "track2" object.
plot(tc1, dotCurve = TRUE)
plot(qnorm(ppoints(20)), tc1)

## defining methods for a special function.
## Although "[" and "length" are not ordinary functions
## methods can be defined for them.
setMethod("[", "track2",
          function(x, i, j, ..., drop) {
            x@x <- x@x[i]; x@y <- x@y[i]
            x
          })
plot(t1[1:5])
### end of help file
