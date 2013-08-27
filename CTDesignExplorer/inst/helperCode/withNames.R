withNames =
function(x, n) {temp = data.frame(x=x,n=n);
                x = temp$x;
                n = temp$n;
                names(x) <- n; 
                x}