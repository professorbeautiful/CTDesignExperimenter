This shows that environments are references,
and as args they are called-by-reference.

> a = new.env()
> a$x=1
> get("x", env=a)
[1] 1
> b=a
> get("x", env=b)
[1] 1
> b$x=2
> get("x", env=a)
[1] 2
> c=function(d) { d$x=3}
> c(b)
> b$x
[1] 3
> a$x
[1] 3
