###  Problem (bug?) 
###  the names of a .Data list are lost,
###  if either subclassed or adding a slot.

```{r}
setClass("TempA", contains="list")
A = new("TempA", list(a=1,b=2))
A   ### Does not show the list names.
A[["b"]]  ## Nevertheless, you can extract by name.
names(A)  ## And this extracts the names.
A@.Data  ## But there are no names here. Where are they hiding?
```


But if we subclass, the names are nowhere.
```{r}
setClass("TempB", contains="TempA")
B = new("TempB", list(a=1,b=2))
B  ## no names this time.
names(B) ## no names.
B[["b"]] ## NULL
B@.Data  ## no names
```
Does this different approach do it?  Nope.
```{r}
B2 = new("TempB", new("TempA", list(a=1,b=2)))
B2  ## no names
B2[["a"]]  # NULL
names(B2) # NULL
as(B2, "TempA")  ## still no dice
names(as(B2, "TempA"))  ## still no dice
B2@.Data  ### Of course, no names.

### Adding slots destroys the named list aspect
### Wait!  It's working now!
setClass("TempC", contains="list",
         slots=c(Dslot="numeric"))
D = new("TempC", list(a=1,b=2), Dslot=99)
D[["a"]]
names(D)
