require(CTDesignExplorer)
require(RBioinf)
subClassNames(getClass("Specifier"))

# great;  how do you get distances?  This does it:
getClass("Specifier")

library("classGraph")
plot(class2Graph(subClassNames(getClass("Specifier"))))
cTree = classTree("Specifier")
edges(cTree)
nodes(cTree)
# neato, dots, twopi
plot(cTree, "twopi", attrs=list(node=list(
  #nodeheight=3.0,  #does nothing
  shape="rectangle",
  srt=400, #does nothing
  fontsize=40,
  fillcolor="green")))
Rgraphviz::plot(cTree)
getMethod("plot", "Ragraph")
?Ragraph
str(cTree)
cTree@edgeData@data[[1]]$weight = 5 

plot(cex.main=3, col.main="red", 
     cex.sub=3, col.sub="red", cTree, main="cTree")
str(  classTree("Specifier", all=T)  ## graphNEL )
      
      