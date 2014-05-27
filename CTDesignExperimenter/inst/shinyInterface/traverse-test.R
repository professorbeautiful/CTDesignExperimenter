
temp = tagList(div(hr(),"abc"))
temp[[1]] = tagAppendAttributes(temp[[1]], class="c")



as.vector(traverse(myTree, callback=4))


myTree[[(as.vector(
  traverse(myTree, searchTerm = "SummarizeSimulation")
)
)]]
traverse(myTree, searchTerm = "SummarizeSimulation", 
         callback=
           'list(level=level, index=as.vector(attr(tree, "index")))')
# myTree[[c(5 , 3,  1,  3,  1, 13,  3,  1,  1)]]  # "SummarizeSimulation"
# myTree[[c(5 , 3,  1,  3,  1, 12,  3,  1,  1)]]  # "SummarizeTrial"
## OK it will work.
myTree[[c(5 , 3,  1,  3,  1, 13,  3,  1,  2)]]  # (0)
# myTree[[c(5 , 3,  1,  3,  1, 13,  2 )]]  # list()

### Can I add a class or id attribute?  Need the
myTreeTemp = myTree  
myTreeTemp[[c(5 , 3,  1,  3,  1, 13)]]  # Full list item.
myTreeTemp[[c(5 , 3,  1,  3,  1, 13,  3)]]
locationVector = c(5 , 3,  1,  3,  1, 13)  # or equivalently,
locationVector = shorten(
  traverse(myTreeTemp, searchTerm = "^li$", 2)[[2]]
, 1)

myTreeTemp[[locationVector]] <-
  tagAppendAttributes(myTreeTemp[[locationVector]], class="CLASS")
#### YES!!!!  Success. We can add a class (or id)  attribute.

traverse(myTree, searchTerm = "\\(0\\)")
unlist(traverse(myTree, searchTerm = "\\(0\\)"))
traverse(myTree, searchTerm = "\\(1\\)", 
         callback=
           'list(level=level, index=as.vector(attr(tree, "index")))')
traverse(myTree, searchTerm = "\\(1\\)", callback=2)  #It checks!
traverse(myTree, searchTerm = "\\(1\\)", callback=3)

traverse(myTree, searchTerm = "vg_")
# Try replacing the 3 1 1 at the end to find the "<li" above.
myTree[[c(5, 3, 1, 3, 1, 1, 3, 1, 2, 3, 1, 1, 3, 1, 1)]]
#vg_SampleSizeMax_2
myTree[[c(5, 3, 1, 3, 1, 1, 3, 1, 2, 3, 1, 1, 3, 1, 2, 1)]] #ul
myTree[[c(5, 3, 1, 3, 1, 1, 3, 1, 2, 3, 1, 1, 3, 1, 2, 2)]] #ul
myTree[[c(5, 3, 1, 3, 1, 1, 3, 1, 2, 3, 1, 1, 2)]] # list()


#The data are in myTree[[5]].  1 2 and 3 are NOT empty...
# but not printing unless unlisted first
# myTree[[4]]
# <script>$(function() {$('#jstree1').jstree()})</script> 

# debug(traverse)
# 
# class(
#   myTree[[5]][[2]][[1]]  # jstree
#   myTree[[5]][[2]][[2]]  # ss-jstree
# )
# myTree[[5]][[3]][[1]][[1]] = "ul id='THIS-ID'"
# 
# 
# traverse(myTree)
# attr(traverse(myTree)[[186]], "index")
# 
