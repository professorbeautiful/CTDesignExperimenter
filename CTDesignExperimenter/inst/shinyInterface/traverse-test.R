# The world needs a treeApply!!

  
(traverse(myTree, callback=1))  #### abandoned the callback approach

traverse(myTree, searchTerm = "SummarizeSimulation")
myTree[[(as.vector(
  traverse(myTree, searchTerm = "SummarizeSimulation", 2)
)
)]]  ### Found by index vector.

# traverse(myTree, searchTerm = "SummarizeSimulation", 
#          callback=
#            'list(level=level, index=as.vector(attr(tree, "index")))')
# myTree[[c(5 , 3,  1,  3,  1, 13,  3,  1,  1)]]  # "SummarizeSimulation"
# myTree[[c(5 , 3,  1,  3,  1, 12,  3,  1,  1)]]  # "SummarizeTrial"
## OK it will work.
myTree[[c(5 , 3,  1,  3,  1, 13,  3,  1,  2)]]  # (0)
# myTree[[c(5 , 3,  1,  3,  1, 13,  2 )]]  # list()

myTreeObj[[c( 3,  1, 13,  3,  1,  1)]] 
# Remove the first 3 if using myTreeObj.
myTreeObj[[c( 3,  1, 13)]] # This is the entire node.

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

### Here we assign the vg_ class to each VG node.
myTreeTemp = myTree  

vgHits = traverse(myTree, searchTerm="vg_")
for(i in seq(along=vgHits)) {
  locationVector = shorten(vgHits[[i]], 3)
  myTreeTemp[[locationVector]] <-  #not doubleheaded
    tagAppendAttributes(myTreeTemp[[locationVector]], class="vg_")
}
myTreeTemp[[shorten(vgHits[[1]], 3)]]

### Here we assign the BLOCK class to each block node.
whichAreBlocks = which(sapply(traverse(myTreeObj), length) == 6)
for(i in whichAreBlocks) {
  locationVector = print( traverse(myTreeObj)[[i]] )
  myTreeObj[[locationVector]] <-  #not doubleheaded
    tagAppendAttributes(myTreeObj[[locationVector]], class="BLOCK")
}


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
