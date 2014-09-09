myListToTags <- function(myList, parent=tags$ul(),
  prefix="", addLevelClass=TRUE, addLevelType=TRUE, addtreePath=TRUE, level=0, treePath="0") 
{
  # Handle parent tag attributes
  attribJSON <- getJSON(myList)
  parent <- tagAppendAttributes(parent, `data-jstree`=attribJSON)
  
  # There's probably an *apply way to do this. Whatevs.
  for (ind in 1:length(myList)){
    name <- names(myList)[ind]
    if (is.null(name)){
      name <- ""
    }
    
    attribJSON <- getJSON(myList[[ind]])
    
    level=level+1
    treePath = paste0(prefix, treePath, "_", ind)
    if (is.list(myList[[ind]]))
      childList = tags$li(name, myListToTags(
        myList[[ind]],
        prefix=prefix,
        addLevelClass=addLevelClass,
        addLevelType=addLevelType,
        addtreePath=addtreePath,
        level=level,
        treePath=treePath)
        , `data-jstree`=attribJSON
      )
    else 
      childList = tags$li(name, `data-jstree`=attribJSON)
    if(addLevelClass)
      childList <- tagAppendAttributes(childList, class=paste0(prefix, "treeclass_", level))
    if(addLevelType) {
      childList <- tagAppendAttributes(childList, type=paste0(prefix, "level_", level))
      childList <- tagAppendAttributes(childList, rel=paste0(prefix, "level_", level))
    }
    if(addtreePath)
      childList <- tagAppendAttributes(childList, treePath=treePath)
    parent <- tagAppendChild(parent, childList)
  }
  return(parent)
}
assignInNamespace("listToTags", value = myListToTags, ns = "shinyTree")

getJSON <- function(node){
  attrib <- NULL
  
  # Handle 'opened' attribute
  opened <- attr(node, "stopened")
  if (!is.null(opened) && opened){
    attrib <- c(attrib, "\"opened\": true")
  }
  
  # Handle 'selected' attribute
  selected <- attr(node, "stselected")
  if (!is.null(selected) && selected){
    attrib <- c(attrib, "\"selected\": true")
  }
  
  # Handle 'disabled' attribute
  disabled <- attr(node, "stdisabled")
  if (!is.null(disabled) && disabled){
    attrib <- c(attrib, "\"disabled\": true")
  }
  
  # Handle 'icon' attribute
  icon <- attr(node, "sticon")
  if (!is.null(icon)){
    icon <- paste0("fa fa-",icon)
    attrib <- c(attrib, paste0("\"icon\": \"", icon, "\""))
  }
  
  paste0("{",paste(attrib, collapse = ","),"}")  
}


library(shiny)
openedNode = function(theNode) {
  structure(theNode, stopened=TRUE)
}
newData =     list(
  root1 = structure(list(StuffInRoot1="StuffInRoot1Text"), stselected=TRUE, sticon="signal"),
  root2 = structure(
    list(
      SubListA = 
        openedNode(list(leaf1 = "", leaf2 = "", leaf3="")),
      SubListB = structure(list(leafA = "", leafB = ""), stdisabled=TRUE)
    ),
    stopened=TRUE
  )
)
getJSON = shinyTree:::getJSON

temp1 = capture.output(shinyTree:::listToTags(newData))
temp2 = capture.output(myListToTags(newData))
temp3 = capture.output(myjstree.obj(newData))
identical(temp1, temp2)
identical(temp1, temp3)
temp3
temp1
