jsonUL = function(input)  '[ ' %&% input %&% ' ]'

jsonList = function(input) {

}

appendTreeAttributes = function(a, prefix, level, pathAttr){
        a <- jsonAppendAttributes(a, class=paste0(prefix, "treeclass_", level))
        a <- jsonAppendAttributes(a, type=paste0(prefix, "level_", level))
        a <- jsonAppendAttributes(a, rel=paste0(prefix, "level_", level))
        a <- jsonAppendAttributes(a, pathAttr=pathAttr)
      return(a)
}


myjstree.JSON = 
  function (x, prefix="", addLevelClass=TRUE, addLevelType=TRUE, addPathAttr=TRUE, level=0, pathAttr="0") 
  {
    INDENT = function(nSpaces, spacing=3) paste0(rep(" ", nSpaces * spacing), collapse="")
    handleListItem <- function(ind, theList, prefix, level, pathAttr) {
      # We process item #ind in theList. We pass the whole list to get the names.
      pathAttr = paste0(prefix, pathAttr, "_", ind)
      theItem = theList[[ind]]
      # If theItem is a list, its name in the list is the text
      nodeText = if( (is.list(theItem))) { names(theList)[[ind]]} else theList[[ind]]
      nodeText = gsub('"', '\\\\\"', nodeText)
      nodeString = #'\n' %&% 
        INDENT(level) %&% ' { "text" : "'    %&% nodeText    %&%    '"'
      nodeString = nodeString %&% 
        ', "li_attr" : { ' %&%
        '"rel" : "' %&% paste0(prefix, 'level_', level) %&% '", ' %&%
        '"type" : "' %&% paste0(prefix, 'level_', level) %&% '", ' %&%
        '"pathAttr" : "' %&% paste0(prefix, pathAttr) %&% '"' %&%
        #'class : "' %&% paste0(prefix, 'treeclass_', level) %&% '"' %&%
        ' }'
      if(is.list(theItem)) {
        nodeString = nodeString %&%
          paste(collapse=",",
                myjstree.JSON(theItem, 
                              prefix=prefix,
                              addLevelClass=addLevelClass,
                              addLevelType=addLevelType,
                              addPathAttr=addPathAttr,
                              level=level+1,
                              pathAttr=pathAttr
                )) 
      }
      nodeString = nodeString %&% ' }'
      # a <- appendTreeAttributes(a, prefix, level, pathAttr)
      return(nodeString)
    }  ## end of handleListItem()
    
    if (is.list(x)) {
      
      indSeq <- seq(along=x)
      res <- paste(collapse=", ", 
                   lapply(indSeq, handleListItem, theList=x, 
                          prefix=prefix, level=level+1, pathAttr=pathAttr))
      returnval = paste(res, collapse = ",")
      returnval = '[ '       %&% returnval        %&% ' ]'
      if(level != 0)
        returnval = #'\n' %&% 
          INDENT(level) %&% 
          ' , "state" : {   "opened" : true},' %&% #'\n' %&% 
          INDENT(level) %&% 
          '  "children" :  '       %&% 
        returnval  
      return(returnval)
    }
    else {
      return('"' %&% x %&% '"')
    }
  }

reducedScenario = currentScenario
reducedScenario@inserts = new("ListOfInserts", reducedScenario@inserts)

pbcopy(#gsub("'", "", substring(first = nchar("\n  'children' : "),
    myjstree.JSON(
      makeTree(scenario=reducedScenario, "full")
      ) 
    )