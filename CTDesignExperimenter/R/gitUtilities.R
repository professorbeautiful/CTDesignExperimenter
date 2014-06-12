cat("==========  gitUtilities =========\n")

writeSwapMeetFile = function(item,
                             commitIt=FALSE,
                             verbose=TRUE,
                             ...
){
  if(item@description %in% "" | is.null(item@description)) 
    stop("item@description cannot be empty")
  item = addItemFileName(item)
  #save(item, file=path)  
  ## dput does not handle S4 objects.
  ## save does it fine.
  ## However, when retrieved with "load()", 
  #  the object is assigned to the name "item"!
  # Therefore...
  #save(filename, file=path, ascii=FALSE)  
  dput2(x = item, file = makeItemFilePath(item), control="all")
  if(verbose) cat("writeSwapMeetFile: writing ", item@filename, "\n")
  if(commitIt) commitSwapMeetFiles(item@filename, swapMeetDir(), ...)
  return(item)
}

swapMeetDir = function(dir) {
  if(!missing(dir)) {
    assign('swapDir', dir, pos=1)
    return(dir)
  }
  else if(exists(x = "swapDir", where = 1))
    return(get("swapDir", pos=1))
  else {
    dir = "../CTDEswapmeet/"  ## default
    assign('swapDir', dir, pos=1)
    return(dir)
  }
}

addItemFileName = function(item, write=TRUE, commitIt=FALSE) {
  if(!is(item, "SwapItem"))
    stop("addItemFileName: item should be a SwapItem")
  prefix = switch(class(item),
                  VariableGenerator="I_" %&% item@outputVariable@name,
                  EligibilityCriterion="I_" %&% item@outputVariable@name,
                  Variable="V_" %&% item@name,
                  Scenario="S_" %&% item@name,
                  ListOfInserts="L_" %&% "ListOfInserts", 
                  Criterion="C_" %&% item@name,
                  "UNKNOWN_item_type")
  if(item@author == "") item@author = Sys.getenv("USER")
  if(is.na(item@timestamp)) item@timestamp = Sys.time()
  if(item@name == "NEED NAME")
    item@name = prefix
  filename = paste0(prefix, "_", item@author,
                    "_", as.numeric(item@timestamp), ".R")
  item@filename = filename
#   if(write)
#     if(!file.exists(makeItemFilePath(item))) {
#       writeSwapItemFile(item)
#     }
  return(item)
}

makeItemFilePath = function(item) {
  return(swapMeetDir() %&% "/" %&% item@filename)
}

writeVariableFile = function(var, commitIt=FALSE, ...) {
  writeSwapMeetFile(var, commitIt)
}

writeInsertFile = function(insert, commitIt=FALSE, ...) {
  if(is.list(insert@requirements)) 
    for(vNum in seq(along=insert@requirements)) 
      insert@requirements[[vNum]] = 
        writeVariableFile(insert@requirements[[vNum]])
  else if(is(insert@requirements, "Variable"))
    insert@requirements = writeVariableFile(
      insert@requirements)
  if(is.list(insert@provisions)) {
    for(vNum in seq(along=insert@provisions)) 
      insert@provisions[[vNum]] =
        writeVariableFile(insert@provisions[[vNum]])
    insert@outputVariable = insert@provisions[[1]]
  }
  else if(is(insert@provisions, "Variable")) {
    insert@provisions = writeVariableFile(
      insert@provisions)
    insert@outputVariable = insert@provisions[[1]]
  }
  writeSwapMeetFile(insert)
}

writeScenarioFile = function(scenario, commitIt=FALSE, ...) {
  for(insertName in names(scenario@inserts))  {
    scenario@inserts[[insertName]] = writeInsertFile(
      scenario@inserts[[insertName]])
  }
  writeSwapMeetFile(scenario, commitIt=commitIt)
}

seeSwapMeetFile = function(item) {
  if(is(item, "SwapItem"))
    name = item@filename
  else
    name = item
  system("cat " %&% swapMeetDir() %&% "/" %&% name)
} 

commitSwapMeetFiles = function(fileNames, dir="../CTDEswapmeet", comment) {
  if(missing(comment))
    comment = readline("Please write a comment for git (one line only): ")
  command = paste0("cd ", dir, "; git add ", paste(fileNames, collapse=" "), 
                "; git commit ", paste(fileNames, collapse=" "), " -m'", comment, "'")
  system(command)
  return(command)
}

pushSwapMeetFiles = function(dir="../CTDEswapmeet") {
  system(paste0("cd ", dir, "; git push"))
}

if(interactive()) {
 writeScenarioFile(defaultScenario, comment="Write inserts from defaultScenario")
 seeSwapMeetFile(.Last.value)
}


##################################

