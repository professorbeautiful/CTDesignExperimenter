cat("==========  gitUtilities =========\n")

writeSwapMeetFile = function(item,
                             commitIt=TRUE,
                             ...
){  
  path = makeItemFilePath(item)
  filename = gsub(".*/", "", path)
  item@filename = filename
  dput(item, file=path)
  if(commitIt) commitSwapMeetFile(filename, swapDir, ...)
  return(filename)
}

swapMeetDir = function(dir) {
  if(!missing(dir)) {
    assign('swapDir', dir, pos=1)
    return(dir)
  }
  else if(exists(x = "swapDir", where = 1))
    return(get("swapDir", pos=1))
  else {
    dir = "../CTDEswapmeet"  ## default
    assign('swapDir', dir, pos=1)
    return(dir)
  }
}

makeItemFileName = function(item) {
  if(!is(item, "SwapItem"))
    stop("makeItemFileName: item should be a SwapItem")
  if(item@description %in% "" | is.null(item@description)) 
    stop("item@description cannot be empty")  
  # NOTE: this sets the item's timestamp field.
  item@timestamp = Sys.time()
  #   and author field
  item@author = Sys.getenv("USER")
  prefix = switch(class(item),
                  VariableGenerator="I_" %&% item@outputVariable@name,
                  EligibilityCriterion="I_" %&% item@outputVariable@name,
                  Variable="V_" %&% item@name,
                  Scenario="S_" %&% item@name,
                  ListOfInserts="L_" %&% "ListOfInserts", 
                  Criterion="C_" %&% item@name,
                  "UNKNOWN_item_type")
  filename = paste0(prefix, "_",  
                    "_", as.numeric(item@timestamp), ".R")
  return(filename)
}

makeItemFilePath = function(item, dir) {
  return(swapMeetDir() %&% "/" %&% makeItemFileName(item))
}

writeInsertFile = function(insert, ...) {
  for(v in insert@requirements) {
    if(!file.exists(paste0(swapMeetDir(), v@filename))) {
      writeSwapMeetFile(v)
    }
  }
  provlist = ifelse(is.list(insert@provisions), 
                    insert@provisions, list(insert@provisions))
  for(v in insert@provisions)  {
    if(!file.exists(paste0(swapMeetDir(), v@filename))) {
      writeVariableFile(v, commitIt=TRUE)
    }
    writeSwapMeetFile(insert)
  }
}

writeScenarioFile = function(scenario,  ...) {
  if(missing(comment))
    comment = readline("Please write a comment for git (one line only): ")
  filenames = sapply(defaultScenario, writeInsertFile, commitIt=FALSE, ...)
  commitSwapMeetFiles(filenames, ...)
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
 pushSwapMeetFiles()
}


##################################

