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
    dir = Sys.getenv("HOME") %&% "/CTDEswapmeet/"  ## default
    if(!file.exists(dir)) dir.create(dir)
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
    for(vNum in seq(along=insert@requirements)) {
      if(insert@requirements[[vNum]]@filename == "")
        insert@requirements[[vNum]] = 
          writeVariableFile(insert@requirements[[vNum]])
      else if (!file.exists(insert@requirements[[vNum]]@filename))
        warning("writeInsertFile:  the file " 
                %&% insert@requirements[[vNum]]@filename
                %&% "does not exist.")
    }
  else if(is(insert@requirements, "Variable")) {
    if(insert@requirements@filename == "")
      insert@requirements = writeVariableFile(insert@requirements)
    else if (!file.exists(insert@requirements@filename))
      warning("writeInsertFile:  the file " 
              %&% insert@requirements@filename
              %&% "does not exist.")
  }
  if(is.list(insert@provisions)) {
    for(vNum in seq(along=insert@provisions)) {
      if(insert@provisions[[vNum]]@filename == "")
        insert@provisions[[vNum]] = 
        writeVariableFile(insert@provisions[[vNum]])
      else if (!file.exists(insert@provisions[[vNum]]@filename))
        warning("writeInsertFile:  the file " 
                %&% insert@provisions[[vNum]]@filename
                %&% "does not exist.")
    }
    insert@outputVariable = insert@provisions[[1]] #temporary
  }
  else if(is(insert@provisions, "Variable")) {
    if(insert@provisions@filename == "")
      insert@provisions = writeVariableFile(insert@provisions)
    else if (!file.exists(insert@provisions@filename))
      warning("writeInsertFile:  the file " 
              %&% insert@provisions@filename
              %&% "does not exist.")
    insert@provisions = writeVariableFile(
      insert@provisions)
    insert@outputVariable = insert@provisions
  }
  writeSwapMeetFile(insert)
}

writeScenarioFile = function(scenario, commitIt=FALSE, ...) {
  for(insertName in names(scenario@inserts))  {
    if(scenario@inserts[[insertName]]@filename == "")
      scenario@inserts[[insertName]] = writeInsertFile(
        scenario@inserts[[insertName]])
  }
  names(scenario@inserts@.Data) = 
    sapply(scenario@inserts@.Data, slot, "filename")
  ## Then... surprise!
#   > names(latestScenario@inserts@.Data) 
#   NULL
#   > names(latestScenario@inserts) 
#   [1] "I_liverFunction_Roger_1404934528.80031.R"         
#   [2] "I_age_Roger_1404934528.79521.R"     ...
  
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

latestScenarioName <- function() 
  rev(dir(swapMeetDir(), pattern="^S_"))[1]

loadLatestScenario = function(setCurrent=TRUE, verbose=FALSE){
 if(verbose) 
   catn("swapMeetDir()", swapMeetDir(), "  ", "latestScenarioName:", latestScenarioName())
  if(!is.na(latestScenarioName())) {
    latestScenarioFile <<- swapMeetDir() %&% latestScenarioName()
    latestScenario <- dget(latestScenarioFile)
    catn(class(latestScenario))
    names(latestScenario@inserts@.Data) <- sapply(latestScenario@inserts@.Data,
                                                 slot, name="filename")
    assign("latestScenario",  latestScenario, pos=1)
    if(setCurrent)
      currentScenario <<-latestScenario
  }
  invisible()
}

loadScenarioInserts = function(verbose=TRUE){
  foreach(insert = currentScenario@inserts) %do% {
    if(verbose) catn("Loading " %&% insert@name)
    assign(insert@name, insert, pos=1)
  }
}




