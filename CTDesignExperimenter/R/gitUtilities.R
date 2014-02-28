cat("==========  gitUtilities =========\n")

#####

writeSwapMeetFile = function(object, prefix,
                             dir="../CTDEswapmeet",
                             author=system("echo $USER",intern=TRUE),
                             time=Sys.time(),
                             commitIt=TRUE,
                             ...
){
  filename = paste(prefix, ".", as.numeric(time), ".R", sep="")
  dput(object, file=paste0(dir, "/",filename))
  if(commitIt) commitSwapMeetFile(filename, dir, ...)
  return(filename)
}

writeVariableFile = function(variable, ...)
  writeSwapMeetFile(variable, prefix=paste0("v_", variable@name), ...)

writeVariableGeneratorFile = function(variableGenerator)
  writeSwapMeetFile(variableGenerator,
       prefix=paste0("vg_", variableGenerator@outputVariable@name),
      ...)

writeInsertFile = function(insert, ...) {
  for(v in insert@requirements)
    writeVariableFile(v, commitIt=TRUE)
  provlist = ifelse(is.list(insert@provisions), 
                    insert@provisions, list(insert@provisions))
  for(v in insert@provisions)  
    writeVariableFile(v, commitIt=TRUE)
  writeSwapMeetFile(insert,
        prefix=paste0("i_", insert@insertSubType, "_"), ...)
}

writeScenarioFile = function(scenario, comment, ...) {
  if(missing(comment))
    comment = readline("Please write a comment for git (one line only): ")
  filenames = sapply(defaultScenario, writeInsertFile, commitIt=FALSE, ...)
  commitSwapMeetFiles(filenames, comment=comment)
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

writeScenarioFile(defaultScenario, comment="Write inserts from defaultScenario")
pushSwapMeetFiles()



##################################

