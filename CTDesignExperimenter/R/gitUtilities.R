cat("==========  gitUtilities =========\n")

#####
writeVariable = function(variable, 
                             dir="../CTDEswapmeet",
                             author=system("echo $USER",intern=TRUE),
                             time=Sys.time()
){
  name = variable@name
  description = variable@description
  checkDataType = variable@checkDataType
  filename = paste("v_", name, ".", as.numeric(time), ".R", sep="")
  dput(variable, file=paste0(dir, "/",filename))
  commitVariableFile(filename, dir)
}

commitSwapFile = commitVariableFile = function(fileName, dir="../CTDEswapmeet", comment) {
  if(missing(comment))
    comment = readline("Please write a comment for git (one line only): ")
  system(paste0("cd ", dir, "; git add ", fileName, 
                "; git commit ", fileName, " -m'", comment, "'"))
}

pushSwapFiles = pushVariables = function(dir="../CTDEswapmeet") {
  system(paste0("cd ", dir, "; git push"))
}



##################################

