require("CTDesignExperimenter")
require(shinysky) 
options(shiny.trace=FALSE)

scaffoldObjectNames = scaffoldObjects[[1]]

source("makeTree.R")


experimentTable = data.frame(sampleSize=NA)


