collateOrder = function(){
  strsplit(split=" ", 
           unlist(tail(n=1, 
                       read.delim
                       ("CTDesignExplorer/DESCRIPTION", header=F, 
                        stringsAsFactors=F))[[1]]))[[1]][-1]}


for(f in collateOrder()) { 
  cat(".............", f, "...........\n"); 
  source(paste0("CTDesignExplorer/R/",f))}