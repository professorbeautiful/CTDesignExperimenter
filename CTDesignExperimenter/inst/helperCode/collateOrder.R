collateOrder = function(){
  strsplit(split=" ", 
           unlist(tail(n=1, 
                       read.delim
                       ("CTDesignExplorer/DESCRIPTION", header=F, 
                        stringsAsFactors=F))[[1]]))[[1]][-1]
}


for(f in collateOrder()) { 
  cat(".............", f, "...........\n"); 
  source(paste0("CTDesignExplorer/R/",f), keep.source=TRUE)
  ### keep.source does nothing useful for trace()
}

setdiff = function(In, notIn) { base:::setdiff(x=In, y=notIn)} ## so we know what's what.
setdiff(In=collateOrder(), notIn=dir(path="CTDesignExplorer/R")) 
setdiff(notIn=collateOrder(), In=dir(path="CTDesignExplorer/R"))