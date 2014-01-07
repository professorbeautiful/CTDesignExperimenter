collateOrder = function(){
  strsplit(split=" ", 
           unlist(tail(n=1, 
                       read.delim
                       ("CTDesignExperimenter/DESCRIPTION", header=F, 
                        stringsAsFactors=F))[[1]]))[[1]][-1]
}

runCollatedFiles = function(){
for(f in collateOrder()) { 
  cat(".............", f, "...........\n"); 
  source(paste0("CTDesignExperimenter/R/",f), keep.source=TRUE)
  ### keep.source does nothing useful for trace()
}}

checkCollation = function(){
  setdiff = function(In, notIn) { base:::setdiff(x=In, y=notIn)} ## so we know what's what.
  setdiff(In=collateOrder(), notIn=dir(path="CTDesignExperimenter/R")) 
  setdiff(notIn=collateOrder(), In=dir(path="CTDesignExperimenter/R"))
}
