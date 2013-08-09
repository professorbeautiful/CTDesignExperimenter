# patientToString,CTresultToString

patientToString = function(pat) {
  #    if(!is.null(pat$ID))
  as.cat(paste(
    "\t", paste(names(pat@BaseChars), pat@BaseChars, sep="=", collapse=",  "),
    "\n",
    "\t", paste(names(pat@ConcurrentTrtsDataList[[1]]@Outcomes), 
                pat@ConcurrentTrtsDataList[[1]]@Outcomes, sep="=", collapse=",  ")
  ))
}

