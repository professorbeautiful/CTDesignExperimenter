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

CTresultToString = function(thisCTresult=oneCTresult, showPatients=TRUE){
  #output = ifelse(!is.null(thisCTresult@CTTimes),  thisCTresult@CTTimes, "") ### NULL
  output = paste(sep="\n",
                 "Conclusions: " %&% paste(names(thisCTresult@Conclusions), 
                                           thisCTresult@Conclusions, sep="=", collapse=",  "),  
                 ("nPatients:    " %&% length(thisCTresult@PatsData) %&% "\n"))
  if(showPatients==TRUE)
    for(i in 1:length(thisCTresult@PatsData))
      output = paste(output, paste("  Patient ", i, ": \n", showAPatient(thisCTresult@PatsData[[i]])),
                     "\n")
  as.cat(output)
}
###patientToString(oneCTresult@PatsData[[2]])
### CTresultToString(oneCTresult)
