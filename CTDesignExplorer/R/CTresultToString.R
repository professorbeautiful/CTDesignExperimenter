
CTresultToString = function(thisCTresult=oneCTresult, showPatients=TRUE){
  #output = ifelse(!is.null(thisCTresult@CTTimes),  thisCTresult@CTTimes, "") ### NULL
  output = paste(sep="\n",
                 "Conclusions: " %&% paste(names(thisCTresult@Conclusions), 
                                           thisCTresult@Conclusions, sep="=", collapse=",  "),  
                 ("nPatients:    " %&% length(thisCTresult@PatsData) %&% "\n"))
  if(showPatients==TRUE)
    for(i in 1:length(thisCTresult@PatsData))
      output = paste(output, paste("  Patient ", i, ": \n", 
                                   patientToString(thisCTresult@PatsData[[i]])),
                     "\n")
  as.cat(output)
}
###patientToString(oneCTresult@PatsData[[2]])
### CTresultToString(oneCTresult)
