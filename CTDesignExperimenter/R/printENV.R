printENV = function(ENV = trialData){
  #ENV = trialDataAugmented
  invisible(sapply(ls(env=ENV), 
                   function(ob) {
                     theOb = get(ob, env=ENV)
                     if(class(theOb)=="environment")
                       cat(ob, ": ==> environment ", "\n") 
                     else if(class(theOb)=="list")
                       cat(ob, ": ==> list ", "\n") 
                     else if(class(theOb)=="VariableValue") 
                       cat(ob, "(VV): ", theOb@.Data, "\n") 
                     else cat(ob, ": ", theOb, "\n")
                   }
  ))
}