
objectToString = functionToString = function(f) {
  if(is.character(f)) f = get(f)
  sink(con<-pipe("pbcopy"))
  print(f)
  sink()
  close(con)
  pbpaste()
}