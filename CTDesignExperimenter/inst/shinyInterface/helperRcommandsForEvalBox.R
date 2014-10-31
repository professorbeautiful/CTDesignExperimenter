## To test reactivity, put this in the R evaluation box:
# This resets it, and tree refreshes:
rValues$currentScenario = defaultScenario


# OK, this works to test removing an insert:
isolate(rValues$currentScenario@inserts <<-             
          new('ListOfInserts', rValues$currentScenario@inserts[-1]))


## The two-step method seems to work fine:
temp <<- 
  new('ListOfInserts', rValues$currentScenario@inserts[-1])
#Then
rValues$currentScenario@inserts <<- temp

## One step does not work: 
rValues$currentScenario@inserts <<-             
  new('ListOfInserts', rValues$currentScenario@inserts[-1])
### NO GOOD:  if in the R box it keeps on repeating until all are gone.
# Changing myTree in reactive expression. Number of inserts is  10 
# Changing myTree in reactive expression. Number of inserts is  9 
# Changing myTree in reactive expression. Number of inserts is  8 
# Changing myTree in reactive expression. Number of inserts is  7 
# Changing myTree in reactive expression. Number of inserts is  6 
# Changing myTree in reactive expression. Number of inserts is  5 
# Changing myTree in reactive expression. Number of inserts is  4 
# Changing myTree in reactive expression. Number of inserts is  3 
# Changing myTree in reactive expression. Number of inserts is  2 
# Changing myTree in reactive expression. Number of inserts is  1 
# Changing myTree in reactive expression. Number of inserts is  0 

# But try this:
rValues$currentScenario@inserts <<-             
  isolate(new('ListOfInserts', rValues$currentScenario@inserts[-3]))
# Nope.  Try...
isolate(rValues$currentScenario@inserts <<-             
  new('ListOfInserts', rValues$currentScenario@inserts[-1]))
# OK, that works!


