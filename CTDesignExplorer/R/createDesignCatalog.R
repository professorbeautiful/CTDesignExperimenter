cat("======== createDesignCatalog.R ================\n")

createDesignCatalog = function() {
  threePlusThree_five_tiers = 
    new("APlusBSpecifier", TierDoses=1:5)
  
  crm9 <- new("CRMSpecifier",
              InitialProbGuesses=c(0.1,0.2,0.3,0.4,0.5),
              TargetProb=0.3,
              SampleSize=21,
              NPatsPerCohort=3,
              StartingDoseLevel=3,
              TierDoses=1:5,
              InitialStageDoseLevels=NULL)
  
  p2bd = new("Phase2BryantDaySpecifier", 
             N1Pats=15, NPats=30, Efficacy1LL=2, EfficacyLL=8,
             NonTox1LL=2, NonToxLL=8)
  ########  There is no implementation of Phase2BryantDaySpecifier  yet.   ##########
  return(sys.frame(sys.nframe()))
}
# list2env(as.list(), .GlobalEnv) # fails
#for(obj i
# head(ls(envir=createDesignCatalog()))
# envTemp=createDesignCatalog()
# ls(envir=envTemp)
# ) 
#   assign(obj, get(obj, envir=createDesignCatalog()), pos=1)
