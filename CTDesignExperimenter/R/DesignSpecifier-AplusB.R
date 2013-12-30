cat("======== DesignSpecifier-AplusB.R  ================\n")

#####

##'  Class: APlusBSpecifier
##' "A+B with dose de-escalation" design was described in the article 
##' written by Lin in Biostatistics,v2,203-215,2001
##' The default object is 3+3 design specification
##' We use the notation A/B to mean A toxicity incidences out of B patients 
##' and >A/B to mean more than A toxicity incidences out of B patients. 
##' Similarly the notation A/B + <= C/D means that 
##' A toxicity incidences in the first cohort of B patients 
##' and no more than C toxicity incidences in the second cohort 
##' or both cohorts of D patients (depending on the specific design). 
##' 
##' Add A pts. 
##' 

#####
vg_ABCDE_design_parameters =   
  VariableGenerator(parameters=list(
    A_initial_cohort_size=3,
    B_additional_cohort_size=3,
    C_escalate=1, # if < C/A tox
    D_MTD_A=2, # if > D/A tox
    E_MTD_AplusB=2 #  if > E/(A+B) tox
  ),
  provisions=VariableList(vList=list(
    Variable(name="A", desc="initial cohort size", check=function(x) is.integer(x) & x>0),
    Variable(name="B", desc="additional cohort size", check=function(x) is.integer(x) & x>0),
    Variable(name="C", desc="escalate if < C/A tox", check=function(x) is.integer(x) & x>0),
    Variable(name="D", desc="MTD if > D/A tox", check=function(x) is.integer(x) & x>0),
    Variable(name="E", desc="MTD if > E/(A+B) tox", check=function(x) is.integer(x) & x>0)
  )),
  generatorCode=function(){
    list(A=A_initial_cohort_size,
      B=B_additional_cohort_size,
      C=C_escalate, # if < C/A tox
      D=D_MTD_A, # if > D/A tox
      E=E_MTD_AplusB #  if > E/(A+B) tox
    )
  }
  )

#####
##' A Parameter is a Variable that cannot change during a simulation 
##' 
##' It's not just a Variable with no requirements-- 
##' could be stochastic, as long as its variable is 
setClass("Parameter", contains="Variable")

# this VG should be executed by 
vg_dose_tiers =   
  VariableGenerator(parameters=list(),
                    requirements=NULL,
                    provisions=
                      doseTiers=1:5, doses=c(1,2,3,5,8)
)



vg_initialDoseThisPatient_ABCDE = 
  VariableGenerator(parameters=list(doseTiers=1:5, doses=c(1,2,3,5,8)),
                  requirements=VariableList(vList=list(
                    Variable(name="A", desc="initial cohort size", check=function(x) is.integer(x) & x>0),
                    Variable(name="B", desc="additional cohort size", check=function(x) is.integer(x) & x>0),
                    Variable(name="C", desc="escalate if < C/A tox", check=function(x) is.integer(x) & x>0),
                    Variable(name="D", desc="MTD if > D/A tox", check=function(x) is.integer(x) & x>0),
                    Variable(name="E", desc="MTD if > E/(A+B) tox", check=function(x) is.integer(x) & x>0)
                    )),
                  provisions=VariableList(
                    Variable(name="initialDoseThisPatient",
                             description="initial dose for this patient",
                             checkDataType=function(x){
                               x %in% parameters$doses
                             }
                    ), 
                    Variable(name="currentCohortSize", desc="currentCohortSize",
                             check=function(x) x %in% c(A, A+B))
                  ),
                  generatorCode=function(){
                    # Must access currentCTData
                    nTox = sum(sapply(currentCTData, patientHadToxicity))
                    if(currentTierSize==A){
                      if(nTox < C) {
                        initialDoseThisPatient = dose[currentDoseTier]
                      else if (nTox > D) endTrial()
                      else addEvent("generateNewPatient")
                    }
                    else {  #currentTierSize==A+B
                      if(nTox <= E) addEvent("tryToEscalate")
                      else endTrial()
                    }
                  }
  )



eg_MTDreachedThisPatient_ABCDE = 
  EventGenerator(parameters=list(doseTiers=1:5, doses=c(1,2,3,5,8)),
                 requirements=VariableList(vList=list(
                   Variable(name="A", desc="initial cohort size", check=function(x) is.integer(x) & x>0),
                   Variable(name="B", desc="additional cohort size", check=function(x) is.integer(x) & x>0),
                   Variable(name="C", desc="escalate if < C/A tox", check=function(x) is.integer(x) & x>0),
                   Variable(name="D", desc="MTD if > D/A tox", check=function(x) is.integer(x) & x>0),
                   Variable(name="E", desc="MTD if > E/(A+B) tox", check=function(x) is.integer(x) & x>0)
                 )),
                 provisions=ActionList(
                   Event(name="StopTrial",
                         description="StopTrial"),
                   Variable(name="MTD",
                            description="Maximum Tolerated Dose",
                               checkDataType=function(x){
                                 x %in% parameters$doses || x==Inf
                                 # Inf means MTD was not reached.
                               }
                   )
                 ), 
                 generatorCode=function(){
                   # Must access currentCTData
                   nTox = sum(sapply(currentCTData, patientHadToxicity))
                   if(currentTierSize==A){
                     if((nTox >= C) & (nTox > D)) return(TRUE)
                   }
                   else {  #currentTierSize==A+B
                     if(nTox > E) return(TRUE) # endTrial()
                   }
                   if(currentTier == max(doseTiers) ) 
                     return(TRUE)
                   return(FALSE)  #continue trial.
                 }
  )

ABCDEdesign = new("ListOfInserts", #(was "DesignSpecifier")
                  list(eg_MTDreachedThisPatient_ABCDE,
                       vg_initialDoseThisPatient_ABCDE
                  )
)

ABCDEdesign@parameters = list(
  A=3,B=3,C=1,D=1,E=1, 
  tierDoses=1:5)

A3plusB3design = ABCDEdesign

v_BinaryToxicity = new("Variable",
                       name="hadToxicity",
                       description="boolean; TRUE== experienced toxicity",
                       checkDataType=is.logical)


