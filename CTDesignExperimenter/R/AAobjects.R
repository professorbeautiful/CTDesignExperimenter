cat("=========    AAobjects  ============")
## objects that should be defined first.

scaffoldInsertSubTypes = cq(
  DesignParameter,
  DesignParameter,
  PatientAttribute,
  EligibilityCriterion,
  ,
  ScheduleTreatment,
  PatientAttribute,
  OffStudyCriterion,
  ModificationRule,
  PatientSummary,
  StoppingCriterion,
  TrialSummary,
  SimulationSummary  ## Same as evaluation criterion?
)