CT-design-evaluator
===================

A social network for clinical trial designers, using RStudio shiny, to promote sharing and evaluation of old and new design ideas. 

The fundamental functionality is to support clinical trials designers to:  
* Explore search and view the previously catalogued and shared objects.
* Create and contribute a new object.
* Specify and simulate a clinical trial scenario.
* Specify an experimental design for comparing a set of design, population
models, and outcome models.
* Conduct the experiment by running multiple clinical trial simulations for
each experiment.
* Calculate and view the criteria.

A biostatistician designing a trial ...
* ... can compare several designs,
* ... can compare several designs on multiple criteria,
* ... can test the robustness of a design upon varying assumptions about
the patient pool,
* ... can test the robustness of a design upon varying assumptions about
how the outcome relates to treatment at an individual basis,
alternative models of the patient pool or the outcome, from utterly
simple to wildly complex, can be swapped in or augmented freely.

A design innovator can...
* ... port the design to this evaluation platform, or encourage a graduate
student to do so,
*make his/her new design easy to evaluate in an unlimited set of
contexts,
* ... enable and encourage others to extend the new design.

This package is under development, with two active branches. The commit with tag v0.5, from August 2013, demonstrated viewing classes and instances within a browser using [shiny](http://www.rstudio.org/shiny), and running and displaying one clinical trial. The package name was CTDesignExplorer. 

The organization of the user-selectable elements has undergone a major refactoring in the period Sept 2013 - Jan 2014. Further information on the previous version is later in this README.

Current development version: CTDesignExperimenter
----
The development version provides scaffolding for the event queue that drives the simulation. The names of the scaffolding events are:
* BeginSimulation
* BeginClinicalTrial
* GeneratePatient
* CheckEligibility 
* EnrollPatient
* AssignTreatmentPlan
* GenerateOutcomes,
* CheckOffStudy
* CheckModifications
* SummarizePatient
* CheckStoppingRules
* SummarizeTrial
* SummarizeSimulation

For each of these, users can insert  *insert* objects in the form of ActionGenerators or VariableGenerators. Each scaffolding event knows how to handle its inserts. The order of events allows conditional returning and branching. The collection of inserts is the new concept of a scenario. Unlike v0.5, there is no longer a rigid division between PopulationModel, Design, and OutcomeModel.

To check out (and test, please!) the current state of the development program, install the package, and call runTrial().

**ACCOMPLISHED**
* Highly flexible scaffolding of events in clinical trials simulation.
* Automated formation of VariableNetworks from a collection of VariableGenerators, checking of network compatibility, and instantiation of VariableValues.
* Automated push of new Variable objects to the "CTDEswapmeet".
* A _shiny_ interface for viewing objects and running a trial.


**TODO**
* Adapt the _shiny_ interface to the new event queue architecture.
* Greatly expand the set of example inserts in the CTDEswapmeet.
* Yada yada.

Previous version v0.5: CTDesignExplorer
----
The previous design divided components into PopulationModel, Design, OutcomeModel, and EvaluationCriterion.  One Scenario would consist of one PopulationModel, one Design, and one OutcomeModel. A trial run would create synthetic patients according to the PopulationModel, assign treatments according to the Design, create outcomes for patient according to the OutcomeModel, and evaluate each scenario with EvaluationCriterion objects, making comparisons across the scenarios. Results are reported in the doctoral dissertation of Yuanyuan Wang. Within a _shiny_ interface, one could view available instances of  PopulationModel, Design, and OutcomeModel, select one of each, and run it. Although it worked, this approach was too rigid to be useful.

