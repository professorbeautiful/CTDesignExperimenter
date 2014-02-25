CT-design-evaluator
===================

A social network for clinical trial designers, using RStudio shiny, to promote sharing and evaluation of old and new design ideas.

This has undergone a major refactoring in the period Sept 2013 - Jan 2014.
The previous design divided components into PopulationModel, Design, OutcomeModel, and EvaluationCriterion.  One Scenario would consiste of one of each. A trial run would create synthetic patients according to the PopulationModel, assign treatments according to the Design, create outcomes for patient according to the OutcomeModel, and evaluate each scenario with EvaluationCriterion objects, making comparisons across the scenarios. Results are reported in the doctoral dissertation of Yuanyuan Wang. Within a _shiny_ interface, one could view available instances of  PopulationModel, Design, and OutcomeModel, select one of each, and run it. Although it worked, this approach was too rigid to be useful. This development terminated in the branch _reqs-provs_, which is not maintained.

To achieve the flexibility and ease of use.
