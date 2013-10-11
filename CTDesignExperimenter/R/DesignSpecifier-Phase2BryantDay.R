
## Class: Phase2BryantDaySpecifier
# Bryant and Day Phase II trial design is described in the article, Biometrics, v51, 1372-1383, 1995
setClass("Phase2BryantDaySpecifier",representation(N1Pats="numeric",NPats="numeric",Efficacy1LL="numeric",EfficacyLL="numeric",
                                                   NonTox1LL="numeric",NonToxLL="numeric"),
         contains="DesignSpecifier")
