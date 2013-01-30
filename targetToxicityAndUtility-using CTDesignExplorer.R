#### Status of this file:  in progress!!!

###  Possible paper:  remarks on the phase 1 target toxicity idea.
#
##  Claim:  the target toxicity idea makes sense if there is a prior association between toxicity dose=response and efficacy dose-response.
# 	We define the prior using a 1 PK + 2 PD model.

#### PRIORS
#### REPLICATE from prior


####  We will construct a prior for universes.
####  The clinical trialist is imagined to have this prior,
####	within which reaching a 0.33 pr(Toxicity) is "good" in some sense:
####   maximizing the chance of reporting out a dose with a good EU.

####	We may stiplulate that the toxicity curve applies to the Phase I patients,
####	and both tox and response curves apply to Phase II patients.

####	You have to break symmetry somewhere.
####	Either Rt is better than rT is bad,
####	or else Pr(response) is generally bigger thab Pr(toxicity).

####   POPULATION MODEL
makePeopleData = function(universe, plot=TRUE){
	gPK = rbinom(p=pPK, n=nPeople, size=1)
	gPDt = rbinom(p=pPDt, n=nPeople, size=1)
	gPDe = rbinom(p=pPDe, n=nPeople, size=1)
	### PK model
	logthetaPK = rnorm(n=nPeople, 
		muPK + gPK*betaPK, sdPK)
	### tox PD model
	logthetaPDt = rnorm(nPeople, 
		muPDt + gPDt*betaPDt + logthetaPK*betaPDt, sdPDt)
	### efficacy PD model
	logthetaPDe = rnorm(nPeople, muPDe + gPDe*betaPDe+ 	logthetaPK*betaPDe, sdPDe)

	### Outcome model
	thetaPDe = 10^(logthetaPDe)   ## patient's threshold for efficacy
	thetaPDt = 10^(logthetaPDt)   ## patient's threshold for toxicity
	efficacyOutcome = outer(doses, thetaPDe, ">")
	toxicityOutcome = outer(doses, thetaPDt, ">")
	p.rt = apply(!efficacyOutcome & !toxicityOutcome, 1, mean)
	p.Rt = apply(efficacyOutcome & !toxicityOutcome, 1, mean)
	p.rT = apply(!efficacyOutcome & toxicityOutcome, 1, mean)
	p.RT = apply(efficacyOutcome & toxicityOutcome, 1, mean)
	p.T = p.rT + p.RT
	plot(doses, p.rt, type="l", log="x")
	lines(doses, p.Rt, type="l", col=2)
	lines(doses, p.rT, type="l", col=3)
	lines(doses, p.RT, type="l", col=4)
	probabilities = rbind(p.rt, p.Rt, p.rT, p.RT)
	EU = c(U4 %*% probabilities )
	probabilities = rbind(probabilities, EU)
	lines(doses, EU, type="l", col=5, lwd=3)
	legend(1e-2, 0.4, legend=rownames(probabilities),  lwd=c(rep(2,4), 5), col=1:5)
	#return(rbind(probabilities, p.T=p.T, EU=EU))
	return(as.data.frame(t(rbind(probabilities, p.T=p.T, EU=EU))))
}

nUniverses = 1
universe = data.frame(
	pPK		= rep(0, nUniverses),   ##  Pr(being in group 2 for PK heterogeneity)
	muPK	= rnorm(nUniverses, 1, 10), ## Mean log of output of PK process (group 1)
	betaPK	= rep(1, nUniverses),	## Change in log of output of PK process (group 2)
	sdPK	= rep(1, nUniverses),	## ## SD of log of output of PK process (either group)
	#######  The PK process is the "concentration", input into the PDt and PDe processes.
	pPDt	= rep(0, nUniverses),   ##  Pr(being in group 2 for PDt heterogeneity
	muPDt	= rep(1, nUniverses),
	betaPDt= rep(1, nUniverses),
	sdPDt	= rep(1, nUniverses),
	#######  We assume that efficacy and toxicity are independent conditional on concentration.
	#######  There will be a threshold
	#######	We may want to assume a refractory probability too.
	pPDe	= rep(0.5, nUniverses),   ##  Pr(being in group 2 for PDe heterogeneity
	muPDe	= rep(1, nUniverses),
	betaPDe= rep(-1, nUniverses),
	sdPDe	= rep(1, nUniverses)
)
#### "PK" represents ANY shared factor causing the two dose-response curves to move up or down together.


#### PATIENT POP MODEL
####	Alternative to simulating outcomes:
####   use meihua's code to derive the four probabilities from the joint distribution of (thetaPDt, thetaPDe), and the expected utility.

###	Patient simulation method:
nPeople = 1000   ### per universe
### We create a set of matrices, nPeople * nUniverses
### patient heterogeneity model.

####	DOSES
doses = 10^seq(-4, to=4, length=12)

####	UTILITIES
####  Define the utility function for outcomes.
U4 = c(rt=0, Rt=1, rT=-0.5, RT=0)

peopleData = lapply(1:nUniverses, function(i) 
	makePeopleData(universe[i, ])
)  ## end "lapply"


####	Finally, plot the expected utility as a function of Pr(tox | dose). 
####	Claim:  with reasonable priors, this will be optimized at a reasonable target Pr(tox|dose).
####	In particular, variation in the prior for logthetaPK will entwine the 2 curves, so that toxicity becomes informative about efficacy.

#whichToPlot = "p.RT"

require(tcltk)
tkWindow <<- tktoplevel()

plot(range(doses), c(0,0), pch=" ", xlab="dose", ylim=c(-1,1), ylab= "", log="x")
for(i in 1:nUniverses) points(doses, peopleData[[i]][ , "EU"], pch="u",
	col=1:length(doses))
for(i in 1:nUniverses) points(doses, peopleData[[i]][ , "p.rt"], pch="0",
	col=1:length(doses))

