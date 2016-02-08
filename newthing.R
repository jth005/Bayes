# Load libraries
source('bugsAuxil.r')
library(nlme)

# a GLMM example courtesy of Andy Houseman

antilogit = function(u) 1-1/(1+exp(u))


Sibs = c(rep(2,600),rep(1,600))
NFam = length(Sibs)

FamID = unlist(sapply(1:NFam, function(i) rep(i,Sibs[i])))
N = length(FamID)

# generate data
Reff = rnorm(NFam)*4

SES = rnorm(NFam)
Edu = rbinom(N,1,antilogit(1.2 + SES[FamID]*2 + Reff[FamID]))
Dep = 10 + 5*SES[FamID] + 3*Edu + 2*Reff[FamID] + rnorm(N,0)*1.5


SESi = SES[FamID]
imod2 = lme(Dep~Edu+SESi, random=~1|FamID)
imod1 = glm(Edu~I(SES[FamID])+I(imod2$coef$rand$FamID[FamID]), family=binomial())

# write data and inits to files
bugsData(data= list(
  N=N, XY=matrix(SES[FamID]), XZ=matrix(SES[FamID]), Z=Edu, Y=Dep, dY=1, dZ=1,
  FamID=FamID, NFam=NFam),digits=6,fileName='data.txt')

bugsInits(list(list(gamma0=as.vector(imod1$coef[1]),
                    gamma=as.vector(imod1$coef[2]),
  beta0=as.vector(imod2$coef$fixed[1]), 
  betaZ=as.vector(imod2$coef$fixed[2]), beta=as.vector(imod2$coef$fixed[2]),
  betaA=1/as.vector(imod1$coef[3]),tauY=1/summary(imod2)$sigma^2, 
  tauA=1/as.vector(imod1$coef[3])^2/(as.matrix(imod2$modelStruct$reStruct$FamID)[1,1]*imod2$sigma^2))),fileName='inits.txt',digits=6)



params=c("sigmaY","sigmaA","beta0","betaA","betaZ","beta","gamma0","gamma")

# run bugs and place output in 'out'
out=bugsRun(dir=getwd(),modelFile='model.txt',dataFile='data.txt',initsFile='inits.txt',genInits=TRUE,seed=1,paramNames=params,nBurnin=1000,nSample=1000,thin=10,DIC=TRUE,saveBurnin=FALSE)
# note that sampling 10000=1000*10 post-burnin takes a little while

if(FALSE){ # alternative to the bugsRun call above: you can also just print out the script file as follows (or create the script file outside of R) and then manually run the commands in script.txt from the Linux command line
  bugsRun(dir=getwd(),modelFile='model.txt',dataFile='data.txt',initsFile='inits.txt',genInits=TRUE,seed=1,paramNames=params,nBurnin=1000,nSample=1000,thin=10,DIC=TRUE,saveBurnin=FALSE,scriptOnly=TRUE,scriptFile='script.txt',outputFileStem='output')
  # manually run the script.txt commands in BUGS
  out=bugsOutput('output')
}




