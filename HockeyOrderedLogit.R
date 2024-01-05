#logit codes for predicting NHl/AHL entry from WHL
library(MASS)
library(VGAM)
library(stargazer)
library(ordinal)

NHLAHLWHLFrame <-read.delim("NHLAHLWHLLogit.csv", sep = ',',header=T)
ValueSim <-read.delim("valuesim2.csv", sep = ',',header=T)

LimOrdFrame <- NHLAHLWHLFrame[which(NHLAHLWHLFrame$Points.Per.Game>0 & NHLAHLWHLFrame$Games.Played>17 & NHLAHLWHLFrame$Fights.Per.Game>0),]

LogWHLPPG <- log(LimOrdFrame$Points.Per.Game)
SqrtWHLPPG <- sqrt(LimOrdFrame$Points.Per.Game)
LogWHLFPG <- log(LimOrdFrame$Fights.Per.Game)
WHLPPG <- SqrtWHLPPG^2
LogWHLFPGSq<- LogWHLFPG^2
plot(LogWHLFPGSqrt2,LogWHLFPGSqrt)


PPG.FPG <-LogWHLFPG*SqrtWHLPPG
plot(density(LogWHLFPG))
plot(density(sqrt(LimOrdFrame$Points.Per.Game)))


#models
LogitNHLPtsOnly <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG,family= data=LimOrdFrame)
LogitNHLAHLPtsOnly <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG, data=LimOrdFrame)
LogitNHL <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG, data=LimOrdFrame)
LogitNHLAHL <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG + LogWHLFPG, data=LimOrdFrame)
LogitNHLInt <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, data=LimOrdFrame)
LogitNHLAHLInt <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, data=LimOrdFrame)
LogitNHLIntSq <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt, data=LimOrdFrame)
LogitNHLAHLIntSq <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt, data=LimOrdFrame)
logLik(LogitNHLIntSq)


stargazer(LogitNHLPtsOnly,LogitNHL,LogitNHLInt,LogitNHLIntSq,LogitNHLAHLPtsOnly,LogitNHLAHL,LogitNHLAHLInt,LogitNHLAHLIntSq, type = "text", digits = 3,star.cutoffs = c(0.05, 0.01, 0.001))


LogNHLPtsOnly <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG,family = 'binomial', data=LimOrdFrame)
LogNHLAHLPtsOnly <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG,family = 'binomial', data=LimOrdFrame)
LogNHL <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG, family = 'binomial',data=LimOrdFrame)
LogNHLAHL <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG + LogWHLFPG, family = 'binomial',data=LimOrdFrame)
LogNHLInt <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, family = 'binomial',data=LimOrdFrame)
LogNHLAHLInt <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, family = 'binomial',data=LimOrdFrame)
LogNHLIntSq <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt,family = 'binomial', data=LimOrdFrame)
LogNHLAHLIntSq <-glm(Played.in.NHL.or.AHL ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt,family = 'binomial', data=LimOrdFrame)

#Table 2
stargazer(LogNHLPtsOnly,LogNHL,LogNHLInt,LogNHLIntSq,LogNHLAHLPtsOnly,LogNHLAHL,LogNHLAHLInt,LogNHLAHLIntSq, type = "text", digits = 3,star.cutoffs = c(0.05, 0.01, 0.001))

#test of proportional odds for varaibles
PropOddsTest_Pts_AHLNHL <- glm(Played.in.NHL.or.AHL ~ SqrtWHLPPG,family = 'binomial',data=LimOrdFrame)
PropOddsTest_Pts_NHL <- glm(Played.in.NHL.2015 ~ SqrtWHLPPG,family = 'binomial', data=LimOrdFrame, subset=(Played.in.NHL.or.AHL==1))
PropOddsTest_Fights_AHLNHL <- glm(Played.in.NHL.or.AHL ~ LogWHLFPG,family = 'binomial',data=LimOrdFrame)
PropOddsTest_Fights_NHL <- glm(Played.in.NHL.2015 ~ LogWHLFPG,family = 'binomial',data=LimOrdFrame, subset=(Played.in.NHL.or.AHL==1))

PropOddsPtsMatrix <- matrix((0:100)*0.015,101,1)
PropOddsFightsMatrix <- matrix(-6+(0:100)*0.06,101,1)
colnames(PropOddsFightsMatrix)<-c("LogWHLFPG")
colnames(PropOddsPtsMatrix)<-c("SqrtWHLPPG")
PropOddsPtsMatrix <-data.frame(PropOddsPtsMatrix)
PropOddsFightsMatrix <-data.frame(PropOddsFightsMatrix)

PtsPredNHL <-predict(PropOddsTest_Pts_NHL,newdata = PropOddsPtsMatrix,type ="response")
PtsPredAHLNHL <-predict(PropOddsTest_Pts_AHLNHL,newdata = PropOddsPtsMatrix,type="response")
FtsPredNHL <-predict(PropOddsTest_Fights_NHL,newdata = PropOddsFightsMatrix,type = "response")
FtsPredAHLNHL <-predict(PropOddsTest_Fights_AHLNHL,newdata = PropOddsFightsMatrix,type="response")

plot(PropOddsPtsMatrix[,1],PtsPredAHLNHL)
points(PropOddsPtsMatrix[,1],PtsPredNHL)

plot(PropOddsFightsMatrix[,1],FtsPredAHLNHL,ylim = c(0,1))
points(PropOddsFightsMatrix[,1],FtsPredNHL)

#orderedmodels
OL1 <-polr(as.factor(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG, data = LimOrdFrame)
OL2 <-polr(as.factor(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG, data = LimOrdFrame)
OL3 <-polr(as.factor(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, data = LimOrdFrame)
OL4 <-polr(as.factor(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt, data = LimOrdFrame)

#vglms
vglm1 <-vglm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG, data = LimOrdFrame, family = cumulative(parallel = F))
vglm2 <-vglm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG, data = LimOrdFrame, family = cumulative(parallel = F))
vglm2a <-vglm(ordered(OrdinalAHLNHL) ~ SqrtWHLPPG + LogWHLFPG, data = LimOrdFrame, family = cumulative(parallel = F))
vglm3 <-vglm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, data = LimOrdFrame, family = cumulative(parallel = F))
vglm4 <-vglm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt, data = LimOrdFrame, family = cumulative(parallel = F))
stargazer(vglm1,vglm2,vglm3, type = "text", digits = 3,star.cutoffs = c(0.05, 0.01, 0.001))
vglm(nausea ~ cisplatin, weights=freq, data=nausea.short,
     family=cumulative(parallel=T, reverse=T))

# Unconstrained partial proportional odds ordinal logistic regression
vglm(nausea ~ cisplatin, weights=freq, data=nausea.short,
     family=cumulative(parallel=F, reverse=T))

#ordinal
ord1 <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG, data = LimOrdFrame)
ord2 <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG, data = LimOrdFrame)
ord3 <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, data = LimOrdFrame)
ord4 <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt , data = LimOrdFrame)

par_ord1 <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG, nominal = ~Games.Played + SqrtWHLPPG,data = LimOrdFrame)
par_ord2 <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG, nominal = ~Games.Played + SqrtWHLPPG + LogWHLFPG,data = LimOrdFrame)
par_ord3 <- clm(as.ordered(OrdinalAHLNHL) ~ 1,nominal = ~Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG ,link = "logit",data = LimOrdFrame)

par_ord3a2 <- clm(as.ordered(OrdinalAHLNHL) ~ SqrtWHLPPG + LogWHLFPG + PPG.FPG,nominal =~Games.Played  ,data = LimOrdFrame)
par_ord3b <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + LogWHLFPG + PPG.FPG,nominal =~SqrtWHLPPG ,data = LimOrdFrame)
par_ord3c <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG,nominal =~LogWHLFPG ,data = LimOrdFrame)
par_ord3d <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG,nominal =~PPG.FPG ,data = LimOrdFrame)

par_ord3ab <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG,nominal =~Games.Played + SqrtWHLPPG,data = LimOrdFrame)
par_ord3ac <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG,nominal =~Games.Played + LogWHLFPG ,data = LimOrdFrame)
par_ord3ad <- clm(as.ordered(OrdinalAHLNHL) ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG,nominal =~Games.Played + PPG.FPG ,data = LimOrdFrame)

anova(par_ord3,ord3)
anova(par_ord3b,ord3)
anova(par_ord3c,ord3)
anova(par_ord3d,ord3)
anova(par_ord3ab,par_ord3a,ord3)
anova(par_ord3ac,par_ord3a,ord3)
anova(par_ord3ad,par_ord3a,ord3)
anova(ord3,par_ord3a)

stargazer(ord3,par_ord3a,par_ord3a2, type = "text", digits = 3,star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(ord1,ord2,ord3, type = "text", digits = 5,star.cutoffs = c(0.05, 0.01, 0.001))
summary(par_ord3a)

summary(par_ord3a2)
anova(ord1,par_ord1)
anova(ord2,par_ord2)
anova(ord3,par_ord3)

anova(ord1,ord2,ord3)
anova(par_ord1,par_ord2,par_ord3)


AHLtoNHL1 <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1))
AHLtoNHL2 <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1))
AHLtoNHL3 <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1))
AHLtoNHL4 <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1))

AHLtoNHL1L <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1),family = 'binomial')
AHLtoNHL2L <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1),family='binomial')
AHLtoNHL3L <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1),family='binomial')
AHLtoNHL4L <-glm(Played.in.NHL.2015 ~ Games.Played + SqrtWHLPPG + LogWHLFPG + PPG.FPG + WHLPPG + LogWHLFPGSqrt, data=LimOrdFrame,subset=(Played.in.NHL.or.AHL==1),family='binomial')

#Table 3
stargazer(OL1,OL2,OL3,OL4,AHLtoNHL1L,AHLtoNHL2L,AHLtoNHL3L,AHLtoNHL4L, type = "text", digits = 6,star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(AHLtoNHL1L,AHLtoNHL2L,AHLtoNHL3L,AHLtoNHL4L, type = "text", digits = 6,star.cutoffs = c(0.05, 0.01, 0.001))

summary(OL4)

summary(LogitAHLtoNHL)

summary(LogitNHL)
summary(LogitNHLAHL)
Ordinal <- polr(as.factor(OrdinalAHLNHL) ~ LogWHLPPG + LogWHLFPG + NHLAHLWHLFrame$Games.Played, data = NHLAHLWHLFrame,subset = (Games.Played >0 & Fights.Per.Game > 0 & Points.Per.Game >0))
summary(Ordinal1)

#predictions
predictedLevel<-predict(OL4,LimOrdFrame)
table(LimOrdFrame$OrdinalAHLNHL,predictedLevel)
table(LimOrdFrame$OrdinalAHLNHL,predictedlevel2c)
table(LimOrdFrame$OrdinalAHLNHL,predictedlevel2d)
table(LimOrdFrame$OrdinalAHLNHL,predictedlevel2e)

max(LogWHLFPG)


#predictions for simulated data

ValueSim2 = cbind.data.frame(0,ValueSim)
names(ValueSim2)[1]<-"OrdinalAHLNHL"

simord <- predict(ord3,newdata = ValueSim, type = "cum.prob")


sim1<-predict(LogNHLAHLIntSq,newdata=ValueSim,type="response")
sim2<-predict(AHLtoNHL4L,newdata=ValueSim,type="response")
plot(sim1[1:8],sim2[1:8],xlim = c(0,1),ylim=c(0,1))
lines(sim1[9:16],sim2[9:16])


sim3<-predict(OL4,newdata=ValueSim)
#print(sim1)
simmatrix_ord<-matrix(0:0,64,1)
for(x in (1:64))
{
  simmatrix_ord[x,1] <-ValueSim$Games.Played[x] * 0.00290 + ValueSim$SqrtWHLPPG[x] * 3.561 + ValueSim$LogWHLFPG[x] * 1.212 + ValueSim$PPG.FPG[x] * -.976

}
simmatrix_ord2<-matrix(0:0,64,2)
for(x in (1:64))
{
  simmatrix_ord2[x,1] = exp(simmatrix_ord[x] - 1.746)/(1 + exp(simmatrix_ord[x] - 1.746))
  simmatrix_ord2[x,2] = exp(simmatrix_ord[x] - 3.151)/(1 + exp(simmatrix_ord[x] - 3.151))
}

simmatrix_ord_final <- cbind(simmatrix_ord,simmatrix_ord2)
print(si)

simmatrix<-matrix(0:0,64,3)
for(x in (1:64))
  {
  simmatrix[x,1]<-sim1[x]
  simmatrix[x,2]<-sim2[x]
  simmatrix[x,3]<-sim3[x]
}

plot(density(log(predictedLevel2a)))
predictedLevel2a<-predict(LogNHLAHLIntSq,LimOrdFrame,type="response")
predictedLevel2b<-predict(AHLtoNHL4L,LimOrdFrame,type="response")
table(predictedLevel2a,predictedLevel2b)
plot(density(predictedLevel2a[predictedLevel2a>0.5]))

predictedlevel2c<-matrix(0:0,2158,1)
for (x in (1:2158)){
  WHLProb<-(1-predictedLevel2a[x])
  NHLProb<-(predictedLevel2a[x]*predictedLevel2b[x])
  AHLProb<-(predictedLevel2a[x]*(1-predictedLevel2b[x]))
  #print(WHLProb)
  #print(NHLProb)
  #print(AHLProb)
  if(AHLProb > NHLProb){
    if(AHLProb > WHLProb){
      predictedlevel2c[x,1]<-1
      }
    }
  if(NHLProb > WHLProb){
    if(NHLProb > AHLProb){
    predictedlevel2c[x,1]<-2
    }
  }
}

probs2d<-matrix(0:0,2158,4)
predictedlevel2d<-matrix(0:0,2158,1)
for (x in (1:2158)){
  WHLProb<-(1-predictedLevel2a[x])
  NHLProb<-(predictedLevel2a[x]*predictedLevel2b[x])
  AHLProb<-(predictedLevel2a[x]*(1-predictedLevel2b[x]))
  probs2d[x,1]<-WHLProb
  probs2d[x,2]<-AHLProb
  probs2d[x,3]<-NHLProb
  if(NHLProb > 0.5){
      predictedlevel2d[x,1]<-2
      probs2d[x,4]<-2
  }
  else if(NHLProb+AHLProb > 0.5){
      predictedlevel2d[x,1]<-1
      probs2d[x,4]<-1
    }
}

table(predictedlevel2d)

predictedlevel2e<-matrix(0:0,2158,1)
for (x in (1:2158)){
  if(predictedLevel2a[x] > 0.5){
    predictedlevel2e[x,1]<-1
    if(predictedLevel2b[x] > 0.5){
      predictedlevel2e[x,1]<-2
    }
  }
}
table(predictedlevel2e)

#graphs

par(mfrow=c(1,1))
plot(density(LimOrdFrame$Games.Played[LimOrdFrame$OrdinalAHLNHL==2]),col = 'black',main = 'Games Played in WHL by Highest Level Achieved')
lines(density(LimOrdFrame$Games.Played[LimOrdFrame$OrdinalAHLNHL==1]),col = 'grey 45')
lines(density(LimOrdFrame$Games.Played[LimOrdFrame$OrdinalAHLNHL==0]),col = 'grey')
legend("topright",c('NHL','AHL','WHL'),lty=c(1,1,1),col=c('black','grey 45','grey'))

plot(density(log(LimOrdFrame$Fights.Per.Game)[LimOrdFrame$OrdinalAHLNHL==2]),col = 'black',main = 'Log of Fights Per Game in WHL by Highest Level Achieved')
lines(density(log(LimOrdFrame$Fights.Per.Game)[LimOrdFrame$OrdinalAHLNHL==1]),col = 'grey 45')
lines(density(log(LimOrdFrame$Fights.Per.Game)[LimOrdFrame$OrdinalAHLNHL==0]),col = 'grey')
legend("topright",c('NHL','AHL','WHL'),lty=c(1,1,1),col=c('black','grey 45','grey'))

plot(density(sqrt(LimOrdFrame$Points.Per.Game)[LimOrdFrame$OrdinalAHLNHL==2]),col = 'black',main = 'Sqrt of Points Per Game in WHL by Highest Level Achieved')
lines(density(sqrt(LimOrdFrame$Points.Per.Game)[LimOrdFrame$OrdinalAHLNHL==1]),col = 'grey 45')
lines(density(sqrt(LimOrdFrame$Points.Per.Game)[LimOrdFrame$OrdinalAHLNHL==0]),col = 'grey')
legend("topright",c('NHL','AHL','WHL'),lty=c(1,1,1),col=c('black','grey 45','grey'))

plot(sqrt(LimOrdFrame$Points.Per.Game),log(LimOrdFrame$Fights.Per.Game) ,col=ifelse(LimOrdFrame$OrdinalAHLNHL == 0,"grey",ifelse(LimOrdFrame$OrdinalAHLNHL == 1,"grey 45","black")),pch=ifelse(LimOrdFrame$OrdinalAHLNHL == 0,5,19),main = 'Level of Professional Success',xlab='Sqrt Points Per Game in WHL',ylab='Log Fights Per Game in WHL')
legend("topright",c('NHL','AHL','WHL'),pch=c(19,19,5),col=c('black','grey 45','grey'))



par(mfrow=c(1,1))
plot(density(LimOrdFrame$Games.Played[LimOrdFrame$OrdinalAHLNHL==2]),col = 'black',main="",xlab = "Games Played")
lines(density(LimOrdFrame$Games.Played[LimOrdFrame$OrdinalAHLNHL==1]),col = 'grey 45')
lines(density(LimOrdFrame$Games.Played[LimOrdFrame$OrdinalAHLNHL==0]),col = 'grey')
legend("topright",c('NHL','AHL','WHL'),lty=c(1,1,1),col=c('black','grey 45','grey'))

plot(density(log(LimOrdFrame$Fights.Per.Game)[LimOrdFrame$OrdinalAHLNHL==2]),col = 'black',main = '',xlab = 'Log(Fights Per Game)')
lines(density(log(LimOrdFrame$Fights.Per.Game)[LimOrdFrame$OrdinalAHLNHL==1]),col = 'grey 45')
lines(density(log(LimOrdFrame$Fights.Per.Game)[LimOrdFrame$OrdinalAHLNHL==0]),col = 'grey')
legend("topright",c('NHL','AHL','WHL'),lty=c(1,1,1),col=c('black','grey 45','grey'))

plot(density(sqrt(LimOrdFrame$Points.Per.Game)[LimOrdFrame$OrdinalAHLNHL==2]),col = 'black',main = '',xlab = 'Square Root of Points Per Game')
lines(density(sqrt(LimOrdFrame$Points.Per.Game)[LimOrdFrame$OrdinalAHLNHL==1]),col = 'grey 45')
lines(density(sqrt(LimOrdFrame$Points.Per.Game)[LimOrdFrame$OrdinalAHLNHL==0]),col = 'grey')
legend("topright",c('NHL','AHL','WHL'),lty=c(1,1,1),col=c('black','grey 45','grey'))

plot(sqrt(LimOrdFrame$Points.Per.Game),log(LimOrdFrame$Fights.Per.Game) ,col=ifelse(LimOrdFrame$OrdinalAHLNHL == 0,"grey",ifelse(LimOrdFrame$OrdinalAHLNHL == 1,"grey 45","black")),pch=ifelse(LimOrdFrame$OrdinalAHLNHL == 0,5,19),main = '',xlab='Sqrt Points Per Game in WHL',ylab='Log Fights Per Game in WHL')
legend("topright",c('NHL','AHL','WHL'),pch=c(19,19,5),col=c('black','grey 45','grey'))
