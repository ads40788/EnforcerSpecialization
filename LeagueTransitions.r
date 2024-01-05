#Transitions Between Leagues
WHLtoNHL <-read.csv("TransWHLNHL.csv")
WHLtoAHL <-read.csv("TransWHLAHL.csv")
AHLtoNHL <-read.csv("TransAHLNHL.csv")
ALLThree <-read.csv("TransALL3.csv")
ScatterInfo <-read.csv("WHLAHLNHLScatter.csv")


#All 3 - No Subsets
AHLScat <- ScatterInfo[which(ScatterInfo$AHL.GP>20 & ScatterInfo$AHLPPG>0 & ScatterInfo$AHL.FPG > 0),]
LogAHLFPG <- log(AHLScat$AHL.FPG)
SqrtAHLPPG <- sqrt(AHLScat$AHLPPG)
WHLScat <- ScatterInfo[which(ScatterInfo$WHL.GP>18 & ScatterInfo$WHLPPG >0 & ScatterInfo$WHLFPG>0),]
LogWHLFPG <- log(WHLScat$WHLFPG)
SqrtWHLPPG <- sqrt(WHLScat$WHLPPG)
NHLScat <- ScatterInfo[which(ScatterInfo$NHL.GP>20 & ScatterInfo$NHL.PPG>0 & ScatterInfo$NHLFPG>0 ),]
LogNHLFPG <- log(NHLScat$NHLFPG)
SqrtNHLPPG <- sqrt(NHLScat$NHL.PPG)

ZLogWHLFPG <- (LogWHLFPG-mean(LogWHLFPG))/sqrt(var(LogWHLFPG))
ZLogAHLFPG <- (LogAHLFPG-mean(LogAHLFPG))/sqrt(var(LogAHLFPG))
ZLogNHLFPG <- (LogNHLFPG-mean(LogNHLFPG))/sqrt(var(LogNHLFPG))
ZSqrtWHLPPG <- (SqrtWHLPPG-mean(SqrtWHLPPG))/sqrt(var(SqrtWHLPPG))
ZSqrtAHLPPG <- (SqrtAHLPPG-mean(SqrtAHLPPG))/sqrt(var(SqrtAHLPPG))
ZSqrtNHLPPG <- (SqrtNHLPPG-mean(SqrtNHLPPG))/sqrt(var(SqrtNHLPPG))



mean(LogWHLFPG)
plot((LogWHLFPG-mean(LogWHLFPG))/sqrt(var(LogWHLFPG)),(SqrtWHLPPG-mean(SqrtWHLPPG))/sqrt(var(SqrtWHLPPG)),col='grey',pch = 1, xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game' )
points((LogAHLFPG-mean(LogAHLFPG))/sqrt(var(LogAHLFPG)),(SqrtAHLPPG-mean(SqrtAHLPPG))/sqrt(var(SqrtAHLPPG)),col='grey 45',pch = 18)
points((LogNHLFPG-mean(LogNHLFPG))/sqrt(var(LogNHLFPG)),(SqrtNHLPPG-mean(SqrtNHLPPG))/sqrt(var(SqrtNHLPPG)),col='black',pch =0)
abline(lm(ZLogWHLFPG~ZSqrtWHLPPG),col = 'grey')
abline(lm(ZLogAHLFPG~ZSqrtAHLPPG),col = 'grey 45')
abline(lm(ZLogNHLFPG~ZSqrtNHLPPG),col = 'black')
legend("topright",c('NHL','AHL','WHL'),pch=c(0,18,1),col=c('black','grey 45','grey'))

#CleanMatricmies
WHLtoNHLTrim<-WHLtoNHL[which(WHLtoNHL$WHL.GP>18 & WHLtoNHL$WHL.Points>0 & WHLtoNHL$WHL.Fights>0 & WHLtoNHL$NHL.GP>20 & WHLtoNHL$NHL.Points>0 & WHLtoNHL$NHL.Fights>0),]
WHLtoAHLTrim<-WHLtoAHL[which(WHLtoAHL$WHL.GP>18 & WHLtoAHL$WHL.Points>0 & WHLtoAHL$WHL.Fights>0 & WHLtoAHL$AHL.GP>20 & WHLtoAHL$AHL.Points>0 & WHLtoAHL$AHL.Fights>0),]
AHLtoNHLTrim<-AHLtoNHL[which(AHLtoNHL$AHL.GP>20 & AHLtoNHL$AHL.Points>0 & AHLtoNHL$AHL.Fights>0 & AHLtoNHL$NHL.GP>20 & AHLtoNHL$NHL.Points>0 & AHLtoNHL$NHL.Fights>0),]
ALLThreeTrim<- ALLThree[which(ALLThree$WHL.GP>18 & ALLThree$WHL.Points>0 & ALLThree$WHL.Fights>0 & ALLThree$AHL.GP>20 & ALLThree$AHL.Points>0 & ALLThree$AHL.Fights>0 & ALLThree$NHL.GP>20 & ALLThree$NHL.Points>0 & ALLThree$NHL.Fights>0),]

#WHL to NHL Variables
WN.WHLFPG <- WHLtoNHLTrim$WHL.Fights/WHLtoNHLTrim$WHL.GP
WN.WHLPPG <- WHLtoNHLTrim$WHL.Points/WHLtoNHLTrim$WHL.GP
WN.LogWHLFPG <- log(WN.WHLFPG)
WN.SqrtWHLPPG <- sqrt(WN.WHLPPG)
WN.NHLFPG <- WHLtoNHLTrim$NHL.Fights/WHLtoNHLTrim$NHL.GP
WN.NHLPPG <- WHLtoNHLTrim$NHL.Points/WHLtoNHLTrim$NHL.GP
WN.LogNHLFPG <- log(WN.NHLFPG)
WN.SqrtNHLPPG <- sqrt(WN.NHLPPG)

#plot WHL vs NHL
par(mfrow=c(1,1))
plot((WN.LogWHLFPG-mean(WN.LogWHLFPG))/sqrt(var(WN.LogWHLFPG)),(WN.SqrtWHLPPG-mean(WN.SqrtWHLPPG))/sqrt(var(WN.SqrtWHLPPG)),col=1)
points((WN.LogNHLFPG-mean(WN.LogNHLFPG))/sqrt(var(WN.LogNHLFPG)),(WN.SqrtNHLPPG-mean(WN.SqrtNHLPPG))/sqrt(var(WN.SqrtNHLPPG)),col=2)

WN.ZLogWHLFPG <- (WN.LogWHLFPG-mean(WN.LogWHLFPG))/sqrt(var(WN.LogWHLFPG))
WN.ZSqrtWHLPPG <- (WN.SqrtWHLPPG-mean(WN.SqrtWHLPPG))/sqrt(var(WN.SqrtWHLPPG))
WN.ZLogNHLFPG <- (WN.LogNHLFPG-mean(WN.LogNHLFPG))/sqrt(var(WN.LogNHLFPG))
WN.ZSqrtNHLPPG <- (WN.SqrtNHLPPG-mean(WN.SqrtNHLPPG))/sqrt(var(WN.SqrtNHLPPG))

plot(WN.ZLogWHLFPG,WN.ZSqrtWHLPPG,col='red',main = 'Sqrt PPG vs Log FPG - WHL vs NHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(WN.ZLogNHLFPG,WN.ZSqrtNHLPPG,col='blue')
abline(lm(WN.ZLogWHLFPG~WN.ZSqrtWHLPPG),col = 'red')
abline(lm(WN.ZLogNHLFPG~WN.ZSqrtNHLPPG),col = 'blue')


#models WHL vs NHL
WN.NHLPoints <- lm(formula = WN.SqrtNHLPPG ~ WN.SqrtWHLPPG + WN.LogWHLFPG)
WN.NHLPoints1 <- lm(formula = WN.SqrtNHLPPG ~ WN.SqrtWHLPPG)
WN.NHLFights <- lm(formula = WN.LogNHLFPG ~ WN.SqrtWHLPPG + WN.LogWHLFPG)
WN.NHLFights1 <- lm(formula = WN.LogNHLFPG ~ WN.LogWHLFPG)

#WHL to AHL Variables
WA.WHLFPG <- WHLtoAHLTrim$WHL.Fights/WHLtoAHLTrim$WHL.GP
WA.WHLPPG <- WHLtoAHLTrim$WHL.Points/WHLtoAHLTrim$WHL.GP
WA.LogWHLFPG <- log(WA.WHLFPG)
WA.SqrtWHLPPG <- sqrt(WA.WHLPPG)
WA.AHLFPG <- WHLtoAHLTrim$AHL.Fights/WHLtoAHLTrim$AHL.GP
WA.AHLPPG <- WHLtoAHLTrim$AHL.Points/WHLtoAHLTrim$AHL.GP
WA.LogAHLFPG <- log(WA.AHLFPG)
WA.SqrtAHLPPG <- sqrt(WA.AHLPPG)

#plot WHL vs AHL
par(mfrow=c(1,1))
WA.ZLogWHLFPG <- (WA.LogWHLFPG-mean(WA.LogWHLFPG))/sqrt(var(WA.LogWHLFPG))
WA.ZSqrtWHLPPG <- (WA.SqrtWHLPPG-mean(WA.SqrtWHLPPG))/sqrt(var(WA.SqrtWHLPPG))
WA.ZLogAHLFPG <- (WA.LogAHLFPG-mean(WA.LogAHLFPG))/sqrt(var(WA.LogAHLFPG))
WA.ZSqrtAHLPPG <- (WA.SqrtAHLPPG-mean(WA.SqrtAHLPPG))/sqrt(var(WA.SqrtAHLPPG))

plot(WA.ZLogWHLFPG,WA.ZSqrtWHLPPG,col='red',main = 'Sqrt PPG vs Log FPG - WHL vs AHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(WA.ZLogAHLFPG,WA.ZSqrtAHLPPG,col='purple')
abline(lm(WA.ZLogWHLFPG~WA.ZSqrtWHLPPG),col = 'red')
abline(lm(WA.ZLogAHLFPG~WA.ZSqrtAHLPPG),col = 'purple')

#models WHL vs NHL
WA.AHLPoints <- lm(formula = WA.SqrtAHLPPG ~ WA.SqrtWHLPPG + WA.LogWHLFPG)
WA.AHLPoints1 <- lm(formula = WA.SqrtAHLPPG ~ WA.SqrtWHLPPG)
WA.AHLFights <- lm(formula = WA.LogAHLFPG ~ WA.SqrtWHLPPG + WA.LogWHLFPG)
WA.AHLFights1 <- lm(formula = WA.LogAHLFPG ~ WA.LogWHLFPG)
summary(WA.AHLFights)

#AHL to NHL Variables
AN.AHLFPG <- AHLtoNHLTrim$AHL.Fights/AHLtoNHLTrim$AHL.GP
AN.AHLPPG <- AHLtoNHLTrim$AHL.Points/AHLtoNHLTrim$AHL.GP
AN.LogAHLFPG <- log(AN.AHLFPG)
AN.SqrtAHLPPG <- sqrt(AN.AHLPPG)
AN.NHLFPG <- AHLtoNHLTrim$NHL.Fights/AHLtoNHLTrim$NHL.GP
AN.NHLPPG <- AHLtoNHLTrim$NHL.Points/AHLtoNHLTrim$NHL.GP
AN.LogNHLFPG <- log(AN.NHLFPG)
AN.SqrtNHLPPG <- sqrt(AN.NHLPPG)

#plot AHL vs NHL
par(mfrow=c(1,1))
plot((AN.LogAHLFPG-mean(AN.LogAHLFPG))/sqrt(var(AN.LogAHLFPG)),(AN.SqrtAHLPPG-mean(AN.SqrtAHLPPG))/sqrt(var(AN.SqrtAHLPPG)),col=1)
points((AN.LogNHLFPG-mean(AN.LogNHLFPG))/sqrt(var(AN.LogNHLFPG)),(AN.SqrtNHLPPG-mean(AN.SqrtNHLPPG))/sqrt(var(AN.SqrtNHLPPG)),col=2)

AN.ZLogAHLFPG <- (AN.LogAHLFPG-mean(AN.LogAHLFPG))/sqrt(var(AN.LogAHLFPG))
AN.ZSqrtAHLPPG <- (AN.SqrtAHLPPG-mean(AN.SqrtAHLPPG))/sqrt(var(AN.SqrtAHLPPG))
AN.ZLogNHLFPG <- (AN.LogNHLFPG-mean(AN.LogNHLFPG))/sqrt(var(AN.LogNHLFPG))
AN.ZSqrtNHLPPG <- (AN.SqrtNHLPPG-mean(AN.SqrtNHLPPG))/sqrt(var(AN.SqrtNHLPPG))

plot(AN.ZLogAHLFPG,AN.ZSqrtAHLPPG,col='purple',main = 'Sqrt PPG vs Log FPG - AHL vs NHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(AN.ZLogNHLFPG,AN.ZSqrtNHLPPG,col='blue')
abline(lm(AN.ZLogAHLFPG~AN.ZSqrtAHLPPG),col = 'purple')
abline(lm(AN.ZLogNHLFPG~AN.ZSqrtNHLPPG),col = 'blue')




#models AHL vs NHL
AN.NHLPoints <- lm(formula = AN.SqrtNHLPPG ~ AN.SqrtAHLPPG + AN.LogAHLFPG)
AN.NHLPoints1 <- lm(formula = AN.SqrtNHLPPG ~ AN.SqrtAHLPPG)
AN.NHLFights <- lm(formula = AN.LogNHLFPG ~ AN.SqrtAHLPPG + AN.LogAHLFPG)
AN.NHLFights1 <- lm(formula = AN.LogNHLFPG ~ AN.LogAHLFPG)

#All3 Variables
A3.AHLFPG <- ALLThreeTrim$AHL.Fights/ALLThreeTrim$AHL.GP
A3.AHLPPG <- ALLThreeTrim$AHL.Points/ALLThreeTrim$AHL.GP
A3.WHLFPG <- ALLThreeTrim$WHL.Fights/ALLThreeTrim$WHL.GP
A3.LogAHLFPG <- log(A3.AHLFPG)
A3.SqrtAHLPPG <- sqrt(A3.AHLPPG)
A3.LogWHLFPG <- log(A3.WHLFPG)
A3.SqrtWHLPPG <- sqrt(A3.WHLPPG)
A3.NHLFPG <- ALLThreeTrim$NHL.Fights/ALLThreeTrim$NHL.GP
A3.NHLPPG <- ALLThreeTrim$NHL.Points/ALLThreeTrim$NHL.GP
A3.LogNHLFPG <- log(A3.NHLFPG)
A3.SqrtNHLPPG <- sqrt(A3.NHLPPG)
#plot AHL vs NHL vs WHL
par(mfrow=c(1,1))

plot((A3.LogAHLFPG-mean(A3.LogAHLFPG))/sqrt(var(A3.LogAHLFPG)),(A3.SqrtAHLPPG-mean(A3.SqrtAHLPPG))/sqrt(var(A3.SqrtAHLPPG)),col=1)
points((A3.LogWHLFPG-mean(A3.LogWHLFPG))/sqrt(var(A3.LogWHLFPG)),(A3.SqrtWHLPPG-mean(A3.SqrtWHLPPG))/sqrt(var(A3.SqrtWHLPPG)),col=2)
points((A3.LogNHLFPG-mean(A3.LogNHLFPG))/sqrt(var(A3.LogNHLFPG)),(A3.SqrtNHLPPG-mean(A3.SqrtNHLPPG))/sqrt(var(A3.SqrtNHLPPG)),col=3)

A3.ZLogWHLFPG <- (A3.LogWHLFPG-mean(A3.LogWHLFPG))/sqrt(var(A3.LogWHLFPG))
A3.ZSqrtWHLPPG <- (A3.SqrtWHLPPG-mean(A3.SqrtWHLPPG))/sqrt(var(A3.SqrtWHLPPG))
A3.ZLogAHLFPG <- (A3.LogAHLFPG-mean(A3.LogAHLFPG))/sqrt(var(A3.LogAHLFPG))
A3.ZSqrtAHLPPG <- (A3.SqrtAHLPPG-mean(A3.SqrtAHLPPG))/sqrt(var(A3.SqrtAHLPPG))
A3.ZLogNHLFPG <- (A3.LogNHLFPG-mean(A3.LogNHLFPG))/sqrt(var(A3.LogNHLFPG))
A3.ZSqrtNHLPPG <- (A3.SqrtNHLPPG-mean(A3.SqrtNHLPPG))/sqrt(var(A3.SqrtNHLPPG))

plot(A3.ZLogWHLFPG,A3.ZSqrtWHLPPG,col='red',main = 'Sqrt PPG vs Log FPG - WHL & AHL vs NHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(A3.ZLogAHLFPG,A3.ZSqrtAHLPPG,col='purple')
points(A3.ZLogNHLFPG,A3.ZSqrtNHLPPG,col='blue')
abline(lm(A3.ZLogWHLFPG~A3.ZSqrtWHLPPG),col = 'red')
abline(lm(A3.ZLogAHLFPG~A3.ZSqrtAHLPPG),col = 'purple')
abline(lm(A3.ZLogNHLFPG~A3.ZSqrtNHLPPG),col = 'blue')
legend("topright",c('NHL','AHL','WHL'),lty=c(1,1,1),col=c('black','grey 45','grey'))

#models AHL vs NHL
A3.NHLPoints <- lm(formula = A3.SqrtNHLPPG ~ A3.SqrtWHLPPG + A3.SqrtAHLPPG + A3.LogWHLFPG + A3.LogAHLFPG)
A3.NHLPoints1 <- lm(formula = A3.SqrtNHLPPG ~ A3.SqrtWHLPPG + A3.SqrtAHLPPG)
A3.NHLFights <- lm(formula = A3.LogNHLFPG ~ A3.SqrtWHLPPG + A3.SqrtAHLPPG + A3.LogWHLFPG + A3.LogAHLFPG)
A3.NHLFights1 <- lm(formula = A3.LogNHLFPG ~ A3.LogWHLFPG + A3.LogAHLFPG)

#4 plots

par(mfrow=c(2,2))

plot(WN.ZLogWHLFPG,WN.ZSqrtWHLPPG,col='red',main = 'Sqrt PPG vs Log FPG - WHL vs NHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(WN.ZLogNHLFPG,WN.ZSqrtNHLPPG,col='blue')
abline(lm(WN.ZLogWHLFPG~WN.ZSqrtWHLPPG),col = 'red')
abline(lm(WN.ZLogNHLFPG~WN.ZSqrtNHLPPG),col = 'blue')

plot(WA.ZLogWHLFPG,WA.ZSqrtWHLPPG,col='red',main = 'Sqrt PPG vs Log FPG - WHL vs AHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(WA.ZLogAHLFPG,WA.ZSqrtAHLPPG,col='purple')
abline(lm(WA.ZLogWHLFPG~WA.ZSqrtWHLPPG),col = 'red')
abline(lm(WA.ZLogAHLFPG~WA.ZSqrtAHLPPG),col = 'purple')

plot(AN.ZLogAHLFPG,AN.ZSqrtAHLPPG,col='purple',main = 'Sqrt PPG vs Log FPG - AHL vs NHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(AN.ZLogNHLFPG,AN.ZSqrtNHLPPG,col='blue')
abline(lm(AN.ZLogAHLFPG~AN.ZSqrtAHLPPG),col = 'purple')
abline(lm(AN.ZLogNHLFPG~AN.ZSqrtNHLPPG),col = 'blue')

plot(A3.ZLogWHLFPG,A3.ZSqrtWHLPPG,col='red',main = 'Sqrt PPG vs Log FPG - WHL & AHL vs NHL', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(A3.ZLogAHLFPG,A3.ZSqrtAHLPPG,col='purple')
points(A3.ZLogNHLFPG,A3.ZSqrtNHLPPG,col='blue')
abline(lm(A3.ZLogWHLFPG~A3.ZSqrtWHLPPG),col = 'red')
abline(lm(A3.ZLogAHLFPG~A3.ZSqrtAHLPPG),col = 'purple')
abline(lm(A3.ZLogNHLFPG~A3.ZSqrtNHLPPG),col = 'blue')

summary(A3.NHLFights)


summary(AN.NHLFights1)

stargazer(WN.NHLPoints,WA.AHLPoints,AN.NHLPoints,A3.NHLPoints,WN.NHLFights,WA.AHLFights,AN.NHLFights,A3.NHLFights, type = "text", digits = 4,star.cutoffs = c(0.05, 0.01, 0.001))


#4 plots

par(mfrow=c(2,2))

plot(WN.ZLogWHLFPG,WN.ZSqrtWHLPPG,col='grey',main = '', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(WN.ZLogNHLFPG,WN.ZSqrtNHLPPG,col='black')
abline(lm(WN.ZLogWHLFPG~WN.ZSqrtWHLPPG),col = 'grey')
abline(lm(WN.ZLogNHLFPG~WN.ZSqrtNHLPPG),col = 'black')
legend("topright",c('NHL','WHL'),pch=c(1,1),col=c("black","grey"))

plot(WA.ZLogWHLFPG,WA.ZSqrtWHLPPG,col='grey',main = '', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(WA.ZLogAHLFPG,WA.ZSqrtAHLPPG,col='grey 45')
abline(lm(WA.ZLogWHLFPG~WA.ZSqrtWHLPPG),col = 'grey')
abline(lm(WA.ZLogAHLFPG~WA.ZSqrtAHLPPG),col = 'grey 45')
legend("topright",c('AHL','WHL'),pch=c(1,1),col=c("grey 45","grey"))


plot(AN.ZLogAHLFPG,AN.ZSqrtAHLPPG,col='grey 45',main = '', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(AN.ZLogNHLFPG,AN.ZSqrtNHLPPG,col='black')
abline(lm(AN.ZLogAHLFPG~AN.ZSqrtAHLPPG),col = 'grey 45')
abline(lm(AN.ZLogNHLFPG~AN.ZSqrtNHLPPG),col = 'black')
legend("topright",c('NHL','AHL'),pch=c(1,1),col=c("black","grey 45"))


plot(A3.ZLogWHLFPG,A3.ZSqrtWHLPPG,col='grey',main = '', xlab = 'Standardized Log Fights Per Game',ylab = 'Standardized Sqrt Points Per Game')
points(A3.ZLogAHLFPG,A3.ZSqrtAHLPPG,col='grey 45')
points(A3.ZLogNHLFPG,A3.ZSqrtNHLPPG,col='black')
abline(lm(A3.ZLogWHLFPG~A3.ZSqrtWHLPPG),col = 'grey')
abline(lm(A3.ZLogAHLFPG~A3.ZSqrtAHLPPG),col = 'grey 45')
abline(lm(A3.ZLogNHLFPG~A3.ZSqrtNHLPPG),col = 'black')
legend("topright",c('NHL','AHL','WHL'),pch=c(1,1,1),col=c("black","grey 45","grey"))