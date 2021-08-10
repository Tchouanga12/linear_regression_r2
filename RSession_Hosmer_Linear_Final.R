
#Importation des donnees
TableHosmer<-read.table('C:/Users/LUCION/Documents/Nguefack_Lectures/ISJ/MyData/DataHosmer.txt', header = TRUE)


attach(TableHosmer)
head(TableHosmer)
#ID	AGE	LWT	RACE	SMOKE	PTL	HT	UI	FVT	BWT	BWTLow	FVT1	PTL1	 LWTkg 

hist(TableHosmer$BWT,proba=TRUE,xlab="Poids de naissance (BWT)", ylab="Densite",main="Repartition des poids de naissance", ylim=c(0,0.0006))
BWTordonne<-TableHosmer$BWT[order(TableHosmer$BWT)]
lines(BWTordonne, dnorm(BWTordonne, mean=mean(BWTordonne), sd=sd(BWTordonne)), col="red")
savePlot("C:/Users/LUCION/Documents/Nguefack_Lectures/ISJ/Models Multivaries/HistogrammeBWT.pdf",type="pdf")

%Regression simple
plot(TableHosmer$LWT,TableHosmer$BWT, xlab="Poids de la mere (livre)", ylab="Poids de naissance (gramme)")

RegLin1<-lm(BWT ~ LWT,data=TableHosmer)

summary(RegLin1)

plot(RegLin1$res,ylab="Residus",main="Graphique des residus")

plot(RegLin1$fit,RegLin1$res,xlab="Estimation",ylab="Residus")
abline(h=0)

plot(RegLin1$fit,abs(RegLin1$res),xlab="Estimation",ylab="|Residus|")

summary(lm(abs(RegLin1$res)~ RegLin1$fit))


####Evaluer la normalite####
qqnorm(RegLin1$res,ylab="Raw Residus Brutes")
qqline(RegLin1$res)



###########Model Selection#############

###Model Selection (First put all variables)
RegLinFull <- lm(BWT~ AGE + LWT + factor(RACE) + SMOKE + HT +	UI + FVT1 + PTL1, data=TableHosmer)

step(RegLinFull)

sRegLinFull<-step(RegLinFull)
sRegLinFull$anova

sRegLinFull

library(MASS)
stepAIC(RegLinFull)


sbackRegLinFull<-step(RegLinFull, direction = "backward")
sbackRegLinFull$anova

sforwardRegLinFull<-step(RegLinFull, direction = "forward")
sforwardRegLinFull$anova

sbothRegLinFull<-step(RegLinFull, direction = "both")
sbothRegLinFull$anova


####Backward####

RegLinFull <- lm(BWT~ AGE + LWT + factor(RACE) + SMOKE + HT +	UI + FVT1 + PTL1, data=TableHosmer)
summary(RegLinFull)

RegLinFull<- update(RegLinFull, . ~ . - FVT1)
summary(RegLinFull)

RegLinFull<- update(RegLinFull, . ~ . - AGE)
summary(RegLinFull)

RegLinFull<- update(RegLinFull, . ~ . - PTL1)
summary(RegLinFull)





plot(TableHosmer$LWT,TableHosmer$BWT,xlab="Poids de la mere (LWT, en livres)", ylab="Poids de naissance (BWT, en grammes)")
abline(RegLin1,lwd=2,lty=2,col="red")
legend("topright",c(expression(BWT==hat(beta)[0] + hat(beta)[1]*LWT), "observations"),lty=c(2,0),pch=c(NA,1),col=c("red","black"))

qqnorm(rstudent(RegLin1),ylab="Studentized residuals")
abline(0,1)


hist(RegLin1$res,10)
boxplot(RegLin1$res,main="Boxplot of savings residuals")

RegLin2<-lm(BWT ~ SMOKE,data=TableHosmer)
summary(RegLin2)


predict(RegLin2,newdata=data.frame(SMOKE<-c(0,1)), interval="confidence")

confint(RegLin2)

Regression multiplr
#################################
RegLin3<-lm(BWT ~ factor(RACE),data=TableHosmer)
summary(RegLin3)

RegLin4<-lm(BWT ~ LWT + UI + HT + factor(RACE),data=TableHosmer)
summary(RegLin4)

RegLin4<-lm(BWT ~ LWT + UI + HT + factor(RACE),data=TableHosmer)
RegLin4bis<-lm(BWT ~ LWT + UI + HT,data=TableHosmer)
anova(RegLin4bis,RegLin4)

#########Exemple de test d'interaction ou de modication d'effet.
RegLin5<-lm(BWT ~ AGE+SMOKE+SMOKE:AGE,data=TableHosmer)
summary(RegLin5)

confint(RegLin5)


SMOKE1<-1-TableHosmer$SMOKE
confint(lm(BWT ~ AGE+SMOKE1+SMOKE1:AGE,data=TableHosmer))

Exemples d'analyse des residus
RegLin6<-lm(BWT ~ SMOKE+AGE+LWT+factor(RACE)+UI+HT+AGE:SMOKE, data=TableHosmer)
summary(RegLin6)


ResidusOrd<-RegLin6$residuals[order(RegLin6$residuals)]
hist(ResidusOrd,proba=TRUE,main="Histogramme des residus", ylab="Densite",xlab="Residus")
lines(ResidusOrd, dnorm(ResidusOrd, sd=sd(ResidusOrd)),col="red")

qqnorm(RegLin6$residuals)
plot(RegLin6$fitted.values,RegLin6$residuals, ylab=expression(e[i]==Y[i]-hat(Y)[i]), xlab=expression(hat(Y)[i]),cex.lab=1.2)

abline(h=0,lty=2,lwd=2,col="red")


#######Nuages de points des residus en fonction de chaque variable explicative:
par(mfrow=c(2,3))
res<-RegLin6$residuals
plot(res ~ TableHosmer$SMOKE,xlab="SMOKE",ylab="residus")
abline(h=0,lty=2,lwd=2,col="red")

plot(res ~ TableHosmer$AGE,xlab="AGE",ylab="residus")
abline(h=0,lty=2,lwd=2,col="red")

plot(res ~ TableHosmer$LWT,xlab="LWT",ylab="residus")
abline(h=0,lty=2,lwd=2,col="red")

plot(res ~ TableHosmer$RACE,xlab="RACE",ylab="residus")
abline(h=0,lty=2,lwd=2,col="red")

plot(res~TableHosmer$UI,xlab="UI",ylab="residus")
abline(h=0,lty=2,lwd=2,col="red")

plot(res  TableHosmer$HT,xlab="HT",ylab="residus")
abline(h=0,lty=2,lwd=2,col="red")




