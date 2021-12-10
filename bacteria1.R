bacteria=read.csv("c:/data/bacteria.csv")
bacteria
plot(bacteria$t,bacteria$Count,xlab="Time",ylab="Count",pch=16)
plot(bacteria$t,log(bacteria$Count),xlab="Time",ylab="ln(Count)",pch=16)

fit1=lm(Count~t,bacteria)
summary(fit1)
plot(fit1, which=1:2)
plot(bacteria$t,fit1$resid,xlab="t",ylab="Residual",pch=16)
fit2=lm(log(Count)~t,bacteria)
summary(fit2)
plot(bacteria$t,fit2$resid,xlab="t",ylab="Residual",pch=16)
anova(fit2)
confint(fit2,"t",level=0.95)
predict(fit2,newdata=data.frame(t=3.0),interval="predict")
predict(fit2,newdata=data.frame(t=3.0),interval="confidence")


