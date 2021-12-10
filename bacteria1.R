R version 2.15.1 (2012-06-22) -- "Roasted Marshmallows"
Copyright (C) 2012 The R Foundation for Statistical Computing
ISBN 3-900051-07-0
Platform: x86_64-pc-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

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


