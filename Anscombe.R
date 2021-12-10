fit1(~t
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

anscombe=read.csv("c:/data/anscombe.csv")
anscombe

fit=lm(y1~x1,anscombe)
summary(fit)
plot(anscombe$x1,anscombe$y1,xlab="x1",ylab="y1",pch=16)
abline(lm(y1~x1,anscombe))

fit=lm(y2~x2,anscombe)
summary(fit)
plot(anscombe$x2,anscombe$y2,xlab="x2",ylab="y2",pch=16)
abline(lm(y2~x2,anscombe))

fit=lm(y3~x3,anscombe)
summary(fit)
plot(anscombe$x3,anscombe$y3,xlab="x3",ylab="y3",pch=16)
abline(lm(y3~x3,anscombe))

fit=lm(y4~x4,anscombe)
summary(fit)
plot(anscombe$x4,anscombe$y4,xlab="x4",ylab="y4",pch=16)
abline(lm(y4~x4,anscombe))





 
