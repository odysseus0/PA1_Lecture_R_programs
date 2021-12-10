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





 
