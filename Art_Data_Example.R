art = read.csv("c:/data/art.csv")
summary(art)
tab0=table(art$Visit)
tab0

#Fit a simple linear regression model

artgroup=read.csv("c:/data/art-education group data.csv")
artgroup
fit0=lm(logodds~Education,artgroup)
summary(fit0)
plot(artgroup$Education,artgroup$logodds,pch=16)
abline(fit0)

#Fit a simple logistic regression model
fit1 = glm(Visit ~ Education, family=binomial, data = art)
summary(fit1)
vector1=data.frame(Education=c(1,2,3,4,5,6,7,8))
probs1=predict(fit1,newdata=vector1,type="response")
probs1
pred1=rep("No", 8)
pred1[probs1>0.3]="Yes"
pred1
tab1=table(fit1$y, fit1$fitted.values>.30)
tab1
CCR1=sum(diag(tab1))/sum(tab1)
CCR1


#Fit a multiple logistic regression model
fit2 = glm(Visit ~ Education+Income+Gender+Children+Married+factor(County),family=binomial, data = art)
summary(fit2)
vector2=data.frame(Education=6,Income=6,Gender=0,Children=1,Married=0,County=1)
probs2=predict(fit2,newdata=vector2,type="response")
probs2
pred2=rep("No", 1)
pred2[probs2>0.3]="Yes"
pred2
tab2=table(fit2$y, fit2$fitted.values>.30)
tab2
CCR2=sum(diag(tab2))/sum(tab2)
CCR2

artnomiss1=na.omit(art[,c(1,6)])
summary(artnomiss1)
artnomiss2=na.omit(art[,-2])
summary(artnomiss2)
dim(art)
dim(artnomiss1)
dim(artnomiss2)

fit1 = glm(Visit ~ Education, family=binomial, data = artnomiss1)
summary(fit1)
fit1 = glm(Visit ~ Education, family=binomial, data = artnomiss2)
summary(fit1)
fit2 = glm(Visit ~ Education+Income+Gender+Children+Married+factor(County),family=binomial, data = artnomiss2)
summary(fit2)

tab=table(fit2$y, fit2$fitted.values>.50)
tab
CCR=sum(diag(tab))/sum(tab)
CCR
tab=table(fit2$y, fit2$fitted.values>.60)
tab
CCR=sum(diag(tab))/sum(tab)
CCR
tab=table(fit2$y, fit2$fitted.values>.56)
tab
CCR=sum(diag(tab))/sum(tab)
CCR

# Linear Discriminant Analysis
library(MASS)
art = read.csv("c:/data/art.csv")
artnomiss1=na.omit(art[,c(1,6)])
summary(artnomiss1)
dim(artnomiss1)
tab0=table(artnomiss1$Visit)
tab0



artnomiss2=na.omit(art[,-2])
summary(artnomiss2)
dim(artnomiss2)

meanEd <- sapply(0:1, function(x) mean(artnomiss1$Education[artnomiss1$Visit==x]))
meanEd
varEd <- sapply(0:1, function(x) var(artnomiss1$Education[artnomiss1$Visit==x]))
varEd
SD <- sqrt((2.928285*1681+ 2.910008*924)/2605)
SD
LDF <- (5.698378-4.723543)/SD
LDF

fit3=lda(Visit ~ Education,data=artnomiss1,prior=c(1/2,1/2))
fit3

vector1=data.frame(Education=c(5))
probs3=predict(fit3,newdata=vector1,type="response")
probs3

n = dim(artnomiss1)[1]
Y.hat.3 = rep(0,n);
Y.prob.3 = predict(fit3, data=artnomiss1)$posterior 
for(i in 1:n){if(max(Y.prob.3[i,]) == Y.prob.3[i,1]){Y.hat.3[i]=1;}
else if(max(Y.prob.3[i,]) == Y.prob.3[i,2]){Y.hat.3[i]=2;}}

ctable3 = table(artnomiss1$Visit, Y.hat.3);
ctable3;
CCR3 = sum(diag(ctable3)[1:2])/n;
CCR3

fit4=lda(Visit ~Education+Income+Gender+Children+Married+factor(County),data=artnomiss2,prior=c(1/2,1/2))
fit4
n = dim(artnomiss2)[1]
Y.hat.4 = rep(0,n);
Y.prob.4 = predict(fit4, data=artnomiss2)$posterior 
for(i in 1:n){if(max(Y.prob.4[i,]) == Y.prob.4[i,1]){Y.hat.4[i]=1;}else if(max(Y.prob.4[i,]) == Y.prob.4[i,2]){Y.hat.4[i]=2;}}

ctable4 = table(artnomiss2$Visit, Y.hat.4);
ctable4;
CCR4 = sum(diag(ctable4)[1:2])/n;
CCR4




