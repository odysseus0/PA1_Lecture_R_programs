library(nnet)
MBA=read.csv("c:/data/MBA.csv")
head(MBA)
meanGPA <- sapply(1:3, function(x) mean(MBA$GPA[MBA$admit==x]))
meanGMAT <- sapply(1:3, function(x) mean(MBA$GMAT[MBA$admit==x]))
meanGPA
meanGMAT

pdf("c:/users/atamhane/desktop/MBAplot.pdf", height=6, width=6)

plot(MBA$GPA, MBA$GMAT, col= MBA$admit, pch=MBA$admit,xlab="GPA",ylab="GMAT")
fit1=multinom(admit~GPA + GMAT, data = MBA,maxit=1000)
summary(fit1)
predict=predict(fit1,type='probs',newdata=data.frame(GPA=3.20,GMAT=450))
predict

# classify to the category for which it has the highest estimated probabilities

Y.prob.1 = fitted(fit1, outcome= FALSE);
Y.prob.1

n = dim(MBA)[1];
Y.hat.1 = rep(0,n);
for(i in 1:n){if(max(Y.prob.1[i,]) == Y.prob.1[i,1]){Y.hat.1[i]=1;}
else if(max(Y.prob.1[i,]) == Y.prob.1[i,2]){Y.hat.1[i]=2;}else if(max(Y.prob.1[i,]) == Y.prob.1[i,3]){Y.hat.1[i]=3;}}
Y.hat.1

ctable1 = table(MBA$admit, Y.hat.1);
ctable1;
CCR1 = sum(diag(ctable1)[1:3])/n;
CCR1

# Ordinal logistic regression
library(ordinal)
### just use levels and labels to specify which label is which
MBA$admit.ordered= ordered(MBA$admit,levels=c(1,2,3),labels=c(1,2,3))
MBA$admit.ordered= as.ordered(MBA$admit)
fit2=clm(admit.ordered~GPA+GMAT,data=MBA)
summary(fit2)
predict(fit2,newdata=data.frame(GPA=3.20,GMAT=450))
predict

# classify to the category for which it has the highest estimated probabilities
n = dim(MBA)[1];
Y.hat.2 = rep(0,n);
### use predict function over original data and call "fit" atribute
Y.prob.2 = predict(fit2, newdata=MBA[,1:2])$fit;
for(i in 1:n){if(max(Y.prob.2[i,]) == Y.prob.2[i,1]){Y.hat.2[i]=1;}else if(max(Y.prob.2[i,]) == Y.prob.2[i,2]){Y.hat.2[i]=2;}else if(max(Y.prob.2[i,]) == Y.prob.2[i,3]){Y.hat.2[i]=3;}}
ctable2 = table(MBA$admit, Y.hat.2);
ctable2;
CCR2 = sum(diag(ctable2)[1:3])/n;
CCR2

# Linear discriminant analysis
library(MASS)
fit3=lda(admit~GPA+GMAT,data=MBA,prior=c(1,1,1)/3)
fit3
predict(fit3,newdata=data.frame(GPA=3.20,GMAT=450))

# classify to the category for which it has the highest estimated probabilities
n = dim(MBA)[1]
Y.hat.3 = rep(0,n);
Y.prob.3 = predict(fit3,  newdata=MBA[,1:2])$posterior;
for(i in 1:n){if(max(Y.prob.3[i,]) == Y.prob.3[i,1]){Y.hat.3[i]=1;}
else if(max(Y.prob.3[i,]) == Y.prob.3[i,2]){Y.hat.3[i]=2;}
else if(max(Y.prob.3[i,]) == Y.prob.3[i,3]){Y.hat.3[i]=3;}}

ctable3 = table(MBA$admit, Y.hat.3);
ctable3;
CCR3 = sum(diag(ctable3)[1:3])/n;
CCR3
 
