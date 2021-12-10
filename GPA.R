gpa = read.csv("c:/data/GPA.csv")
#options(digits=4)
# Correlation matrix
cor(gpa)
# Scatterplot
plot(gpa)
source("c:/data/anova_table.R")
#Fit a linear model
linfit = lm(GPA ~ Verbal+Math , data = gpa)
summary(linfit)
anova_table(linfit)

anova(linfit)
drop1(linfit)
vcov(linfit)
predict(linfit,newdata=data.frame(Verbal=80,Math=90),interval="predict")
predict(linfit,newdata=data.frame(Verbal=80,Math=90),interval="confidence")
plot(linfit$fitted,linfit$resid,xlab="fitted values",ylab="residuals",pch=16)
abline(a=0,b=1,h=0)
vcov(linfit)
plot(linfit,which=6,pch=16)

wlsfit=lm(GPA ~ Verbal+Math, weights=1/(lmfit$fitted)^2,data = gpa)
summary(wlsfit)
plot(wlsfit$fitted,wlsfit$resid,xlab="fitted values",ylab="residuals")
abline(a=0,b=1,h=0)


plot(linfit, which=1:2,pch=16)
pdf("c:/users/atamhane/desktop/gpainfluenceplot.pdf", height=4, width=8)
par(mfrow=c(1,2))
plot(linfit,which=cbind(4,6),pch=16)
dev.off()
glmfit=glm(GPA ~ Verbal + Math, data = gpa,family=gaussian)
summary(glmfit)

#Fit a quadratic model
quadfit = lm(GPA ~ Verbal + Math +  I(Verbal^2) + I(Math^2), data = gpa)
summary(quadfit)
predict(quadfit,newdata=data.frame(Verbal=80,Math=90),interval="predict")
predict(quadfit,newdata=data.frame(Verbal=80,Math=90),interval="confidence")

anova(linfit,quadfit)
plot(quadfit, which=1:2)
plot(quadfit,which=c(4,6))
logquadfit = lm(log(GPA) ~ Verbal + Math + I(Verbal^2) + I(Math^2), data = gpa)
summary(logquadfit)
plot(logquadfit, which=1:2)

#Fit a quadratic model with interactions
intfit = lm(GPA ~ Verbal*Math + I(Verbal^2) + I(Math^2), data = gpa)
summary(intfit)
anova(quadfit,intfit)
pdf("c:/users/Ajit Tamhane/desktop/gpaplot.pdf", height=4, width=8)
par(mfrow=c(1,2))
plot(infit, which=1:2)
dev.off()

gpa$logGPA=log(gpa$GPA)
inlogfit = lm(logGPA ~ Verbal + Math + Verbal:Math + I(Verbal^2) + I(Math^2), data = gpa)
pdf("c:/users/Ajit Tamhane/desktop/gpalogplot.pdf", height=4, width=8)
par(mfrow=c(1,2))
plot(inlogfit, which=1:2)
dev.off()

# Influential observations and outliers
rstandard(linfit)
rstudent(linfit)
hatvalues(linfit)
cooks.distance(linfit)





