cardiac=read.csv("c:/data/cardiac.csv")
cardiac
plot(cardiac$Invasive,cardiac$Noninvasive)
abline(lm(Noninvasive~Invasive,cardiac))
cor(cardiac,method="pearson")
cor(cardiac,method="spearman")
cor(cardiac$Invasive,cardiac$Noninvasive)
cor.test(cardiac$Invasive,cardiac$Noninvasive)
fit=lm(Noninvasive~Invasive,cardiac)
summary(fit)
predict(fit,newdata=data.frame(Invasive=6.0),interval="predict")
predict(fit,newdata=data.frame(Invasive=6.0),interval="confidence")


 
