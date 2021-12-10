install.packages("pROC")
library(pROC)
art = read.csv("c:/data/art.csv")
summary(art)
artnomiss=na.omit(art[, c(-2)]) # data omitting missing obs. on all predictors
dim(artnomiss)

# ROC curve for simple logisitic regression vs. Education
fit1 = glm(Visit ~ Education, family=binomial, data = artnomiss)
summary(fit1)
pdf("c:/users/atamhane/desktop/ArtROC1.pdf", height=6, width=6)
plot.roc(artnomiss$Visit, fit1$fitted.values,print.auc=T,xlab="Specificity",
ylab="Sensitivity")
dev.off()

# ROC curve for multiple logisitic regression
fit2 = glm(Visit ~ Education+Income+Gender+Children+Married+factor(County),family=binomial, data = artnomiss)
summary(fit2)
pdf("c:/users/atamhane/desktop/ArtROC2.pdf", height=6, width=6)
plot.roc(artnomiss$Visit, fit2$fitted.values,print.auc=T,xlab="Specificity",
ylab="Sensitivity")
dev.off()
