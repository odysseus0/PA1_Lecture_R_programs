library(MASS)
library(bestglm)
library(leaps)
art1 = read.csv("c:/data/art.csv")
art1$County = factor(art1$County)
art2 = art1[complete.cases(art1), -2]  # drop cases with missing values and the Age column

# forward stepwise with stepAIC
fit = glm(Visit ~ 1, family="binomial", art2)
step(fit, scope=~ Income + Gender + Children + Married + Education + County,direction = "both")

# backward stepwise with stepAIC
fit = glm(Visit ~ ., binomial, art2)
summary(fit)
step(fit, direction = "both")

#bestglm(data.frame(cbind(x=art2[,-1], y=art2$Visit)), family = binomial)
