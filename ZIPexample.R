library(ggplot2)
library(pscl)

fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
head(fish)
tab=table(fish$count);tab
prop0=142/250;prop0
mean=mean(fish$count);mean
prob0=exp(-mean); prob0


summary(fish)
ggplot(fish, aes(count)) + geom_histogram() + scale_x_log10()
fit1 <- zeroinfl(count ~ child +  camper | persons, data = fish)
summary(fit1)
predict<- predict(fit1,newdata=data.frame(child=1,camper=1,persons=2));predict

fish0 = fish[fish$count == 0,]
fish1 = fish[fish$count > 0,]
dim(fish0)
dim(fish1)
fit2=glm(count ~ child +  camper, family=poisson, data=fish1)
summary(fit2)

fish$count1 = ifelse(fish$count == 0, 0, 1)
fit3=glm(count1~persons,family=binomial,data=fish)
summary(fit3)
predict=predict(fit3, type="response", newdata=data.frame(persons=2));predict


