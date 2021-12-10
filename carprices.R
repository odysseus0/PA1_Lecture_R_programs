data = read.csv("c:/data/carprices.csv")
names(data)
summary(data)
data$Make <- relevel(data$Make, "Cadillac")
data$Type <- relevel(data$Type, "Coupe")
# Put every alternate observation into the training set and the test set.
train_indices <- seq(1, nrow(data), by=2)
train <- data[train_indices, ]
test <- data[-train_indices, ]

# Randomly split the data into the training set and the test set.

train_indices <- sample(1:nrow(data), floor(0.5*nrow(data)), replace=F) # 50:50 split
train <- data[train_indices, ]
test <- data[-train_indices, ]

# Use Price as response variable
fit = lm(Price ~ Mileage+Liter+factor(Make)+factor(Type), data = train)
summary(fit)
fit = lm(Price ~ Mileage+Liter+Cadillac+Chevrolet+Pontiac+SAAB+Saturn+factor(Type), data = train)
summary(fit)

library(car)
vif(fit)
plot(fit,which=1:2)
plot(fit,which=cbind(4,6))
library(MASS)
stdres(fit)[abs(stdres(fit))>3]


# Use log(Price) as response variable
logfit = lm(log10(Price) ~ Mileage+Cylinder+Liter+factor(Make)+factor(Type), data = data)
summary(logfit)
plot(logfit,which=1:2)
plot(logfit,cbind(4,6))
stdres(logfit)[abs(stdres(logfit))>3]

# SSE and R^2 for test data with Price as response variable
SSE_test <- sum((test$Price - predict(fit, test))^2)
SSE_test
SST_test <- sum((test$Price - mean(test$Price))^2)
SST_test 
R2_test <- 1 - SSE_test/SST_test
R2_test

# SSE and R^2 for test data with log(Price) as response variable
SSElog_test <- sum((test$Price - exp(predict(logfit, test)))^2)
SSElog_test
SST_test <- sum((test$Price - mean(test$Price))^2)
SST_test 
R2log_test <- 1 - SSElog_test/SST_test
R2log_test


# Cross-Validation SSE
n_folds <- 5
CV_indices <- as.list(rep(NA, n_folds)) # a list with n_folds empty elements
shuffled_indices <- sample(1:nrow(data))
for(i in 1:n_folds){
	CV_indices[[i]] <- shuffled_indices[(floor(nrow(data)*(i-1)/n_folds)+1):floor(nrow(data)*i/n_folds)]
}
SSE_CV <- 0
for(i in 1:n_folds){
	fit_CV <- lm(Price ~ Mileage+Liter+factor(Make)+factor(Type), data = data[-CV_indices[[i]],])
	SSE_CV <- SSE_CV + sum((predict(fit_CV, data[CV_indices[[i]],]) - data$Price[CV_indices[[i]]])^2)
}
SST_CV <- sum((data$Price-mean(data$Price))^2)
R2_CV <- 1 - SSE_CV/SST_CV
R2_CV
