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


 
