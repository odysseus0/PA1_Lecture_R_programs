library(survival)
head(jasa)
# this includes time dependent covariates
jasa$subject <- 1:nrow(jasa) #we need an identifier variable
tdata <- with(jasa, data.frame(subject = subject,
survtime= pmax(.5, fu.date - accept.dt),
txtime= ifelse(tx.date== fu.date,
(tx.date -accept.dt) -.5,
(tx.date - accept.dt)),
fustat = fustat
))
sdata <- tmerge(jasa, tdata, id=subject,
death = event(futime, fustat),
trt = tdc(txtime),
options= list(idname="subject"))
attr(sdata, "tcount")
sdata$age <- sdata$age -48
sdata$year <- as.numeric(sdata$accept.dt - as.Date("1967-10-01"))/365.25
# model 6 of the table in K&P
sfit<-coxph(Surv(tstart, tstop, death) ~ trt+age + surgery + year,
data= sdata)
sfit

# this does not include time dependent covariates
jasa$subject <- 1:nrow(jasa) #we need an identifier variable
tdata <- with(jasa, data.frame(subject = subject,
survtime= pmax(.5, fu.date - accept.dt),
treattime= ifelse(tx.date== fu.date,
(tx.date -accept.dt) -.5,
(tx.date - accept.dt)),
death = fustat,
age=age,
surgery=surgery
))
tdata$year <- as.numeric(jasa$accept.dt - as.Date("1967-10-01"))/365.25
tdata$transplant = as.numeric(!is.na(tdata$treattime))
tfit<-coxph(Surv(survtime, death) ~ transplant+age+surgery+year, data= tdata)
tfit


sdata2 <- tmerge(jasa, tdata, id=subject,
death = event(futime, fustat),
options= list(idname="subject"))
attr(sdata2, "tcount")
