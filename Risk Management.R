
# 3- Risk Management

# find the time_series

set.seed(6810782)
DAI <- read.csv("DAI.DE.csv")
SAP<- read.csv("SAP.DE.csv")
DPW <- read.csv("DPW.DE.csv")
DTE <- read.csv("DTE.DE.csv")

#Display Stock price
n<-dim(DAI)[1]

time = seq(2015 + 4/12, 2018 + 4/12 , length.out = n)
par(mfrow=c(2,2))

plot(time,DAI$Close,type="l",xlab="Year",ylab="Price",main="The Daimler Index from 2015 to 2018")

plot(time,SAP$Close,type="l",xlab="Year",ylab="Price",main="The SAP Index from 2015 to 2018")

plot(time,DPW$Close,type="l",xlab="Year",ylab="Price",main="The DPW Index from 2015 to 2018")

plot(time,DTE$Close,type="l",xlab="Year",ylab="Price",main="The DTE Index from 2005 to 2018")

# Compute Return

Return_DAI<-diff(log(DAI$Close))
Return_SAP<-diff(log(SAP$Close))
Return_DPW<-diff(log(DPW$Close))
Return_DTE<-diff(log(DTE$Close))

# Display Return

time2=time[2:n]
par(mfrow=c(2,2))
plot(time2,Return_DAI,type="l",xlab="Year",ylab = "Return")
title("The Return Series of DAI")

plot(time2,Return_SAP,type="l",xlab="Year",ylab = "Return")
title("The Return Series of SAP")

plot(time2,Return_DPW,type="l",xlab="Year",ylab = "Return")
title("The Return Series of DPW")

plot(time2,Return_DTE,type="l",xlab="Year",ylab = "Return")
title("The Return Series of DTE")

# Fit 4 GARCH(p,q) models for p, q = 1, 2

library(fGarch)

# DAI
garchFit(~garch(1,1), Return_DAI, trace=FALSE)@fit$ics[2] # smallest BIC
garchFit(~garch(1,2), Return_DAI, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,1), Return_DAI, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,2), Return_DAI, trace=FALSE)@fit$ics[2]



# the best Model IS GARCH(1,1)

GARCH_DAI<- garchFit(~garch(1,1), Return_DAI, trace=FALSE)
GARCH_DAI@fit$coef

#SAP
garchFit(~garch(1,1), Return_SAP, trace=FALSE)@fit$ics[2]# smallest BIC
garchFit(~garch(1,2), Return_SAP, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,1), Return_SAP, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,2), Return_SAP, trace=FALSE)@fit$ics[2]


# Best Model IS GARCH(1,1)

GARCH_SAP <- garchFit(~garch(1,1), Return_SAP, trace=FALSE)
GARCH_SAP@fit$coef

#DPW
garchFit(~garch(1,1), Return_DPW, trace=FALSE)@fit$ics[2] # smallest BIC
garchFit(~garch(1,2), Return_DPW, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,1), Return_DPW, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,2), Return_DPW, trace=FALSE)@fit$ics[2]


# the best Model IS GARCH(1,1)

GARCH_DPW<- garchFit(~garch(1,1), Return_DPW, trace=FALSE)
GARCH_DPW@fit$coef

#DTE
garchFit(~garch(1,1), Return_DTE, trace=FALSE)@fit$ics[2] # smallest BIC
garchFit(~garch(1,2), Return_DTE, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,1), Return_DTE, trace=FALSE)@fit$ics[2]
garchFit(~garch(2,2), Return_DTE, trace=FALSE)@fit$ics[2]



# the best Model IS GARCH(1,1)

GARCH_DTE<- garchFit(~garch(1,1), Return_DTE, trace=FALSE)
GARCH_DTE@fit$coef

# APARCH

# DAI
garchFit(~aparch(1,1), Return_DAI, trace=FALSE)@fit$ics[2] # smallest BIC
garchFit(~aparch(1,2), Return_DAI, trace=FALSE)@fit$ics[2]
garchFit(~aparch(2,1), Return_DAI, trace=FALSE)@fit$ics[2]
garchFit(~aparch(2,2), Return_DAI, trace=FALSE)@fit$ics[2]

#the best Model IS APARCH(1,1)

APARCH_DAI<- garchFit(~aparch(1,1), Return_DAI, trace=FALSE)
APARCH_DAI@fit$coef

#SAP
garchFit(~aparch(1,1), Return_SAP, trace=FALSE)@fit$ics[2]# smallest BIC
garchFit(~aparch(1,2), Return_SAP, trace=FALSE)@fit$ics[2]
garchFit(~aparch(2,1), Return_SAP, trace=FALSE)@fit$ics[2]
garchFit(~aparch(2,2), Return_SAP, trace=FALSE)@fit$ics[2]

#the best Model IS APARCH(1,1)

APARCH_SAP<- garchFit(~aparch(1,1), Return_SAP, trace=FALSE)
APARCH_SAP@fit$coef

#DPW
garchFit(~aparch(1,1), Return_DPW, trace=FALSE)@fit$ics[2] # smallest BIC
garchFit(~aparch(1,2), Return_DPW, trace=FALSE)@fit$ics[2]
garchFit(~aparch(2,1), Return_DPW, trace=FALSE)@fit$ics[2]
garchFit(~aparch(2,2), Return_DPW, trace=FALSE)@fit$ics[2]

# the best Model IS APARCH(1,1)

APARCH_DPW<- garchFit(~aparch(1,1), Return_DPW, trace=FALSE)
APARCH_DPW@fit$coef

#DTE
garchFit(~aparch(1,1), Return_DTE, trace=FALSE)@fit$ics[2]
garchFit(~aparch(1,2), Return_DTE, trace=FALSE)@fit$ics[2] # smallest BIC
garchFit(~aparch(2,1), Return_DTE, trace=FALSE)@fit$ics[2]
garchFit(~aparch(2,2), Return_DTE, trace=FALSE)@fit$ics[2]

# the best Model IS APARCH(1,2)

APARCH_DTE<- garchFit(~aparch(1,2), Return_DTE, trace=FALSE)
APARCH_DTE@fit$coef


#Calculate the 0.95 VaR and ES for both models and all companies

#DAI

#GARCH

sigma_GARCH_DAI<-GARCH_DAI@sigma.t
VaR_GARCH_DAI95 = qnorm(0.95) * sigma_GARCH_DAI
ES_GARCH_DAI95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_GARCH_DAI


#APARCH

sigma_APARCH_DAI<-APARCH_DAI@sigma.t
VaR_APARCH_DAI95 = qnorm(0.95) * sigma_APARCH_DAI
ES_APARCH_DAI95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_APARCH_DAI

#plot

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)

plot(time2,Return_DAI,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (GARCH) of DAI")
lines(time2, VaR_GARCH_DAI95, col = 2)
lines(time2, ES_GARCH_DAI95, col = 4)

plot(time2,Return_DAI,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (APARCH) of DAI")
lines(time2, VaR_APARCH_DAI95, col = 2)
lines(time2, ES_APARCH_DAI95, col = 4)

legend(y = -0.14, x = 2017, legend = c("Return", "VaR", "ES"),
       col = c(1, 2, 4), lwd = 1, horiz = TRUE)
#SAP

#GARH

sigma_GARCH_SAP<-GARCH_SAP@sigma.t
VaR_GARCH_SAP95 = qnorm(0.95) * sigma_GARCH_SAP
ES_GARCH_SAP95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_GARCH_SAP

#APARCH

sigma_APARCH_SAP<-APARCH_SAP@sigma.t
VaR_APARCH_SAP95 = qnorm(0.95) * sigma_APARCH_SAP
ES_APARCH_SAP95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_APARCH_SAP

# Plot

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)

plot(time2,Return_SAP,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (GARCH) of SAP")
lines(time2, VaR_GARCH_SAP95, col = 2)
lines(time2, ES_GARCH_SAP95, col = 4)

plot(time2,Return_SAP,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (APARCH) of SAP")
lines(time2, VaR_APARCH_SAP95, col = 2)
lines(time2, ES_APARCH_SAP95, col = 4)

legend(y = -0.11, x = 2017, legend = c("Return", "VaR", "ES"),
       col = c(1, 2, 4), lwd = 1, horiz = TRUE)

#DPW

#GARCH

sigma_GARCH_DPW<-GARCH_DPW@sigma.t
VaR_GARCH_DPW95 = qnorm(0.95) * sigma_GARCH_DPW
ES_GARCH_DPW95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_GARCH_DPW

#APARCH

sigma_APARCH_DPW<-APARCH_DPW@sigma.t
VaR_APARCH_DPW95 = qnorm(0.95) * sigma_APARCH_DPW
ES_APARCH_DPW95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_APARCH_DPW

#Plot

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)

plot(time2,Return_DPW,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (GARCH) of DPW")
lines(time2, VaR_GARCH_DPW95, col = 2)
lines(time2, ES_GARCH_DPW95, col = 4)

plot(time2,Return_DPW,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (APARCH) of DPW")
lines(time2, VaR_APARCH_DPW95, col = 2)
lines(time2, ES_APARCH_DPW95, col = 4)

legend(y = -0.12, x = 2017, legend = c("Return", "VaR", "ES"),
       col = c(1, 2, 4), lwd = 1, horiz = TRUE)
# DTE
#GARCH

sigma_GARCH_DTE<-GARCH_DTE@sigma.t
VaR_GARCH_DTE95 = qnorm(0.95) * sigma_GARCH_DTE
ES_GARCH_DTE95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_GARCH_DTE


#APARCH

sigma_APARCH_DTE<-APARCH_DTE@sigma.t
VaR_APARCH_DTE95 = qnorm(0.95) * sigma_APARCH_DTE
ES_APARCH_DTE95 = dnorm(qnorm(0.95))/(1 - 0.95) * sigma_APARCH_DTE

#Plot

par(mfrow = c(2, 1), cex = 0.75, xpd = NA)

plot(time2,Return_DTE,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (GARCH) of DTE")
lines(time2, VaR_GARCH_DTE95, col = 2)
lines(time2, ES_GARCH_DTE95, col = 4)

plot(time2,Return_DTE,type="l",xlab="Year",ylab = "Return")
title("The Return Series and 95% VAR, ES (APARCH) of DTE")
lines(time2, VaR_APARCH_DTE95, col = 2)
lines(time2, ES_APARCH_DTE95, col = 4)

legend(y = -0.11, x = 2017, legend = c("Return", "VaR", "ES"),
       col = c(1, 2, 4), lwd = 1, horiz = TRUE)

# GARCH and APARCH of DAIMLER
# DAI
#GARCH

garchFit(~garch(1,1), Return_DAI, trace=FALSE)@fit$coef
garchFit(~garch(1,2), Return_DAI, trace=FALSE)@fit$coef
garchFit(~garch(2,1), Return_DAI, trace=FALSE)@fit$coef
garchFit(~garch(2,2), Return_DAI, trace=FALSE)@fit$coef

# APARCH

garchFit(~aparch(1,1), Return_DAI, trace=FALSE)@fit$coef
garchFit(~aparch(1,2), Return_DAI, trace=FALSE)@fit$coef
garchFit(~aparch(2,1), Return_DAI, trace=FALSE)@fit$coef
garchFit(~aparch(2,2), Return_DAI, trace=FALSE)@fit$coef


