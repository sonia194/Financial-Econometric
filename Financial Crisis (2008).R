
# Problem 2

# 2.1

set.seed(6810782)

APPL<-read.csv("AAPL.csv")

PBCT<-read.csv("PBCT.csv")

# 2.2

#  the returns for both time-series

Close.Apple<-APPL$Close
Close.PBCT<-PBCT$Close


Return.APPLE<- diff(log(Close.Apple))
Return.PBCT<- diff(log(Close.PBCT))


#Plot of time series

n<-dim(APPL)[1]
time = seq(2005 + 4/12, 2018 + 4/12, length.out = n)

plot(time,Close.Apple,type="l",xlab="Year",ylab="Price",main="The APPLE and PBCT Index from 2005 to 2018")
lines(time,Close.PBCT,col="red")
legend(x =2006, y = 220, legend = c( "Apple","PBCT"), col = c("black", "red"), lty = 1)

#plot of Return

time2<-time[2:n]

plot(time2,Return.APPLE,type="l",xlab="Year",ylab = "Return")
title("The Return Series of Apple")

plot(time2,Return.PBCT,type="l",xlab="Year",ylab = "Return")
title("The Return Series of the PBCT")

#2.3

# fit the garch(1,1)

library(fGarch)

APPL.GARCH11<-garchFit(~garch(1,1),Return.APPLE,trace = FALSE)
summary(APPL.GARCH11)
PBCT.GARCH11<-garchFit(~garch(1,1),Return.PBCT,trace=FALSE)
summary(PBCT.GARCH11)


#plot the conditional volatility

sigma.t<-APPL.GARCH11@sigma.t
sigma.t1<-PBCT.GARCH11@sigma.t

plot(time2,Return.APPLE,type="l",xlab="Year",ylab="Returns",main= "Volatility and Returns of Apple")
lines(time2,sigma.t,col="red")
legend(x=2010,y=-0.05,legend = c("Returns of Apple","Volatility of Apple"),col = c("black","red"),lty=1)

plot(time2,Return.PBCT,type="l",xlab="Year",ylab="Returns",main= "Volatility and Returns of PBCT")
lines(time2,sigma.t1,col="red")
legend(x=2013,y=-0.02,legend = c("Returns of PBCT","Volatility of PBCT"),col = c("black","red"),lty=1)

#2.4

# Divide the Return-Series

b<-c(2007.5, 2009.5)

plot(time2,Return.APPLE,type="l",xlab="Year",ylab="Returns",main= " Returns of Apple")
abline(v=b,col="red", lty=2,lwd=3)
text(x=2006.5,y=-0.15,cex=0.8,labels = "before the financial crisis")
text(x=2009,y=0.10,cex=0.8,labels = "during the financial crisis")
text(x=2015,y=-0.15,cex=0.8,labels = "after the financial crisis")

plot(time2,Return.PBCT,type="l",xlab="Year",ylab="Returns",main= "Returns of PBCT")
abline(v=b,col="red", lty=2,lwd=3)
text(x=2006.5,y=-0.10,cex=0.8,labels = "before the financial crisis")
text(x=2009,y=0.10,cex=0.8,labels = "during the financial crisis")
text(x=2015,y=-0.10,cex=0.8,labels = "after the financial crisis")

# Plot For the Volatility of the both time-series

plot(time2,sigma.t,type="l",xlab="Year",main="Volatility of the Apple")
abline(v=b,col="red", lty=2,lwd=3)
text(x=2006.5,y=0.05,cex=0.8,labels = "before the financial crisis")
text(x=2009.5,y=0.06,cex=0.8,labels = "during the financial crisis")
text(x=2015,y=0.05,cex=0.8,labels = "after the financial crisis")

plot(time2,sigma.t1,type="l",xlab="Year",main="Volatility of the PBCT")
abline(v=b,col="red", lty=2,lwd=3)
text(x=2006.5,y=0.06,cex=0.8,labels = "before the financial crisis")
text(x=2009.5,y=0.07,cex=0.8,labels = "during the financial crisis")
text(x=2015,y=0.06,cex=0.8,labels = "after the financial crisis")

#2.5

#calculation of the correlation between Return-Service

# Separate Data

a<- which(APPL$Date=="2007-12-31")
b<-which(APPL$Date=="2009-06-30")
c<-dim(APPL)[1]


# the correlations between the return-series of the two firms

# the correlation before the crisis
cor1<-cor(Return.APPLE[1:a],Return.PBCT[1:a])
cor1

# the correlation during the crisis
cor2<-cor(Return.APPLE[(a+1):b],Return.PBCT[(a+1):b])
cor2

# the correlation after the crisis
cor3<-cor(Return.APPLE[(b+1):c],Return.PBCT[(b+1):c])
cor3

# the correlations between the estimated volatility of the two firms

# the correlations before the crisis  
cor11<-cor(sigma.t[1:a],sigma.t1[1:a])
cor11

# the correlations during the crisis
cor22<-cor(sigma.t[(a+1):b],sigma.t1[(a+1):b])
cor22

# the correlations after the crisis
cor33<-cor(sigma.t[(b+1):c],sigma.t1[(b+1):c])
cor33

