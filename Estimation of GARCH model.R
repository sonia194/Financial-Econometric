#Problem 3

#3.1

set.seed(6832815)

AMZN<- read.csv("AMZ.DE.csv")

AMZN$Return=c(NA,diff(log(AMZN$Close)))

#plot

plot.ts(AMZN[,c(5,8)],plot.type="multiple",type="l",main="The Amazon Index and log-Return,from 12.jun.2017  to 12 jun 2020")

#3.2

#Datentrennung

n<-dim(AMZN)[1]
AMZN_1<-AMZN[1:which(AMZN$Date=="2019-06-12"),]
AMZN_2<-AMZN[(which(AMZN$Date=="2019-06-12")+1):n,]

# Fit 4 GARCH(p,q) models for p, q = 1, 2

library(fGarch)

AMZN_1.GARCH11<-garchFit(~garch(1,1),AMZN_1$Return[-1],trace=FALSE)
AMZN_1.GARCH11
AMZN_1.GARCH12=garchFit(~garch(1,2), AMZN_1$Return[-1], trace=FALSE)
AMZN_1.GARCH12
AMZN_1.GARCH21=garchFit(~garch(2,1), AMZN_1$Return[-1], trace=FALSE)
AMZN_1.GARCH21
AMZN_1.GARCH22=garchFit(~garch(2,2), AMZN_1$Return[-1], trace=FALSE)
AMZN_1.GARCH22

# BIC

BIC11<-AMZN_1.GARCH11@fit$ics[2]
BIC12<-AMZN_1.GARCH12@fit$ics[2]
BIC21<-AMZN_1.GARCH21@fit$ics[2]
BIC22<-AMZN_1.GARCH22@fit$ics[2]

#show in the table

BIC<-matrix(data=c(BIC11,BIC21,BIC12,BIC22),nrow = 2,ncol=2)
BIC
min(BIC)

# the best Model

AMZN_1.GARCH11


#3.3

#Predict the conditional volatility of year 3 of the time series by using your model from

prediction=predict(AMZN_1.GARCH11,n.ahead=dim(AMZN_2)[1],plot=FALSE)
Date<-AMZN_2$Date
prediction=cbind(Date,prediction)

#head the 6 first Data and 6 last Data

head(prediction)
tail(prediction)

#3.4 Estimating a new Garch Model

AMZN.GARCH11<-garchFit(~garch(1,1),AMZN$Return[-1],trace=FALSE)
summary(AMZN.GARCH11)

#volatilität

sigma<-AMZN.GARCH11@sigma.t

#Plot(sigma)

time= seq(2017 + 1/3 , 2020 + 1/3 , length.out= n)
time2= time[2:n]
time3= time[which(AMZN$Date=="2019-06-13"):n]

plot(time2,sigma,type="l",xlab="Year",ylab="volatility",main="Estimated conditional standard deviation and predicted Values by GARCH11")
lines(time3,prediction$standardDeviation,col="red",type="l")
legend(x=2018.5,y=0.055,legend = c(" the volatility","the predicted Volatility"),col = c("black","red"),lty=1)



