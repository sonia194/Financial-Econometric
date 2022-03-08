
#4 High-Frequency Financial 

set.seed(6810782)

HF_BMW<- read.csv("HF-BMW.csv",header = FALSE)

Data1<- HF_BMW[82,] 
Day_1<-unlist(Data1,recursive = TRUE,use.names=FALSE)

Data2<- HF_BMW[83,]
Day_2<- unlist(Data2,recursive = TRUE,use.names = FALSE)

Data3<- HF_BMW[84,]
Day_3<- unlist(Data3,recursive = TRUE,use.names = FALSE)

#Plot all three time-series


n <- length(Day_1)

X<-format(seq(as.POSIXct( "2020-08-22 09:00:00", tz="GMT"), length.out=n,  by='1 min'), '%H:%M')

par(mfrow = c(3, 1))

plot(Day_1,type="l",xaxt="n",xlab="time",ylab="Price",main="the BMW index of the day 1")
axis(1,at=1:511,labels =X[1:511])

plot(Day_2,type="l",xaxt="n",xlab="time",ylab="Price",main="the BMW index of the day 2")
axis(1,at=1:511,labels = X[1:511])

plot(Day_3,type="l",xaxt="n",xlab="time",ylab="Price",main="the BMW index of the day 3")
axis(1,at=1:511,labels = X[1:511])
# Calculate the Return

Return_Day_1<- diff(log(Day_1))
Return_Day_2<- diff(log(Day_2))
Return_Day_3<- diff(log(Day_3))

# Plot the Return

par(mfrow=c(3,1))
plot(Return_Day_1,type="l",xaxt="n",xlab="time",ylab = "Return",main="The Return Series of BMW of the day 1")
axis(1,at=2:511,labels = X[2:511])

plot(Return_Day_2,type="l",xaxt="n",xlab="time",ylab = "Return",main="The Return Series of BMW of the day 2")
axis(1,at=2:511,labels = X[2:511])

plot(Return_Day_3,type="l",xaxt="n",xlab="time",ylab = "Return",main="The Return Series of BMW of the day 3")
axis(1,at=2:511,labels = X[2:511])
# Calculate Volatility
library(fGarch)

Return_Day_1.GARCH11=garchFit(~garch(1,1), Return_Day_1, trace=FALSE)
volatility_Day_1<- Return_Day_1.GARCH11@sigma.t


Return_Day_2.GARCH11=garchFit(~garch(1,1), Return_Day_2, trace=FALSE)
volatility_Day_2 <- Return_Day_2.GARCH11@sigma.t




Return_Day_3.GARCH11=garchFit(~garch(1,1), Return_Day_3, trace=FALSE)
volatility_Day_3 <- Return_Day_3.GARCH11@sigma.t

data<-data.frame(volatility_Day_1,volatility_Day_2,volatility_Day_3)
head(data)

