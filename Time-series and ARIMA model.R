
# 1-Introduction to Time-Series Analysis

# Question 1.1)

set.seed(6822651)

CBK=read.csv("CBK.DE.csv")

# Example of the stationary times series (Plot of the Times Series of Commerzbank)
n=dim(CBK)[1]
time1= seq(2015, 2020+5/12, length.out = dim(CBK)[1])
plot(time1, CBK$Close, type= "l", ylab = "Closing Price", xlab = "Year", main = "Closing Price of Commerzbank 2015-2020")

# Example of the sationary times series (Plot of Return of the Commerzbank)
close_CBK  = CBK$Close
return_CBK= diff(log(close_CBK))
time2<-time1[2:n]
plot(time2,return_CBK,type="l",xlab="Year",ylab = "Return CBK")
title("The Return Series of Commerzbank")

# Fit and Similation of a AR(2) with 400 observation of commerzbank'data
set.seed(6822651)
n1=400
AR=arima(return_CBK, order =c(2, 0, 0))
AR

#Simulation of the fitted AR(2)-model
AR_s=arima.sim(n1, model=list(ar=c(0.0262, 0.0073))) # This model is stationary

#Plot the fitted AR(2)-Model: AR(2): X(t)= 0.0262X(t-1) + 0.0073X(t-2) + err(t)
plot(AR_s, type="l", xlab="time", ylab="values", main = "Stationary AR(2) ")


#Example of Simation of non sationary AR(2)-process 

AR_n=arima.sim(n1, model=list(ar=c(-0.91, 0.34))) #non-stationary AR(2)-Process:X(t)= -0.91X(t-1) + 0.34 X(t-2) + err(t)

AR_s=arima.sim(n1, model=list(ar=c(0.0262, 0.0073)))

AR_n= 1:n1*0
AR_n
AR_n[1:2]= AR_s[1:2]
AR_n
for(t in 3:n1)
{
  AR_n[t]= 0.0262*AR_n[t-1] + 0.0073*AR_n[t-2] + rnorm(1)
}

#Plot Stionary and non Stationary AR(2)-Process

plot(AR_n, type="l", xlab="time", ylab="values", main = "Stationary and non Stationary AR(2)")
lines(AR_s, col="red")
legend(x=40, y=-0.5, legend = c("nonstationary", "stationary"), col=c("black", "red"),lty = 1)

# Question 1.2-b)

beer=read.csv("ausbeer.csv")

# State the model
n2=dim(beer)[1]
time.beer= seq(1956, 2010+3/12, length.out = n2)
plot(time.beer, beer$value, type= "l", ylab = "value", xlab = "time", main = "Quarterly Australian Beer production")

Return.beer<- diff(log(beer$value))
plot(time.beer[2:n2],Return.beer, type="l",xlab="time",ylab = "value", main = "Return beer Production")


# Find the best Model using AIC or BIC

BIC = AIC =matrix(0, nrow = 3, ncol = 3)
for (p in 0:2)
{
  for (q in 0:2) 
    {
    model =arima(Return.beer, order =c(p, 0, q))
    AIC[p+1, q+1] = model$aic
    BIC[p+1, q+1] = -2*model$loglik+ log(n)*(p+q)
  }
  
}

AIC
min(AIC)
which(AIC==min(AIC))

best_model.AIC=arima(Return.beer, order = c(2, 0, 2))

best_model.AIC

#best_model.BIC=arima(ARMA, order = c(2, 0, 2))

# Check for invertibility and stationary of the model.
ARMA=arima.sim(n=n2, model= list(ar=c(-0.0267, -0.8974), ma=c(-1.0551, 0.6951)))
plot(ARMA, type="l", xlab="time", ylab="value", main="ARMA(2,0,2)")

