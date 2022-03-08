###############################################################################
#                                                                             #
#                 Applied Project in R: Code for Task 1                       #
#                                                                             #
###############################################################################

# to include your own code after the simulation.

myMatrikel = 6822651 # put in your matriculation number here.

# 1-1) Start search for errors here
{
  n = 500
  
  set.seed(6822651)
  
  p = sample(1:6, size= 1)
  q = sample(1:6, size= 1)
  
  alpha = round(runif(q, min = 0, max = 1/q), digits = 3)
  beta  = round(runif(p, min = 0, max = 1/p), digits = 3)
  
  
  initY = rnorm(max(p, q))
  initErr = rnorm(max(p, q))
  
  y = err = 1:(2*n)*0
  y[1:max(p, q)] = initY
  err[1:max(p, q)] = initErr
  
  for (t in (max(p, q) + 1):length(y))
  {
    err[t] = rnorm(1)
    y[t] = beta %*% y[(t - 1):(t - p)] + alpha %*% err[(t - 1):(t - q)] + err[t]
  }
  
  Y = y[(n + 1)*(2*n)]
}  
# end search for errors here

# delete unneccessary variables 
rm(list = c("err", "initErr", "initY", "y", "t", "myMatrikel"))


#1-3)

#Simulation ARMA

ARMA=arima.sim(n=n, list(ar=beta, ma=alpha))

# Plot autocorrelation


acf(ARMA, lag.max = 10)
acf(ARMA, lag.max = 50)

#1-4)

AR=arima.sim(n=n, list(ar= beta))
AR.BIC = AR.AIC =  rep(0,15)
for (p in 0:15)
{
  model = arima(AR, order = c(p, 0, 0))
  AR.AIC[p + 1] = model$aic
  AR.BIC[p + 1] = -2*model$loglik + log(n)*(p + 0)
}

AR.AIC
AR.BIC
# find the best Models

min(AR.AIC)
which(AR.AIC==min(AR.AIC))
best_model.AR.AIC=arima(AR, order = c(2,0,0))
best_model.AR.AIC

min(AR.BIC)
which(AR.BIC==min(AR.BIC))
best_model.AR.BIC=arima(AR, order = c(2,0,0))
best_model.AR.BIC


#1-5)

BIC = AIC = matrix(0, nrow = 6, ncol = 6)
for (p in 0:5)
{
  for (q in 0:5)
  {
    model = arima(ARMA, order = c(p, 0, q))
    AIC[p + 1, q + 1] = model$aic
    BIC[p + 1, q + 1] = -2*model$loglik + log(n)*(p + q)
  }
}
AIC
BIC
# Find the best modell

min(AIC)
best_model.AIC=arima(ARMA, order = c(2, 0, 5))
best_model.AIC

min(BIC)
best_model.BIC=arima(ARMA, order = c(2, 0, 0))
best_model.BIC





