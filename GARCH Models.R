
# 2-Financial Time-Series

library(fGarch)

AAPL = read.csv("AAPL.csv")

n = dim(AAPL)[1]
R = diff(log(AAPL$Close))

# Fit the simple GARCH model
garchFit(~garch(1, 1), R, trace = FALSE)@fit$ics[2] # best model
garchFit(~garch(1, 2), R, trace = FALSE)@fit$ics[2]
garchFit(~garch(2, 1), R, trace = FALSE)@fit$ics[2]
garchFit(~garch(2, 2), R, trace = FALSE)@fit$ics[2]

GARCH = garchFit(~garch(1, 1), R, trace = FALSE) 
GARCH@fit$coef

# Fit the APPARCH model
garchFit(~aparch(1, 1), R, trace = FALSE)@fit$ics[2]# best model
garchFit(~aparch(1, 2), R, trace = FALSE)@fit$ics[2]
garchFit(~aparch(2, 1), R, trace = FALSE)@fit$ics[2]
garchFit(~aparch(2, 2), R, trace = FALSE)@fit$ics[2]

APARCH = garchFit(~aparch(1, 1), R, trace = FALSE)
APARCH@fit$coef

# Plot of the volatility of the GARCH(1,1)

time = seq(2018, 2020, length.out = n)
Year = time[2:n]
plot(Year, GARCH@sigma.t, type = "l", ylab = "Volatility", main = "GARCH Model")

# plot of the GARCH by the APPARCH(1,1) extension
plot(Year, GARCH@sigma.t, type = "l", ylab = "volatility",)
lines(Year, APARCH@sigma.t, col = "red")
title("Estimated volatility by APPARCH extension (AAPL)")
legend(y = 0.09, x = 2018.0, legend = c("GARCH", "APPARCH"), col = c("black", "red"), lty = 1)

# calculate the VaR and the ES alpha = 0.025
# estimate GARCH(1,1)
GARCH = garchFit(~garch(1,1), R, trace = FALSE)
sigma_n = GARCH@sigma.t

#define the sequences. 
Year = seq(2018, 2020, length.out = n)
time = Year[2:n]

# calculate VaR and ES
VaR_n975 = qnorm(0.975) * sigma_n
ES_n975 = dnorm(qnorm(0.975))/(1 - 0.975) * sigma_n

# plot the results
#par(mfrow = c(1, 1), cex = 0.60, xpd = NA)
plot(time, R, ylab = "Returns", type = "l")
title("97.5% Risk Measures under a Normal Distribution")
lines(time, VaR_n975, col = 2)
lines(time, ES_n975, col = 4)
legend(y = -0.02, x = 2018.0, legend = c("ES", "VaR"), col = c("red", "blue"), lty = 1)

# simaulation
set.seed(6832815) # reproducibility
n <- 500 # sample size
spec = garchSpec(model = list(omega = 1e-6, mu = 0.005, alpha = c(0.12, 0.04),
                              beta = c(0.1, 0.08)))
data = garchSim(spec, n, extended = TRUE)

# plot of the volatility and the simulated returns
plot(data$sigma, type = "l", ylab = "Volatility", main = "Simulated Volatility") 
    
plot(data$eps, type = "l", ylab = "volatility", main = "simulated Returns")


# Fit he GARCH(p,q) with p,q = 1,2 and find thee best model with BIC
garchFit(~garch(1, 1), data$eps, trace = FALSE)@fit$ics[2]# best model
garchFit(~garch(1, 2), data$eps, trace = FALSE)@fit$ics[2]
garchFit(~garch(2, 1), data$eps, trace = FALSE)@fit$ics[2]
garchFit(~garch(2, 2), data$eps, trace = FALSE)@fit$ics[2]

GARCH = garchFit(~garch(1, 1), data$eps, trace = FALSE)
GARCH@fit$coef

