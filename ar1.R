# dependencies ----
library(rjags)
library(R2jags)
library(dplyr)

# ar1 commands from the following link ----
# https://onlinecourses.science.psu.edu/stat510/node/61
# but don't know what to do with this information 
x <- g4$diff
x <- ts(x)
plot(x, type = "b", main = "Boston Celtics 2014-2015 Season and Results", xlab = "Game #", ylab = "Score Differential", 
     ylim = c(-40, 40))
abline(h = 0, col = "red")
# install.packages("astsa")
library(astsa)
lag1.plot(x, 1) # we don't see any relationship
acf(x, xlim = c(1, 82))
xlag1 <- lag(x, -1)
y <- cbind(x, xlag1)
ar1fit <- lm(y[, 1] ~ y[, 2]) # ar1fit here but didn't know it
summary(ar1fit)

# code below from Mark on assistance with ARIMA ----
arima(x, c(1, 0, 0))

# ar1 with jags...ugh ----
source(file = "../code/functions.R") # plot trace functions in this file
y.t <- x # to correspond with Leontine's jags code
ngames <- length(y.t) # will need for mod.txt
rho <- 0.058 # this and next line pertain to arima(...)
sigma <- sqrt(135.3) # 11.63

# let's set up jags
jags.data <- list(y.t = y.t, ngames = ngames)
parnames <- c("sigma", "rho")
set.seed(124)
mod <- jags.parallel(data = jags.data, parameters.to.save = parnames, 
                     n.chains = 4, n.burnin = 1000, n.iter = 1000 + 30000, n.thin = 30,
                     model.file = "jags-txt/mod.txt")
# jags diagnostics
max(mod$BUGSoutput$summary[, c("Rhat")])
min(mod$BUGSoutput$summary[, c("n.eff")])
which.max(mod$BUGSoutput$summary[, c("Rhat")])
which.min(mod$BUGSoutput$summary[, c("n.eff")])

PlotTrace("sigma", mod$BUGSoutput$sims.array)
PlotTrace("rho", mod$BUGSoutput$sims.array)

round(mod$BUGSoutput$summary[c("sigma", "rho"), ], 3)
sigma; rho

# put in presentation
