#### dependencies ----
library(rjags)
library(R2jags)
library(dplyr)

AddCIs <- function(CI.low.t, # lower bound for seq.years.t
                   CI.up.t, # upper bound for seq.years.t
                   seq.years.t, col = 1){
  # add CIs to a plot.
  col = adjustcolor(col, alpha.f = 0.1)
  for (t in 2:length(seq.years.t))
    polygon(c(seq.years.t[t-1], seq.years.t[t-1], seq.years.t[t], seq.years.t[t],seq.years.t[t-1]),
            c(CI.low.t[t-1], CI.up.t[t-1], CI.up.t[t], CI.low.t[t], CI.low.t[t-1]),
            col=col, border = NA)
}

#### ar1 commands from the following link ----
# https://onlinecourses.science.psu.edu/stat510/node/61
# but don't know what to do with this information 
x <- g4$diff # time series object here and below this line
x <- ts(x)

#plot
plot(x, type = "b", main = "Boston Celtics 2014-2015 Season and Results", 
     xlab = "Game #", ylab = "Score Differential", 
     ylim = c(-40, 40))
abline(h = 0, col = "red")
axis(side = 1, at = seq(10, 82, 10))

# install.packages("astsa")
library(astsa)
lag1.plot(x, 1) # we don't see any relationship
acf(x, xlim = c(1, 82))
xlag1 <- lag(x, -1)
y <- cbind(x, xlag1)
ar1fit <- lm(y[, 1] ~ y[, 2]) # ar1fit here but didn't know it
summary(ar1fit)


#### ar1 with jags...ugh ----
source(file = "../code/functions.R") # plot trace functions in this file
y.t <- x # to correspond with Leontine's jags code
ngames <- length(y.t) # will need for mod.txt
arima(x, c(1, 0, 0)) # code below from Mark on assistance with ARIMA
rho <- 0.058 # this and next line pertain to arima(...)
sigma <- sqrt(135.3) # 11.63
P <- 20 # we'll keep this at 20 for all games

# let's set up jags
jags.data <- list(y.t = y.t, ngames = ngames, P = P)
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
title(sub = "Model 1 (2014-15) sigma")
PlotTrace("rho", mod$BUGSoutput$sims.array)
title(sub = "Model 1 (2014-15) rho")

round(mod$BUGSoutput$summary[c("sigma", "rho"), ], 3)
sigma; rho

# forecasting 
rho.s <- mod2$BUGSoutput$sims.list[["rho"]]
sigma.s <- mod$BUGSoutput$sims.list[["sigma"]]
S <- length(rho.s)
y.sp <- matrix(NA, S, P)
set.seed(1)
y.sp[, 1] <- rnorm(S, rho.s*y.t[ngames], sigma.cc.s)
for(p in 2:P){y.sp[, p] <- rnorm(S, rho.s*y.sp[, p - 1], sigma.s)}
# construct point forecasts and PIs:
y.qp <- apply(y.sp, 2, quantile, c(0.025, 0.5, 0.975))

plot(y.t ~ seq(1, ngames), type = "b", 
     xlim = c(ngames - 10, ngames + P), ylim = c(-45, 40), 
     xlab = "Game #")
lines(y.qp[1,] ~ seq(ngames + 1, ngames + P), type = "l", col = 2, lty = 2)
lines(y.qp[2,] ~ seq(ngames + 1, ngames + P), type = "l", col = 2)
lines(y.qp[3,] ~ seq(ngames + 1, ngames + P), type = "l", col = 2, lty = 2)
# some example trajectories
for (s in c(1,501,1000)){
  lines(y.sp[s,] ~ seq(ngames + 1, ngames + P), type = "l", col = "purple")
}

#### ar1 with spline residuals ----
spl.res <- read.csv(file = "data/doc_residuals.csv")[, 2] # only the residuals
arima(spl.res, c(1, 0, 0))
nsplres <- length(spl.res)
rho.spl <- -0.4266
sigma.spl <- sqrt(5.272) # 11.63
y.t <- spl.res # matches up with Leontine's code, again

# let's set up jags via spline residuals
jags.data <- list(y.t = y.t, nsplres = nsplres, P)
parnames <- c("sigma", "rho")
set.seed(124)
mod1 <- jags.parallel(data = jags.data, parameters.to.save = parnames, 
                     n.chains = 4, n.burnin = 1000, n.iter = 1000 + 30000, n.thin = 30,
                     model.file = "jags-txt/mod1.txt")

# jags diagnostics via spline residuals
max(mod1$BUGSoutput$summary[, c("Rhat")])
min(mod1$BUGSoutput$summary[, c("n.eff")])
which.max(mod1$BUGSoutput$summary[, c("Rhat")])
which.min(mod1$BUGSoutput$summary[, c("n.eff")])

PlotTrace("sigma", mod1$BUGSoutput$sims.array)
PlotTrace("rho", mod1$BUGSoutput$sims.array)

round(mod1$BUGSoutput$summary[c("sigma", "rho"), ], 3)
sigma.spl; rho.spl

#### ar1 for selected celtics compiled ----
celt.comp <- read.csv(file = "data/celtics_compiled.csv")[, 3] # only the diff
arima(celt.comp, c(1, 0, 0)) # report rho and sigma^2
acf(celt.comp, xlim = c(1, 50))
nceltcomp <- length(celt.comp)
rho.cc <- 0.1337 # cc for celtics compilation
sigma.cc <- sqrt(157.9) # 12.566
y.t <- celt.comp # matches up with Leontine's code, again
P <- 20 # forecasting

# let's set up jags via celtics compiled
jags.data <- list(y.t = y.t, nceltcomp = nceltcomp, P = P)
parnames <- c("sigma", "rho")
set.seed(124)
mod2 <- jags.parallel(data = jags.data, parameters.to.save = parnames, 
                      n.chains = 4, n.burnin = 1000, n.iter = 1000 + 30000, n.thin = 30,
                      model.file = "jags-txt/mod2.txt")

# jags diagnostics via celtics tenure
max(mod2$BUGSoutput$summary[, c("Rhat")])
min(mod2$BUGSoutput$summary[, c("n.eff")])
which.max(mod2$BUGSoutput$summary[, c("Rhat")])


which.min(mod2$BUGSoutput$summary[, c("n.eff")])

PlotTrace("sigma", mod2$BUGSoutput$sims.array)
title(sub = "Model 2 (2003-15) sigma")
PlotTrace("rho", mod2$BUGSoutput$sims.array)
title(sub = "Model 2 (2003-15) rho")

round(mod2$BUGSoutput$summary[c("sigma", "rho"), ], 3)
sigma.cc; rho.cc

# extra - let's try forecasting - damn that worked my first time through
rho.cc.s <- mod2$BUGSoutput$sims.list[["rho"]]
sigma.cc.s <- mod$BUGSoutput$sims.list[["sigma"]]
S <- length(rho.cc.s)
y.sp <- matrix(NA, S, P)
set.seed(1)
y.sp[, 1] <- rnorm(S, rho.cc.s*y.t[nyears], sigma.cc.s)
for(p in 2:P){y.sp[, p] <- rnorm(S, rho.cc.s*y.sp[, p - 1], sigma.cc.s)}
# construct point forecasts and PIs:
y.qp <- apply(y.sp, 2, quantile, c(0.025, 0.5, 0.975))

plot(y.t ~ seq(1, nceltcomp), type = "l", 
     xlim = c(nceltcomp - 10, nceltcomp + P), ylim = c(-30, 30), 
     xlab = "Game # and its trajectory")
lines(y.qp[1,] ~ seq(nceltcomp + 1, nceltcomp + P), type = "l", col = 2, lty = 2)
lines(y.qp[2,] ~ seq(nceltcomp + 1, nceltcomp + P), type = "l", col = 2)
lines(y.qp[3,] ~ seq(nceltcomp + 1, nceltcomp + P), type = "l", col = 2, lty = 2)
# some example trajectories
for (s in c(1,501,1000)){
  lines(y.sp[s,] ~ seq(nceltcomp + 1, nceltcomp + P), type = "l", col = "purple")
}


