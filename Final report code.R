library(splines)
library(dplyr)
library(rjags)
library(R2jags)

# Functions defined
GetSplines <- function( # Get B-splines
    x.i, ##<< Vector of x-values (without NAs) for which splines need to be calculated (determines the number of rows of B.ik)
    x0 = NULL, ##<< x-value which determines knot placement. By default, knot is placed half-interval before last observation
    I = 2.5, ##<< Interval length between two knots during observation period
    degree = 3 # currently tested only with degree 3
) {
    if (is.null(x0)) {
        x0 <- max(x.i)-0.5*I 
    } 
    # get knots, given that one knot needs to be in year0
    knots <- seq(x0-1000*I, x0+1000*I, I) 
    while (min(x.i) < knots[1]) knots <- c(seq(knots[1]-1000*I, knots[1]-I,I), knots)
    while (max(x.i) > knots[length(knots)]) knots <- c(knots, seq(knots[length(knots)]+I, 
                                                                  knots[length(knots)]+1000*I, I)) 
    Btemp.ik <- bs(x.i, knots = knots[-c(1, length(knots))],  degree = degree,
                   Boundary.knots = knots[c(1, length(knots))])
    indicesofcolswithoutzeroes <- which(apply(Btemp.ik, 2, sum) > 0)
    # only remove columns with zeroes at start and end
    startnonzerocol <- indicesofcolswithoutzeroes[1]
    endnonzerocol <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
    B.ik <- Btemp.ik[,startnonzerocol:endnonzerocol]
    colnames(B.ik) <- paste0("spline", seq(1, dim(B.ik)[2]))
    knots.k <- knots[startnonzerocol:endnonzerocol]
    names(knots.k) <- paste0("spline", seq(1, dim(B.ik)[2]))
    ##value<< List of B-splines containing:
    return(list(B.ik = B.ik, ##<< Matrix, each row is one observation, each column is one B-spline.
                knots.k = knots.k ##<< Vector of knots.
    ))
}

#Adding CIs to plot {graphics}
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

########################################################################################################################################################

ult <- read.csv("ult.csv")
doc <- read.csv("doc.csv")
brad <- read.csv("brad.csv")

# Splines model
# Overall
x.i <- ult$Rk
y.i <- ult$diff
n <- length(y.i)

I <- 5 # between-knot length
res <- GetSplines(x.i, I = I)
B.ik <- res$B.ik

K <- dim(B.ik)[2]
parnames <- c("alpha.k", "mu.i", "sigma.y")
jags.data <- list(B.ik = B.ik, y.i = y.i, n=n, K=K)
mod <- jags.parallel(jags.data,
                     parameters.to.save = parnames, 
                     model.file="splines_model_notpenalized.txt")

# obtaining estimates for all x
# use the alphas to evaluate the result at any grid of values
alpha.sk <- mod$BUGSoutput$sims.list[["alpha.k"]]
# define grid, here indexed with .t
xgrid.t <- seq(min(x.i), max(x.i), 1)
ngrid <- length(xgrid.t)
# and use the same splines by keeping I and x0 fixed,
# evaluate splines at grid of xs
res2 <- GetSplines(xgrid.t, I = I, x0 = max(x.i)-0.5*I)
CI.qt <- matrix(NA, 3,ngrid)
for (t in 1:ngrid){  
    CI.qt[,t] <- quantile(res2$B.ik[t,]%*%t(alpha.sk),c(0.025, 0.5, 0.975))
}

set.seed(123)
# Doc Rivers
x.i_doc <- doc$Rk
y.i_doc <-  doc$diff
n_doc <- length(y.i_doc)

res_doc <- GetSplines(x.i_doc, I = I)
B.ik_doc <- res_doc$B.ik

K_doc <- dim(B.ik_doc)[2]
parnames <- c("alpha.k", "mu.i", "sigma.y")
jags.data <- list(B.ik = B.ik_doc, y.i = y.i_doc, n=n_doc, K=K_doc)
mod_doc <- jags.parallel(jags.data,
                     parameters.to.save = parnames, 
                     model.file="splines_model_notpenalized.txt")

# obtaining estimates for all x
# use the alphas to evaluate the result at any grid of values
alpha.sk_doc <- mod_doc$BUGSoutput$sims.list[["alpha.k"]]
# define grid, here indexed with .t
xgrid.t_doc <- seq(min(x.i_doc), max(x.i_doc), 1)
ngrid_doc <- length(xgrid.t_doc)
# and use the same splines by keeping I and x0 fixed,
# evaluate splines at grid of xs
res2_doc <- GetSplines(xgrid.t_doc, I = I, x0 = max(x.i_doc)-0.5*I)
CI.qt_doc <- matrix(NA, 3,ngrid_doc)
for (t in 1:ngrid_doc){  
    CI.qt_doc[,t] <- quantile(res2_doc$B.ik[t,]%*%t(alpha.sk_doc),c(0.025, 0.5, 0.975))
}



# Brad Stevens
x.i_brad <- brad$Rk
y.i_brad <- brad$diff
n_brad <- length(y.i_brad)

res_brad <- GetSplines(x.i_brad, I = I)
B.ik_brad <- res_brad$B.ik

K_brad <- dim(B.ik_brad)[2]
parnames <- c("alpha.k", "mu.i", "sigma.y")
jags.data <- list(B.ik = B.ik, y.i = y.i_brad, n=n_brad, K=K_brad)
mod_brad <- jags.parallel(jags.data,
                         parameters.to.save = parnames, 
                         model.file="splines_model_notpenalized.txt")

# obtaining estimates for all x
# use the alphas to evaluate the result at any grid of values
alpha.sk_brad <- mod_brad$BUGSoutput$sims.list[["alpha.k"]]
# define grid, here indexed with .t
xgrid.t_brad <- seq(min(x.i_brad), max(x.i_brad), 1)
ngrid_brad <- length(xgrid.t_brad)
# and use the same splines by keeping I and x0 fixed,
# evaluate splines at grid of xs
res2_brad <- GetSplines(xgrid.t_brad, I = I, x0 = max(x.i_brad)-0.5*I)
CI.qt_brad <- matrix(NA, 3,ngrid_brad)
for (t in 1:ngrid_brad){  
    CI.qt_brad[,t] <- quantile(res2_brad$B.ik[t,]%*%t(alpha.sk_brad),c(0.025, 0.5, 0.975))
}
y.i <- ult$diff
par(lwd = 1, mfrow = c(1,1))
plot(x.i, y.i, ylab = "Score Differential", xlab = "Ranking of Opponnent at Respective Year", main = "Boston Celtics Season '04-'16")
lines(CI.qt_doc[2,] ~ xgrid.t_doc, col = "red")
lines(CI.qt_brad[2,] ~ xgrid.t_brad, col = "blue")
abline(h=0)

lines(CI.qt[2, ]~xgrid.t, col = "green")
legend("topleft", legend = c("Doc Rivers", "Brad Stevens", "Overall"), col = c("red", "blue", "green"), lty = 1)

AddCIs(CI.qt[1,], CI.qt[3,], seq.years.t = xgrid.t, col = 10)



# splines_model_notpenalized.txt
#model{ 
    # data
#    for (i in 1:n){
#        y.i[i]~dnorm(mu.i[i],tau.y)
#        mu.i[i] <- inprod(B.ik[i,], alpha.k)
#    }
#    tau.y <- pow(sigma.y, -2)
#    sigma.y ~ dunif(0,3)
#    for (k in 1:K){ 
#        alpha.k[k] ~ dnorm(0, 0.01)
#    }
#}

fitted <- CI.qt[2,]
op <- ult2 %>% group_by(Rk) %>% summarize(mean = mean(diff))
error <- op$mean - fitted
plot(error, type = "l")


# Getting residuals for this splines model
fitted_doc <- CI.qt_doc[2,]
## need to get average score differential per opponent
op_doc <- doc %>% group_by(Rk) %>% summarize(mean = mean(diff))
error_doc <- op_doc$mean - fitted_doc
plot(error_doc) # time series

fitted_brad <- CI.qt_brad[2,]
op_brad <- brad %>% group_by(Rk) %>% summarize(mean = mean(diff))
error_brad <- op_brad$mean - fitted_brad
plot(error_brad)
# Fitting AR(1) process to the spline residuals

# Model specification
# residual_t <- 

# Compiling multiple seasons
ult$Date <- as.Date(ult$Date, format = "%a, %b %d, %Y")
ult2 <- arrange(ult, by = Date)

View(ult2)
Differential <- ult2$diff
plot(Differential, type = "l", main = "Score Differential Boston Celtics 2004-2016", ylab = "Score Differential", xlab = "Game #")
acf(Differential)

