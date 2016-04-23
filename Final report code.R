library(splines)
library(dplyr)
library(rjags)
library(R2jags)

g1 <- read.csv("g1.csv")
g2 <- filter(g1, team == "Boston Celtics")
boston.ids <- g2$game.id
g3 <- g1[g1$game.id %in% boston.ids, ]
g4 <- filter(g3, team != "Boston Celtics")

# Basic model
mdl <- lm(diff~def.rank, data = g4)
summary(mdl) 

# Splines model
x.i <- g4$def.rank
y.i <-  g4$diff
n <- length(y.i)

# Function defined
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
I <- 2.5 # between-knot length
res <- GetSplines(x.i, I = I)
B.ik <- res$B.ik

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

par(lwd = 1.3, mfrow = c(1,1))
plot(x.i, y.i, ylab = "Score Differential", xlab = "Denfensive Ranking of Opponnent", main = "Boston Celtics Season '14-15")
#points(x.i, m_mean, col = 2, lwd = 1)

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

lines(CI.qt[2,] ~ xgrid.t, col = 2)
AddCIs(CI.qt[1,], CI.qt[3,], seq.years.t = xgrid.t, col = 10)
abline(v = 1:30, lty = 1)
lines(opp$mean)
# Getting residuals for this splines model
fitted <- CI.qt[2,]
## need to get average score differential per opponent
opp <- g4 %>% group_by(def.rank) %>% summarize(mean = mean(diff))

error <- opp$mean - fitted
plot(error, type = "l") # time series

# Fitting AR(1) process to the spline residuals




