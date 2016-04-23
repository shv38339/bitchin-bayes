# data ----
dat   <- read.csv("http://www4.stat.ncsu.edu/~reich/ST590/assignments/Obama2012.csv")
Y     <- 100*dat[,2]
Y     <- (Y-mean(Y))/sd(Y) # centered and scaled
white <- dat[,7]
white <- (white-mean(white))/sd(white) # centered and scaled
unemp <- dat[,18]
unemp <- (unemp-mean(unemp))/sd(unemp) # centered and scaled
n     <- 100

# model ----
model_string <- "model{
# Likelihood
for(i in 1:n){
Y[i]   ~ dnorm(mu[i],inv.var)
mu[i] <- beta[1] + beta[2]*white[i] + beta[3]*unemp[i]
}
# Prior for beta
for(j in 1:3){
beta[j] ~ dnorm(0,0.0001)
}
# Prior for the inverse variance
inv.var   ~ dgamma(0.01, 0.01)
sigma <- 1/sqrt(inv.var)
}"

# compile model in jags ----
model <- jags.model(textConnection(model_string), 
                    data = list(Y = Y, n = n, white = white, unemp = unemp))

# analyze # i have no idea what this shit is... ---- 
# wonder if we can use this on other models instead of all the commands she has
# need to put those in a function anyway
update(model, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta","sigma"), 
                     n.iter=20000, progress.bar="none")

summary(samp)
