x <- g4$diff
x <- ts(x)
plot(x, type = "b")
# install.packages("astsa")
library(astsa)
lag1.plot(x, 1) # we don't see any relationship
acf(x, xlim = c(1, 82))
xlag1 <- lag(x, -1)
y <- cbind(x, xlag1)
ar1fit <- lm(y[, 1] ~ y[, 2])
summary(ar1fit)