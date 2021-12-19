#Working with probability distributions
#Binomial distribution
help(dbinom)

#number of trials
n <- 100

#success probability in each trial
p <- 0.5

#domain of the r.v.
x <- 0:n

#probability distribution
#par(mfrow=c(1,1))
px <- dbinom(x, size = n, prob = p)
cbind(x, round(px, 5)) ### check if 5 digits is enough


#plot the probability distribution
plot(
  x,
  px,
  type = "h",
  lwd = 2,
  main = paste("Binomial distribution - n=", n, "; p=", p, sep = "")
)


#cumulative distribution function cdf
Fx <- pbinom(x, size = n, prob = p)
cbind(x, Fx)

#plot the cdf
plot(
  x,
  Fx,
  type = "s",
  lwd = 2,
  main = paste("Binomial distribution - n=", n, "; p=", p, sep = "")
)

#plotting both px and Fx
par(mfrow = c(1, 2)) ### multiple plots in one graph
plot(
  x,
  px,
  type = "h",
  lwd = 2,
  main = paste("Binomial distribution - n=", n, "; p=", p, sep = "")
)
plot(
  x,
  Fx,
  type = "s",
  lwd = 2,
  main = paste("Binomial distribution - n=", n, "; p=", p, sep = "")
)

#computing a quantile
qq <- 0.5

#the following allows to compute a vector of quantiles
qq <- seq(.25, .75, length = 3)

qbinom(qq, size = n, prob = p)

#draw a random number from the binomial distribution
rbinom(1, n, p)
n.draws <- 20000
sample.bin <- rbinom(n.draws, n, p)

rbinom(1, n, p)
#comparing the mean computed by simulaition
#and the theoretical mean
mean(sample.bin)
n * p

#the same comparison, but this time with variances
var(sample.bin)
n * p * (1 - p)

#P(X=0) by simulation
count.0 <- sample.bin == 0
sum(count.0) / n.draws
dbinom(0, n, p)

#Now the whole prob. distribution
px.sim <- rep(NA, n + 1)

for (i in 1:(n + 1)) {
  count <- sample.bin == x[i]
  px.sim[i] <- sum(count) / n.draws
}
cbind(x, px, px.sim)

#Is this a good simulation approximation?
sum(abs(px - px.sim)) #not bad!

Fx.sim <- cumsum(px.sim)
sum(abs(Fx - Fx.sim))

#Let's check it graphically
windows()
par(mfrow = c(1, 2))
plot(x, px, main = "px", ylab = "")
points(x, px.sim, col = "red", pch = 3)
plot(x, Fx, main = "Fx", ylab = "")
points(x, Fx.sim, col = "red", pch = 3)
legend(60, 0.7,legend = c("True", "Sim"),  pch = c(1,3), col = c("black", "red"))

#Comparison with a Normal Distribution/approximation
plot(x, px, type = "h", col = "blue")
lines(x,
      px.sim,
      type = "h",
      lwd = 2,
      col = "red")
curve(
  dnorm(x, mean = n * p, sd = sqrt(n * p * (1 - p))),
  from = 0,
  to = 100,
  add = TRUE,
  col = "green"
)

# exercise #
#Poisson distribution
lambda <- 4
x <- 0:qpois(.9999999, lambda) ### find the probable support of a poisson distr with lambda = 4

#probability distribution and plot
#cumulative distribution function cdf and plot
#approximation via simulation