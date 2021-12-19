# The Law of Large Numbers states that the sample mean approaches the population mean as
# we increase the sample size. We demonstrate this important law by randomly drawing samples of
# increasing sizes from the normal distribution with a specified mean and standard deviation.
# According to the Law of Large Numbers, as we increase the sample size, we should see the sample
# mean and standard deviation approach the specified mean and standard deviation of the distribution.
#
# We begin by illustrating the law of large numbers with a simple numerical example. Using
# similuation, we start with a sample size of one draw from the normal distribution with a mean
# of 1 and standard deviation of 3. We increase the sample size by an increment of one until we
# reach 1200 draws. For each sample size, we calculate and store the mean and standard deviation.
# Finally, we plot the results.




n.draws <- 1200
means <- rep(NA, n.draws) ## containers for the mean
sds <- rep(NA, n.draws) ## and standard deviation
## Begin by generating n.draws from the normal distribution
## See how the mean and standard deviation changes as
## we inlcude more draws
set.seed(26)
x <- rnorm(n = n.draws, mean = 1, sd = 3)

## simulation
for (i in 1:n.draws) {
  means[i] <- mean(x[1:i]) ## mean of the first i draws
  sds[i] <- sd(x[1:i]) ## standard deviation of the first i draws
}### check! what happens to sds in the first iteration?

# This is more "statistically" correct

# first iteration
means[1] <- x[1] ## mean of the first  draw
sds[1] <- NA ## standard deviation of the first  draw

# now the for loop starts from i = 2
for (i in 2:n.draws) {
  means[i] <- mean(x[1:i]) ## mean of the first i draws
  sds[i] <- sd(x[1:i]) ## standard deviation of the first i draws
}


### an alternative and faster way using cumsum function

means1 <- cumsum(x) / (1:1200)
sum(abs(means1 - means))# really close!!

# btw, cumsum is a function commonly used to mimic a Random Walk, have a look...
browseURL("https://en.wikipedia.org/wiki/Random_walk")

## Plot the results ####
windows()
plot(
  means,
  xlab = "Number of Draws",
  main = "Law of Large Numbers",
  ylim = c(0, 4),
  col = "blue",
  ylab = "Mean and Standard Deviation"
)
abline(h = 1, col = "red", lty = 2)
points(sds, col = "orange")
abline(h = 3, col = "red", lty = 2)

#### Dynamic plot ####
windows()
plot(
  means,
  xlab = "Number of Draws",
  main = "Law of Large Numbers",
  ylim = c(0, 4),
  ylab = "Mean and Standard Deviation",
  col = "white"
)
abline(h = c(1, 3), col = "red", lty = 2)
for (i in seq(3, n.draws, by = 2)) {
  Sys.sleep(0.01)
  lines((i - 2):i, means[(i - 2):i], col = "blue", lwd = 2)
  points((i - 2):i, sds[(i - 2):i], col = "orange", lwd = 2)
}



### trying to speak with R ####

automatic.LLN <- function() {
  n.draws <- as.numeric(readline("Select the number of draws "))
  if (n.draws < 1)
    return("Come on...")
  
  m1 <- as.numeric(readline("Now, choose the mean "))
  v1 <- as.numeric(readline("And the standard deviation "))
  
  if (is.na(m1) |
      is.na(v1) | is.na(n.draws))
    return("should I take it personally?")
  
  seed <- as.numeric(readline("Finally... the seed "))
  if (is.na(seed)) {
    print("ok, I'll choose mine")
    set.seed(42)
  } else{
    set.seed(seed)
  }
  
  means <- sds <- numeric()
  
  x <- rnorm(n = n.draws, mean = m1, sd = v1)
  means[1] <- x[1]
  sds[1] <- NA
  
  for (i in 2:n.draws) {
    means[i] <- mean(x[1:i])
    sds[i] <- sd(x[1:i])
  }
  # select the y-axis limits
  a <- min(sds, means, na.rm = T)
  b <- max(sds, means, na.rm = T)
  
  windows()
  plot(
    means,
    xlab = "Number of Draws",
    main = "Law of Large Numbers",
    ylim = c(a, b),
    ylab = "Mean and Standard Deviation",
    col = "white"
  )
  abline(h = c(m1, v1),
         col = "red",
         lty = 2)
  legend("topright",
         legend = c("Mean", "SD"),
         fill = c("blue", "orange"))
  for (i in seq(3, n.draws, by = 2)) {
    Sys.sleep(0.01)
    lines((i - 2):i, means[(i - 2):i], col = "blue")
    lines((i - 2):i, sds[(i - 2):i], col = "orange")
  }
  return(print("OK!"))
}

# let's give it a try
automatic.LLN()
