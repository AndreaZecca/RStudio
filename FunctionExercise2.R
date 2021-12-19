#### Exercise 1 #############################################
# a function that changes the case
# of a single letter (from upper to lower case and viceversa)
# version 1
LowerUPPER <- function(let) {
  indLower <- letters == let
  indUpper <- LETTERS == let
  if (any(indLower))
    return(LETTERS[indLower])
  else {
    if (any(indUpper))
      return(letters[indUpper])
    else
      print("let should be a single letter")
  }
}

## try

LowerUPPER("P")
LowerUPPER(c("v", "a")) # does not work with more than letter
LowerUPPER(3) # the same for numeric input


# version 2
LowerUPPER <- function(let) {
  o <- order(let)
  indLower <- letters %in% let
  indUpper <- LETTERS %in% let
  if (any(indLower))
    return(LETTERS[indLower][o])
  else {
    if (any(indUpper))
      return(letters[indUpper][o])
    else
      print("let should be a single letter")
  }
}

LowerUPPER(let = c("v", "a"))

# see also  ?toupper, ?tolower

#### Exercise 2 #############################################
# a function to transform a numerical mark into a letter grade
# first version uses nested if...else
grade <- function(mark) {
  if (!is.numeric(mark))
    return("mark should be a number")
  lettgrad <- NA
  if (mark < 0)
    return("mark should be non negative")
  else {
    if (mark < 10)
      lettgrad <- "N"
    else {
      if (mark < 25)
        lettgrad <- "G"
      else {
        if (mark < 40)
          lettgrad <- "F"
        else {
          if (mark < 45)
            lettgrad <- "E"
          else {
            if (mark < 55)
              lettgrad <- "D"
            else {
              if (mark < 65)
                lettgrad <- "C"
              else {
                if (mark < 75)
                  lettgrad <- "B"
                else {
                  if (mark <= 100)
                    lettgrad <- "A"
                  else
                    return("mark should be no larger than 100")
                }
              }
            }
          }
        }
      }
    }
  }
  return(lettgrad)
}

grade(10)

# a version using switch
grade <- function(mark) {
  if (!is.numeric(mark))
    return("mark should be a number")
  else if (!is.integer(mark))
    mark <- as.integer(round(mark))
  cmark <- cut(
    mark,
    breaks = c(-Inf,-1, 9, 24, 39, 44, 54, 64,
               74, 100, Inf), labels = F
  )
  lettgrad <- NA
  switch(
    cmark,
    # transform in numeric expression
    return("mark should be non negative"),
    lettgrad <- "N",
    lettgrad <- "G",
    lettgrad <- "F",
    lettgrad <- "E",
    lettgrad <- "D",
    lettgrad <- "C",
    lettgrad <- "B",
    lettgrad <- "A",
    return("mark should be no larger than 100")
  )
  return(lettgrad)
}


grade(41)

#### Exercise 3 #############################################
# generate several samples from
# a normal distribution,
# store the max of each sample
# make a hist and summaries
#   use a while() loop
distmaxnor.while <- function(r, n, mu, sigma)
{
  # mu, sigma: mean and s.d. of a Normal distribution
  # n : sample size
  # r : simulation sample size
  # simulates r samples of size n from a N(mu, sigma^2)
  # displays a histogram and summary statistics of the
  #    distribution of sample maxima
  #
  maxvec <- rep(0,r)
  i <- 1
  while (i <= r) {
    samp <- rnorm(n, mu, sigma)
    maxvec[i] <- max(samp)
    i <- i + 1
  }
  windows()
  hist(maxvec, xlab="Maxima", main = "")# use strsplit to split title in multiples lines
  title(main = strsplit(paste("Histogram of maximum from an ", n,
                              "-sample \n from a ",
                              "N(", mu,",",sigma,"^2) \n distribution",sep=""), "\n")[[1]])
  print(summary(maxvec))
  return(invisible(maxvec))
}

df <- distmaxnor.while(400,100,30,10)

#### Exercise 4 #############################################
####  Newton's method ####

### use either while or repeat
NewtMeth0 <- function(f, xinit, f1, eps = 1e-7)
{
  # uses Newton's method to find a root of the equation
  # f(x) = 0
  # f: a function whose first argument is x
  # xinit: initial value of x
  # f1: a function containing the first derivative of f
  #
  x <- xinit
  repeat {
    Delta <- f(x) / f1(x)
    x <- x - Delta
    if (abs(Delta) < eps)
      break
  }
  return(list(x = x, xinit = xinit))
}

# we could also have used a while loop here,
# replacing the repeat{ } block with


while (abs(Delta) >= eps) {
  Delta <- f(x) / f1(x)
  x <- x - Delta
}

# could have we used a for loop ?

# stopping criterion could be made more sophisticated than
# abs(Delta) < eps

# the above version of NewtMeth0() works for functions
# f() and f1() that only have x as argument
# for instance

### define the function 
f <- function(x) {
  x ^ 2 - 3 * log(x + 1)
}
### and its derivative
f1 <- function(x) {
  2 * x - 3 / (x + 1)
}
### plot a grid of values
windows()
par(mfrow = c(2, 1))
xgrid <- seq(-0.1, 2, length = 100)
plot(xgrid, f(xgrid), type = "l")
abline(0, 0, col = "red") # equivalently abline(h = 0, col = "red)
plot(xgrid, f1(xgrid), type = "l")
# apply the function
s1 <- NewtMeth0(f, xinit = 0.5, f1, eps = 1e-7)$'x'
s2 <- NewtMeth0(f, xinit = 1, f1, eps = 1e-7)$'x'
cbind(s1, s2)

## check the solutions
xgrid <- seq(-0.1, 2, length = 100)
plot(xgrid, f(xgrid), type = "l")
abline(0, 0, col = "red")
abline(v = c(s1, s2), col = c('blue', "green"))

# what if you wanted to use, say
# f <- function(x, a, b) {x^a + b*log(x+1)} or
# f <- function(x, a, b, d) {x^a + b*log(x+1) + d} ?

# There is a way of passing to an R function a
# variable number of arguments:
# one uses the special argument ... (three dots)

NewtMeth1 <- function(f, xinit, f1, eps = 1e-7, ...)
{
  # uses Newton's method to find a root of the equation
  # f(x) = 0
  # f: an R function whose first argument is x
  # xinit: initial value of x
  # f1: R function containing the first derivative of f
  #
  x <- xinit
  repeat {
    Delta <- f(x, ...) / f1(x, ...)
    x <- x - Delta
    if (abs(Delta) < eps)
      break
  }
  return(list(x = x, xinit = xinit))
}

# Then we can deal with the case a=2, b=-3, d=0 as follows:
# again specify the function
f <- function(x, a, b, d) {
  x ^ a + b * log(x + 1) + d
}
# and its derivative
f1 <- function(x, a, b, d) {
  a * x ^ (a - 1) + b / (x + 1)
}

windows()
xgrid <- seq(-0.1, 2, length = 100)
plot(
  xgrid,
  f(
    x = xgrid,
    a = 2,
    b = -3,
    d = 1
  ),
  type = "l",
  ylab = "f(x)",
  xlab = "x"
)
abline(0, 0, col = "red")
s1b <- NewtMeth1(
  f,
  xinit = 0.5,
  f1,
  eps = 1e-7,
  a = 2,
  b = -3,
  d = 1
)[1]
s2b <- NewtMeth1(
  f,
  xinit = 1,
  f1,
  eps = 1e-7,
  a = 2,
  b = -3,
  d = 1
)[1]
cbind(s1b, s2b)
abline(v = c(s1b, s2b), col = c('blue', "green"))

# May want to allow the user to provide only the function,
# not its first derivative, this will then be computed
# numerically.
# Note: symbolic differentiation can be done using deriv()
# try dx <- deriv(~ x^2, "x") ; dx
# Stop the computation after n iteration: not converged!

# optional argument
NewtMeth <- function(f,
                     xinit,
                     f1 = NULL,
                     eps = 1e-7,
                     n = 100,
                     ...)
{
  # uses Newton's method to find a root of the equation
  # f(x) = 0
  # f: a function whose first argument is x
  # xinit: initial value of x
  # f1: a function containing the first derivative of f (optional)
  # stop the algorithm after n iteration if not converged
  
  x <- xinit
  i = 0
  repeat {
    if (!is.null(f1))
      Delta <- f(x, ...) / f1(x, ...)
    else {
      del <- 1e-7
      fx <- f(x, ...)
      f1x <- (f(x + del, ...) - fx) / del
      Delta <- fx / f1x
    }
    x <- x - Delta
    i = i + 1
    
    if (i <= n & abs(Delta) < eps)
      break
    
    if (i > n & abs(Delta) > eps) {
      print("not converged")
      break
    } 
  }
  
  return(list(x = x, xinit = xinit, iter = i))
}

s1c <- NewtMeth(
  f,
  xinit = 0.5,
  f1 = NULL,
  eps = 1e-7,
  a = 2,
  b = -3,
  d = 1
)[1]
s2c <- NewtMeth1(
  f,
  xinit = 1,
  f1 = NULL,
  eps = 1e-7,
  a = 2,
  b = -3,
  d = 1
)[1]
cbind(s1c, s2c)


########################################################################################################
# This example WILL NOT be part of the exam, but I hope it will be useful in your future! Have a look. #
########################################################################################################

# Finally let's use NewtMeth() to compute the MLE in a statistical model
#
# "Data were collected on the numbers of larvae found on 20 randomly
#  sampled flowering bracts of Heliconia plants.
#  The observed numbers were:

#        9 14 3 3 8 7 7 6 7 0 6 0 5 1 3 12 2 4 0 11

#  One possible model for the number of larvae is that they are
#  observations of independent random variables with probability
#  function
#      f(y_i) = theta^{y_i} (1 - theta)       0 < theta < 1
#                                             y_i >= 0, integers
#
# Then
#
# Likelihood:        L(theta) = theta^{ sum(y_i) }  (1-theta)^{n}
#
# log-likelihood:    l(theta) = sum(y_i) log(theta) + n log(1-theta)
#
#
# 1st der log-lik:   l1(theta) = sum(y_i)/theta - n/(1-theta)
#
# btw, for this simple model a closed form solution for the MLE is available
#
# setting l1(theta) = 0, we get theta.hat = sum(y_i) / (sum(y_i) + n)
#
# 2nd der log-lik:   l2(theta) = -sum(y_i)/theta^2 - n/(1-theta)^2
#
# To maximise the log-likelihood we look for a root of its first
# derivative (then we'll need to check it is actually a maximizer!)
# So l1 and l2 play the role of f and f1 in NewtMeth()

l <- function(theta, y)
{
  return(sum(y) * log(theta) + length(y) * log(1 - theta))
}

l1 <- function(theta, y)
{
  n <- length(y)
  return(sum(y) / theta - n / (1 - theta))
}

l2 <- function(theta, y)
{
  n <- length(y)
  return(-sum(y) / theta ^ 2 - n / (1 - theta) ^ 2)
}

### data
y <- c(9, 14, 3, 3, 8, 7, 7, 6, 7, 0, 6, 0, 5, 1, 3, 12, 2, 4, 0, 11)

# set of possible values for theta
theta <- seq(0.01, 0.99, by = 0.01)
windows()
plot(theta, l(theta, y), type = "l",xlab = expression(theta),
     ylab = expression(l(theta)), main = 
       expression(paste("log-likelihood ", l(theta))))

# more sophisticated label using "plotmath", type "frac" in the help tab
plot(theta, l(theta, y), type = "l",xlab = expression(theta),
     ylab = expression(l(theta)))
# work with the title
title(main = expression(paste(l(theta)," of ",f(y[i]) == 
                                theta^{y[i]}*(1-theta) )))

# which value of theta maximizes the log-likelihood?
theta[which.max(l(theta,y))]
abline(v = theta[which.max(l(theta,y))], col = "red", lty = 2)


# let's try with different starting points
NewtMeth(
  f = l1,
  xinit = 0.5,
  f1 = l2,
  eps = 1e-7,
  y = y
)
NewtMeth(
  f = l1,
  xinit = 0.01,
  f1 = l2,
  eps = 1e-7,
  y = y
)
NewtMeth(f = l1,
         xinit = 0.99,
         eps = 1e-7,
         y = y) #without f1


#### And with a Poisson r.v.?
#  
#      f(y_i) = theta^{y_i} exp{-theta} / y_i!      0 < theta
#                                                   y_i >= 0, integers
#
# Then
#
# Likelihood:        L(theta) = theta^{ sum(y_i) }  exp{- n*theta} / prod(y_i!)
#
# log-likelihood:    l(theta) = sum(y_i) log(theta) - n theta - sum(log(y_i)!)
#
#
# 1st der log-lik:   l1(theta) = sum(y_i)/theta - n
#
# btw, for this simple model a closed form solution for the MLE is available
#
# setting l1(theta) = 0, we get theta.hat = sum(y_i) / n
#
# 2nd der log-lik:   l2(theta) = -sum(y_i)/theta^2

### poisson r.v. ####
lp <- function(theta, y)
{
  n <- length(y)
  return(-sum(log(factorial(y))) + sum(y) * log(theta) - n * theta)
}

lp1 <- function(theta, y) {
  n <- length(y)
  return(sum(y) / theta - n)
}

lp2 <- function(theta, y) {
  return(-sum(y) / theta ^ 2)
}

# alternative and fast way to compute log-likelihood for Poisson r.v.
lg_poisson <- function(theta, y){
  return(sum(dpois(y, theta, log=TRUE)))
}

# grid of theta values, must use sapply to provide a vector of theta as imput
thetas <- seq(1,12, by=0.1)
lg_poisson(thetas, y) # wrong!!!!

# two equivalent ways
ll <- sapply(thetas,function(x){lg_poisson(x,y)})
ll1 <- sapply(thetas,lg_poisson, y = y)

# value of theta maximizing the log-likelihood
thetas[which.max(ll1)]

plot(thetas, ll1, type="p",xlab = expression(theta),
     ylab = expression(l(theta)), pch = 5, cex = 0.5) # use of pch and cex symbols size
title(main = expression(paste(l(theta), " of Poisson dist. ",
                              f(y[i])== frac(theta^y[i]*e^-theta,y[i]*"!")) ) )
abline(v = thetas[which.max(ll1)], col = "red", lty = 3)


# let us try our NewMeth
NewtMeth(
  f = lp1,
  xinit = 7,
  f1 = lp2,
  y = y
)

# what if we provide a "bad" initial guess? say 17

NewtMeth(
  f = lp1,
  xinit = 17,
  f1 = lp2,
  y = y
)

# the hope is not lost, let uus rely on much more sophisticated optimizers:
# there are a bunch of available optimizers in R, try them!
optimize(f = lp, interval = c(0,150) ,y = y, maximum = T) # it works!



