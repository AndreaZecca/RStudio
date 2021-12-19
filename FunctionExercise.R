# an initial example
# from fahrenheit to celsius
fahrenheit_vs_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

# f vs c: a converter
fahrenheit_vs_celsius <- function(temp, f_to_c = TRUE) {
  if (f_to_c) {
    temp_new <- (temp - 32) * 5 / 9
  } else{
    temp_new <- temp * 9 / 5 + 32
  }
  return(temp_new)
}


# the same for celsius and kelvin
celsius_vs_kelvin <- function(temp, c_to_k = TRUE) {
  if (c_to_k) {
    temp_new <- temp + 273.15
  } else{
    temp_new <- temp - 273.15
  }
  return(temp_new)
}


# combining the two converters
fahrenheit_vs_kelvin <- function(temp) {
  temp1 <- fahrenheit_vs_celsius(temp)
  temp2 <- celsius_vs_kelvin(temp1)
  return(temp2)
}


### Create a new Script file and save it as LastName.R or GroupName.R. 
### You have 30 minutes, once you have completed the excercise, please send
### the script file to marco.novelli4@unibo.it 


# Exercise 1
# Create a function that given an integer will calculate 
# how many divisors it has (other than 1 and itself). 
# Make the divisors appear by screen.


# Exercise 2
# The following function calculates the mean and standard deviation 
# of a numeric vector.
meanANDsd <- function(x) {
  av <- mean(x)
  sdev <- sd(x)
  return(c(mean = av, sd = sdev)) # The function returns this vector
}
# Modify the function so that: 
# (a) the default is to use rnorm() to generate 20 random normal numbers, 
# and return the standard deviation; 
# (b) if there are missing values, 
# the mean and standard deviation are calculated for the remaining values.


# Exercise 3
# With a user-written function, reproduce the Fibonacci sequence. 
# The latter has many mathematical relationships and 
# has been discovered repeatedly in nature. 
# The sequence is constructed considering the sum of the previous 
# two values, initialized with the values 1 and 1.
# 1,1,2,3,5,8,13,21,34,55,89,144,...
# input: an integer indicating the elements of the sequence to be calculated


# Exercise 4
# write a function that, taking as imput a data.frame, 
# calculate mean and variance for each variable and
# collect the results in a list
# imput: a data-frame

data.1 <- data.frame(matrix(rnorm(1000000), ncol = 100))

# Exercise 5
# write a function that, taking as imput a vectors of positive numbers,
# calculate the "population" coefficient of variation of the differences from the highest 
# value, namely from (2,5,1,9,4) --> the differences will be given by
# 9-2, 9-5, 9-1, etc...
# pay attention on how R calculates the standard deviation!

# Exercise 6
# compute exponential series
# sum_{n=0,1,...} (x^n)/n!
# up to a prespecified tolerance
# check if it converges to exp(x)

################## solutions ####################
# Exercise 1 ####
f.div <- function(n) {
  i <- 2
  counter <- 0
  while (i <= n / 2) {
    if (n %% i == 0) {
      counter <- counter + 1
      cat (i , sep = "\n")
      #print(i)
    }
    i <- i + 1
  }
  return(counter)
}

# shorter version
f.div1b <- function(n){
  cat(" Div = ", (2:(n/2))[n %% (2:(n/2)) == 0],
      "\n Num Div", length((2:(n/2))[n %% (2:(n/2)) == 0]))
}

# Exercise 2 ####

# (a)
meanANDsd.a <- function(x = rnorm(20), return.mean = F) {
  if (return.mean == F) {
    return(sd(x))
  } else{
    av <- mean(x)
    sdev <- sd(x)
    return(c(mean = av, sd = sdev))
  }
  
}

# (b)

meanANDsd.b <- function(x = rnorm(20), return.mean = F) {
  if (return.mean == F) {
    return(sd(x, na.rm = T))
  } else{
    av <- mean(x, na.rm = T)
    sdev <- sd(x, na.rm = T)
    return(c(mean = av, sd = sdev))
  }
  
}


# Exercise 3 ####

fibo <- function(n) {
  #box <- numeric(n) # alternative "empty box" constructor
  box <- rep(NA, n)
  box[1:2] <- 1
  for (i in 3:n) {
    box[i] <- box[i - 1] + box[i - 2]
  }
  return(box)
}


# alternative solution
fibo2 <- function(n) {
  if (n < 3) {
    return("Do not be silly! n must be an integer greater than or equal 2.")
  } else{
    vals <- numeric(n)
    vals[1:2] <- 1
    for (i in 3:n) {
      vals[i] <- vals[i - 1] + vals[i - 2]
    }
    return(vals)
  }
}

# using recursive function...
# (check, is this version faster than the previous two?) 
fibo3 <- function(n) {
  if (n == 0)
    return(0)
  if (n == 1)
    return(1)
  return (fibo3(n - 1) + fibo3(n - 2))
}

# using a while() loop and with initial element "0"
fibo4 <- function(n) {
  if (n == 0 || n == 1)
    return(n)
  vals <- numeric(n)
  vals[1:2] <- 1
  i <- 2
  while (i < n) {
    i <- i + 1
    vals[i] <- vals[i - 1] + vals[i - 2]
  }
  return(vals)
}

# which version is the fastest?
n <- 3
system.time(fibo(n))
system.time(fibo2(n))
system.time(fibo3(n))
system.time(fibo4(n))


# Exercise 4 ####
## create a toy data-set
data.f <- data.frame(matrix(rnorm(10000), ncol = 100))
# with toy labels...
rownames(data.f) <- paste(letters, 1:nrow(data.f), sep = "-")
colnames(data.f) <- paste("var X", 1:ncol(data.f), sep = "")


stat <- function(x) {
  results <- list()
  dim <- ncol(x)
  for (i in 1:dim) {
    results[[i]] <- c(mean_v = mean(x[, i]) , var_v = var(x[, i]))
  }
  return(results)
}


### faster and easier way using apply
stat2 <- function(x) {
  v1 <- apply(x, 2, mean)
  v2 <- apply(x, 2, var)
  results <- list(v1, v2)
  return(results)
}


## let's check if it's true!
data.f1 <- as.data.frame(matrix(rnorm(5e5), ncol = 1e5))
system.time(stat(data.f1))
system.time(stat2(data.f1))

# Exercise 5 ####

cv <- function(x) {
  n <- length(x)
  xc <- sort(x, decreasing = T)
  mv <- rep(xc[1], length(x))
  xx <- mv - xc # xx <- max(x) - x
  vv <- sqrt(var(xx) * ((n - 1) / n)) / mean(xx)
  return(vv)
}



# Exercise 6 ####

expseries <- function(x, tol = 1e-10) {
  n <- 0
  inc <- 1 # increment
  ser <- inc # sum of values
  while (abs(inc) > tol) {
    n <- n + 1
    inc <- x ^ n / gamma(n + 1) # see use of gamma for factorial, n!
    ser <- ser + inc
  }
  return(ser)
}


expseriesl <- function(x, tol = 1e-10) {
  n <- 0
  inc <- 1 # increment
  ser <- inc # sum of values
  while (abs(inc) > tol) {
    n <- n + 1
    inc <- exp(n*log(x) - lgamma(n + 1)) # see use of gamma for factorial, n!
    ser <- ser + inc
  }
  return(ser)
}

expseriesl(164)
exp(164)