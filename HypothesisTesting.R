####################################################################
#EXERCISE 1
####################################################################

#the manufacturer claims that the mean lifetime of a light
#bulb is not less than 10,000 hours, i.e. H0:mu >= mu0 vs H1:mu < mu0,
# i.e. a one-sided (left tailed) test

mu0 <- 10000

#sample size
n <- 30

#sample mean
xbar <- 9900

#The population standard deviation is assumed to be known
sigma <- 120

#significance level
alpha <- 0.05

#Under the Hypothesis of Gaussianity of the lifetime of a light
#bulb, since the variance is known:
test.statistic <- (xbar - mu0) / (sigma / sqrt(n)) #follows a Gaussian distribution under H0
z.alpha <- qnorm(alpha)	#since the rejection region is left-sided, btw, you can use (- qnorm(1-alpha)) check!

#The rejection region is the set of samples such that test.statistic<z.alpha
#since
test.statistic < z.alpha  #is TRUE

#The null Hp is rejected

#Computing the p-value
pnorm(test.statistic) #is less than alpha

####################################################################
#EXERCISE 1.1
####################################################################
#The difference with respect to EXERCISE 1 is that the population variance is
#Assumed to be unknown, Under the Hypothesis of Gaussianity of the lifetime of a light
#bulb we can use a one-sided t-test

mu0 <- 10000

#sample size
n <- 30

#sample mean
xbar <- 9900

#Estimate of the population standard deviation
s.tilde <- 125

#significance level
alpha <- 0.05

#Under the Hypothesis of Gaussianity of the lifetime of a light
#bulb, since the variance is unknown:
test.statistic <- (xbar - mu0) / (s.tilde / sqrt(n)) #follows a Student's t distribution under H0
t.alpha <- -qt(1 - alpha, n - 1)	#since the rejection region is left-sided
# again -qt(1-alpha,n-1) == qt(alpha, n-1)

#The rejection region is the set of samples such that test.statistic<t.alpha
#since
test.statistic < t.alpha  #is TRUE
#The null is rejected

#Computing the p-value
pt(test.statistic, n - 1) #is less than alpha

####################################################################
#EXERCISE 2
####################################################################
#Claim on the food label: there are at most 2 grams of saturated fat
#in a single cookie, i.e. H0:mu <= mu0 vs H1:mu > mu0,
#i.e. a one-sided (right tailed) test

mu0 <- 2

#sample size
n <- 35

#sample mean
xbar <- 2.1

#The population standard deviation is assumed to be known
sigma <- 0.25

#significance level
alpha <- 0.05

#Under the Hypothesis of Gaussianity amount of saturated fat
#per cookie, since the population standard deviation is known:

test.statistic <- (xbar - mu0) / (sigma / sqrt(n)) #follows a Gaussian distribution under H0
z.alpha <- qnorm(1 - alpha) #since the rejection region is right-sided

#The rejection region is the set of samples such that test.statistic>z.alpha
#since
test.statistic > z.alpha  #is TRUE

#The null Hp is rejected

# p-value, 3 equivalent methods
1 - pnorm(test.statistic)
pnorm(-test.statistic)
pnorm(test.statistic, lower.tail = FALSE)

##################################################################
### what if H0:mu = mu0 vs H1:mu != mu0, i.e. two-sided test? ####
##################################################################

# the test statistic does not change:
test.statistic <- (xbar - mu0) / (sigma / sqrt(n))

# but z.alpha DO change!
z.alpha <- qnorm(1 - alpha / 2)

#The rejection region is the set of samples such that abs(test.statistic)>z.alpha
#since
abs(test.statistic) > z.alpha  #is TRUE

#The null Hp is again rejected

# and the p - value?
2 * pnorm(abs(test.statistic), lower.tail = FALSE)






















