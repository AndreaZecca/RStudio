### EXERCISE 1 ###
data <- read.table('bugs.txt', header=T, sep='\t')

obj1 <- data[1:15, ]

tab.pest <- table(data$pest)

pest.C <- data[data$pest=='C',]
pest.A <- data[data$pest=='A',]

count <- data$count

obj2 <- data[data$count>15,]

n.obs <- nrow(data)
n.var <- ncol(data)

mean(count)
median(count)
var(count)
sd(count)
min(count)
max(count)
quantile(count, 0.111)

{
  x11()
  boxplot(data$count~data$pest, xlab='Type of pesticide', ylab='Bugs count', main='Bugs vs pesticide', col='red')
  abline(h=quantile(pest.C$count), col='blue')
  abline(h=mean(count), col='green', lwd=3)
}

aggregate(data$count, list(data$pest), mean)
tapply(data$count, data$pest, var)

alpha <- .05
t.half.alpha <- qt(1-alpha/2, df=n.obs-1)
LL <- mean(count) - t.half.alpha*sqrt(var(count)/n.obs)
UL <- mean(count) + t.half.alpha*sqrt(var(count)/n.obs)
cbind(LL,UL)



## EXERCISE 2 ##
my_function <- function(a, b, n, p){
  pBinom <- pbinom(b,n,p) - pbinom(a-1,n,p)
  pNorm<-pnorm(b,mean=n*p,sd=sqrt(n*p*(1-p)))-pnorm(a,mean=n*p,sd=sqrt(n*p*(1-p)))
  return(list(binomial=pBinom, normal=pNorm))
}
# probability of having from 3 to 6 heads in 10 coin spin
my_function(1,1,4,0.5)



## EXERCISE 3 ##
sample.size <- 50000
d <- 10
z <- rnorm(sample.size, mean=0, sd=1)
x <- rchisq(sample.size, df=d)
t.sim<-z/sqrt(x/d)
{
  x11()
  hist(t.sim,freq=F,breaks=50)
  curve(dt(x,d),from=qt(.001,d),to=qt(.999,d),add=T,lwd=2,col="blue")
}


## EXERCISE 4 ##
sample1 <- rnorm(25, mean=0, sd=1)
sample2 <- rnorm(100, mean=0, sd=1)
sample3 <- rnorm(500, mean=0, sd=1)
{
  x11()
  par(mfrow=c(1,3))
  hist(sample1, freq=F)
  curve(dnorm(x,mean=0,sd=1), add=T)
  hist(sample2, freq=F)
  curve(dnorm(x,mean=0,sd=1), add=T)
  hist(sample3, freq=F)
  curve(dnorm(x,mean=0,sd=1), add=T)
}


## EXERCISE 6 ##
x<-c(169.2,169.5,177.2,175.0,181.3,169.2,165.9,169.9,180.9,173.5,168.2,184.4,169.6,176.8,177.2,180.8,171.9,167.6,177.4,176.0) 
alpha <- 0.01
z.half.alpha <- qt(1-alpha/2, length(x)-1)
xbar <- mean(x)
LL <- xbar - z.half.alpha*sqrt(var(x)/length(x))
UL <- xbar + z.half.alpha*sqrt(var(x)/length(x))
cbind(LL,UL)

var.assum <- 30
alpha.1 <- 0.10
z.half.alpha.1 <- qnorm(1-alpha.1/2)
LL.1 <- xbar - z.half.alpha.1*sqrt(var.assum/length(x))
UL.1 <- xbar + z.half.alpha.1*sqrt(var.assum/length(x))
cbind(LL.1, UL.1)
