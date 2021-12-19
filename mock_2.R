## EXCERCISE 1 ##
data <- read.table('salmons.txt', header=T, sep='\t')

tab.salmon <- table(data$river)

n <- nrow(data)
p <- ncol(data)

first <- data[1:20, ]

first1 <- data[1:20,1]

weight <- data$weight

river.A <- data[data$river == 'A', ]

mean(weight)
median(weight)
var(weight)
sd(weight)
quantile(weight, c(.1, .9))
min(weight)
max(weight)

{
  x11()
  hist(weight, xlab='weight', ylab='Frequency-Salmon weight', main='Hist of salmon weight', col='blue', breaks=50)
  abline(v=c(mean(weight),median(weight)), col=c('red','green'), lwd=3)
  abline(v=c(LL,UL), col=c('yellow','yellow'), lwd=3)
}

tapply(weight, data$river, mean)

alpha <- 0.01
z.half.alpha <- qnorm(1-alpha/2)
xbar <- mean(weight)
LL <- xbar - z.half.alpha * sqrt(var(weight)/n)
UL <- xbar + z.half.alpha * sqrt(var(weight)/n)
cbind(LL,UL)

expectedMean <- 160
actualMean <- mean(weight)
significanceLevel <- 0.05
test.statistic <- (actualMean - expectedMean)/(sd(weight)/sqrt(n))
pValue <- pnorm(zScore) #rejected 
zScore < qnorm(significanceLevel) # rejected
require(ggplot2)
{
  x11()
  ggplot() + xlim(c(-5,5))+ stat_function(fun = dnorm)+
    geom_vline(xintercept = c(test.statistic, -z.half.alpha, z.half.alpha),
               color = c("blue", rep("red",2)))+
    labs(x = "Z Scores", y ="")+
    stat_function(fun = dnorm, geom = "area",
                  xlim = c(-z.half.alpha, z.half.alpha),
                  fill = "lightblue")+
    annotate(geom = "text", x = 0, y = 0.1,
             label = "Non-rejection area", color = "red")+
    annotate(geom = "text", x = -4, y = 0.2,
             label = "Observed test \n Statistics", color = "blue",angle = 90)
}

my_function <- function(x, conf.level, mu0, signif.level){
  actualMean <- mean(x)
  zScore <- (actualMean - mu0) / (sd(x)/sqrt(length(x)))
  rejected <- zScore < qnorm(signif.level)
  pValue <- pnorm(zScore)
  
  alpha <- 1-conf.level
  z.half.alpha <- qnorm(1-alpha/2)
  LL <- mean(x) - z.half.alpha/(var(x)/length(x))
  UL <- mean(x) + z.half.alpha/(var(x)/length(x))
  conf.interval <- c(LL,UL)
  
  return(list(confidence.interval=conf.interval, decision=ifelse(rejected,"Null Hyp rejected", "Null Hyp NOT rejected"), p.value=pValue))
}

my_function(weight, .95, 160, 0.05)

## EXCERCISE 2 ##
n.draws <- 30000
df  <- 10
samp <- rt(n.draws, df)
t.mean <- 0
t.var <- df/(df-2)
{
  x11()
  hist(samp, freq=F, breaks=50)
  curve(dnorm(x, mean=t.mean,sd=sqrt(t.var)), add=T, from=-6, to=6, col='red')
}




