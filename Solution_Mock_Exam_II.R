#1)
data<-read.table("salmons.txt",header=T,sep="\t")

#2)
tab.salmon<-table(data$river)

#3)
n<-nrow(data)
p<-ncol(data)

#4)
first<-data[1:20,]

#5)
first1<-data[1:20,1]

#6)
weight<-data$weight

#7)
river.A<-data[data$river=="A",]

#8)
mean(weight)
median(weight)
var(weight)
sd(weight)
quantile(weight,c(.1,.6))
min(weight)
max(weight)

#9)
hist(weight,xlab="Weight",ylab="Frequency - Salmon weight",main="Histogram of the salmon weight",col="blue")
abline(v=mean(weight),col="red",lwd=3)
abline(v=median(weight),col="green",lwd=3)

#OR, equivalently
abline(v=c(mean(weight),median(weight)),col=c("red","green"),lwd=c(3,3))

# with gglot2

df <- data.frame(weight = weight)

require(ggplot2)

ggplot(data = df, aes(x = weight))+
  geom_histogram(bins = 50, color = "black", fill = "Blue")+
  labs(x = "Weight", y = "Frequency - Salmon weight")+
  ggtitle("Histogram of the salmon weight")+
  geom_vline(xintercept = c(mean(weight), median(weight)),
              color = c("red", "green"), size = 1.2)


#10)
tapply(weight,data$river,mean)

#OR, equivalently
aggregate(weight,list(data$river),mean)


#11)
alpha<-0.01
z.half.alpha<-qnorm(1-alpha/2)
LL<-mean(weight)-z.half.alpha/sqrt(var(weight)/n)
UL<-mean(weight)+z.half.alpha/sqrt(var(weight)/n)
LL;UL

#12)
alpha<-0.01
z.half.alpha<-qnorm(1-alpha/2)
mu0<-160
test.statistic<-(mean(weight)-mu0)/sqrt(var(weight)/n)

abs(test.statistic);z.half.alpha

#It is a two-sided test, thus the null hypothesis is rejected
#if
abs(test.statistic)>z.half.alpha

# p-value

2*pnorm(-abs(test.statistic))

# let us see this graphically

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

#13)
my.function<-function(x,conf.level,mu0,signif.level){

#sample data
n<-length(x)
xbar<-mean(x)
s2<-var(x)

#Computing the confidence interval
alpha.CI<-1-conf.level
z.half.alpha.CI<-qnorm(1-alpha.CI/2)
LL<-xbar-z.half.alpha.CI*sqrt(s2/n)
UL<-xbar+z.half.alpha.CI*sqrt(s2/n)

#Computing the test statistic
z.half.alpha.TEST<-qnorm(1-signif.level/2)
test.statistic<-(mean(x)-mu0)/sqrt(s2/n)

#Is the test statistic in the critical region?
reject<-ifelse(abs(test.statistic)>z.half.alpha.TEST,"The null hypothesis is rejected","The null hypothesis is NOT rejected")

#computing the p-value
p.value<-2*(1-pnorm(abs(test.statistic)))

output<-list(Confidence.Interval=c(LL,UL),Decision=reject,P.value=p.value)
return(output)

}


#14)
my.function(weight,.95,150,.05)
my.function(weight,.99,155,.05)

######################################
## EXERCISE 2
######################################
n.draws<-30000
d<-10
samp<-rt(n.draws,df=d)

hist(samp,freq=F,breaks=30)
curve(dnorm(x,0,sqrt(d/(d-2))),from=-6,to=6,add=T, col = "red")


# with ggplot 2
require(ggplot2)

df1 <- data.frame(val = samp)
ggplot(data = df1, aes(x = val))+
  geom_histogram(bins = 30, aes(y = ..density..), color = "black")+
  stat_function(fun = dt,
                args = list(
                  df = d
                )) 




























