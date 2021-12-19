#Building confidence intervals
#Simulation study
set.seed(123)
#Gaussian population parameters
mu<-10
sigma2<-3

#sample size
n<-10
###################################################
#CI based on one sample
#drawing a sample 
x<-rnorm(n,mu,sqrt(sigma2))

#confidence level (1-alpha)
alpha<-.05

#n is small and the population variance is estimated by s2.tilde
#=> the CI is built using the quantile's of the Student's t distribution
t.alpha.half<-qt(1-alpha/2,n-1)

x.bar<-mean(x)
s2.tilde<-var(x)
LL<-x.bar-t.alpha.half*sqrt(s2.tilde/n)
UL<-x.bar+t.alpha.half*sqrt(s2.tilde/n)

cbind(LL,UL)

#A simulation study: sampling distribution of the CI
##number of samples drawn from the sample space
n.draw <- 100

sample.means<-rep(NA,n.draw)
sample.variances<-rep(NA,n.draw)
lower.limits<-rep(NA,n.draw)
upper.limits<-rep(NA,n.draw)



for(i in 1:n.draw){
  x<-rnorm(n,mu,sqrt(sigma2))
  sample.means[i]<-mean(x)
  sample.variances[i]<-var(x)
  lower.limits[i]<-mean(x)-t.alpha.half*sqrt(var(x)/n)
  upper.limits[i]<-mean(x)+t.alpha.half*sqrt(var(x)/n)
}


#Computing the coverage probability via simulation
cover<-ifelse(lower.limits<mu & upper.limits>mu,1,0)
mean(cover)

cols<-c("blue","green")

windows()
plot(1:n.draw,sample.means,ylim=c(min(lower.limits), 
                                  max(upper.limits)),pch=19) # see for exaple: 
#http://www.endmemo.com/program/R/pchsymbols.php
segments(1:n.draw,lower.limits,
         1:n.draw,upper.limits,
         col=cols[cover+1])
abline(h=mu,col="red")



### with ggplot
# prepare the data
df <- data.frame(iter = 1:n.draw, means = sample.means, LB = lower.limits, 
                 UB = upper.limits, cover = as.factor(cover))


require(ggplot2)
b <- ggplot(df, aes(x=iter, y=means, color = cover, shape=cover)) +   geom_point() + 
  labs(x="Experiments", y="Confidence Interval") +scale_colour_discrete(name = "coverage", labels=c("NO","YES")) + 
  scale_shape_discrete(name = "coverage", labels=c("NO", "YES")) + 
  geom_hline(aes(yintercept=mu),colour="blue", linetype="dashed") + ggtitle(paste("Coverage of ", mean(cover)))




b <- b + geom_segment(aes(x = iter, y = LB, xend = iter, yend=UB , colour = cover), data= df)
windows()
b


#Sampling from a Gamma distribution

# an example of Gamma distribution with shape = 5 and scale = 1
xg <- seq(0.1, 20, by = .1)
yg <- dgamma(xg, shape = 5, scale = 1) # check ?dgamma
plot(xg,yg , type='l', main= "Gamma distribution", ylab = "Density",xlab = "")
lines(xg, dexp(xg, rate = 1), col = "red") # Exponential distribution
# we will use alpha = .1 --> skewed distribution
# parameters of the gamma distribution
alpha.gamma<-.1
beta.gamma<-1
mu.gamma<-alpha.gamma/beta.gamma

#sample size
n<-50

alpha<-.05
z.alpha.half<-qnorm(1-alpha/2)


n.draw<-100

sample.means<-rep(NA,n.draw)
sample.variances<-rep(NA,n.draw)
lower.limits<-rep(NA,n.draw)
upper.limits<-rep(NA,n.draw)

for(i in 1:n.draw){
  x<-rgamma(n,alpha.gamma,beta.gamma)
  sample.means[i]<-mean(x)
  sample.variances[i]<-var(x)
  lower.limits[i]<-mean(x)-z.alpha.half*sqrt(var(x)/n)
  upper.limits[i]<-mean(x)+z.alpha.half*sqrt(var(x)/n)
}



#Undercoverage: the population is skewed, the Gaussian approximation
#is not accurate for n=50
cover.g<-ifelse(lower.limits<mu.gamma & upper.limits>mu.gamma,1,0)
mean(cover.g)

cols<-c("chocolate", "blueviolet")

windows()
plot(1:n.draw,sample.means,ylim=c(min(lower.limits), 
                                  max(upper.limits)),pch=19) 
segments(1:n.draw,lower.limits,
         1:n.draw,upper.limits,
         col=cols[cover+1])
abline(h=mu.gamma,col="darkgreen", lwd= 2)


df <- data.frame(iter = 1:n.draw, means = sample.means, LB = lower.limits, 
                 UB = upper.limits, cover = as.factor(cover.g))


bg <- ggplot(df, aes(x=iter, y=means, color = cover, shape=cover)) +   geom_point() + 
  labs(x="Experiments", y="Confidence Interval") +scale_colour_discrete(name = "coverage", labels=c("NO","YES")) + 
  scale_shape_discrete(name = "coverage", labels=c("NO","YES")) + 
  geom_hline(aes(yintercept=mu.gamma),colour="blue", linetype="dashed") + ggtitle(paste("Coverage of ", mean(cover.g)))

bg <- bg + geom_segment(aes(x = iter, y = LB, xend = iter, yend=UB , colour = cover), data= df) #+ 

windows()
bg

require(ggpubr)
windows()
ggarrange(b, bg, common.legend = T, legend = "bottom", labels = c("Normal", "Gamma"), label.x = 0.5)