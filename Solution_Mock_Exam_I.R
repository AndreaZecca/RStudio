##################################################################
###########     Mock Exam 1 - Marco Novelli    ##################
##################################################################


##################################################################
###########               EXERCISE 1            ##################
##################################################################

#Answer to Question 1
data<-read.table("bugs.txt",header=T,sep="\t")

#Answer to Question 2
obj1<-data[1:15,]

#Answer to Question 3
tab.pest<-table(data$pest)

#Answer to Question 4
pest.C<-data[data$pest=="C",]

#Answer to Question 5
counts<-data$count

#Answer to Question 6
obj2<-data[data$count>15,]

#Answer to Question 7
n.obs<-nrow(data)
n.var<-ncol(data)

#Answer to Question 8
mean(counts)
median(counts)
var(counts)
sd(counts)
quantile(counts,c(.1,.9))
min(counts)
max(counts)

#Answer to Question 9
boxplot(data$count~data$pest,xlab="Type of pesticide",ylab="Bugs Count",
        main="Bugs Count vs Type of pesticide",col="red")
abline(h=mean(data$count),col="green",lwd=3)

#or equivalently, since data$pest is a factor variable
plot(data$pest,data$count,xlab="Type of pesticide",ylab="Bugs Count",
     main="Bugs Count vs Type of pesticide",col="red")
abline(h=mean(data$count),col="green",lwd=3)

# using ggplot2
require(ggplot2)
ggplot(data = data, aes(x = count, fill = pest))+
  geom_boxplot(color = "black")+
  coord_flip()+
  geom_vline(xintercept = mean(data$count),
             color = "green",
             size = 1.2)+
  scale_fill_discrete(name = " Type of \n Pesticide")



#Answer to Question 10
#using tapply
#tapply(data$count,data$pest,mean)
xbar.k<-tapply(data$count,data$pest,mean)


#using aggregate - the output is a data.frame object
#whose second column contains group means
aggregate(data$count,list(data$pest),mean)
xbar.k<-aggregate(data$count,list(data$pest),mean)[,2]

#Answer to Question 11
tapply(data$count,data$pest,var)
var.k<-tapply(data$count,data$pest,var)


#Answer to Question 12
#bilding a (1-alpha)% confidence interval
alpha<-0.05
z.half.alpha<-qnorm(1-alpha/2)
LL<-xbar-z.half.alpha*sqrt(var(data$count)/n.obs)
UL<-xbar+z.half.alpha*sqrt(var(data$count)/n.obs)
LL;UL

##################################################################
###########         EXERCISE 2           #########################
##################################################################

my.function<-function(a,b,n,p){
#Computing P(a<=X<=b) with the Binomial distribution
#Using the cumulative distribution function
p.binom<-pbinom(b,n,p)-pbinom(a-1,n,p)#a-1 because we are computing P(a<=X<=b)

#Or equivalently, using the probability mass function
#p.binom<-sum(dbinom(a:b,n,p))

# pay attention to the continuity correction!
#p.norm<-pnorm(b+.5,mean=n*p,sd=sqrt(n*p*(1-p)))-pnorm(a-.5,mean=n*p,sd=sqrt(n*p*(1-p)))
 
p.norm<-pnorm(b,mean=n*p,sd=sqrt(n*p*(1-p)))-pnorm(a,mean=n*p,sd=sqrt(n*p*(1-p)))

output<-list(Binomial=p.binom,Gaussian=p.norm)
return(output)
}

my.function(25,35,100,.3)


##################################################################
###########         EXERCISE 3           #########################
##################################################################
#number of draws
n.sim<-50000

#degrees of freedom
d<-10

#sampling from a standard Gaussian and a chi square with d degrees of freedom (d.f.)
z<-rnorm(n.sim,0,1)
x<-rchisq(n.sim,d)

#thus t.sim contains samples from a Student's t distribution with "d" d.f.
t.sim<-z/sqrt(x/d)

#Indeed
hist(t.sim,freq=F,breaks=50)
curve(dt(x,d),from=qt(.001,d),to=qt(.999,d),add=T,lwd=2,col="blue")

# with ggplot2
require(ggplot2)
df <- data.frame(val = t.sim)

ggplot(data = df, aes(x = val))+
  geom_histogram(bins = 50, aes(y = ..density..), color = "black", fill = "grey")+
  stat_function(fun = dt, args = list(df = d), color = "blue")


##################################################################
###########         EXERCISE 4           #########################
##################################################################
#simulation 
s.sizes <- c(25,100,500)
x.25<-rnorm(25)
x.100<-rnorm(100)
x.500<-rnorm(500)

#histograms
windows()
par(mfrow=c(1,3))
hist(x.25,freq=F,xlim=c(-4,4), main = paste("Sample size of",s.sizes[1]))
curve(dnorm(x),from=-4,to=4,add=T,lwd=2,col="blue")

hist(x.100,freq=F,xlim=c(-4,4), main = paste("Sample size of",s.sizes[2]))
curve(dnorm(x),from=-4,to=4,add=T,lwd=2,col="blue")

hist(x.500,freq=F,xlim=c(-4,4), main = paste("Sample size of",s.sizes[3]))
curve(dnorm(x),from=-4,to=4,add=T,lwd=2,col="blue")

# an alternative and faster way!
s.sizes <- c(25,100,500)
sim <- list()
for(i in 1:length(s.sizes)){
  sim[[i]] <- rnorm(s.sizes[i])
}
#histograms
windows()
par(mfrow=c(1,3))
for(i in 1:length(s.sizes)){
hist(sim[[i]],freq=F,xlim=c(-4,4), main = paste("Sample size of",s.sizes[i]))
curve(dnorm(x),from=-4,to=4,add=T,lwd=2,col="blue")

}


##################################################################
###########         EXERCISE 5           #########################
##################################################################
x<-c(169.2,169.5,177.2,175.0,181.3,169.2,165.9,169.9,180.9,173.5,168.2,184.4,169.6,176.8,
177.2,180.8,171.9,167.6,177.4,176.0)

#Sample size
n<-length(x)

#CI for the mean
alpha<-0.01
t.half.alpha<-qt(1-alpha/2,n-1)
LL<-mean(x)-t.half.alpha*sqrt(var(x)/n)
UL<-mean(x)+t.half.alpha*sqrt(var(x)/n)
# LL;UL or

cbind(LL,UL)


#CI for the mean with known population variance
alpha<-0.1
z.half.alpha<-qnorm(1-alpha/2)
LL<-mean(x)-z.half.alpha*sqrt(30/n)
UL<-mean(x)+z.half.alpha*sqrt(30/n)
# LL;UL or

cbind(LL,UL)
