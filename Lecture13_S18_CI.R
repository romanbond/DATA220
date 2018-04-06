#Find probability
#dt(x, df): density function
#pt(q, df): probability
#qt(p, df): quantile
#rt(n, df): random t-distribution--->generate n random t-distribution
dt(0,df=40)
pt(0,df=40)
qt(0.025,df=40)
rt(1,df=40) #generate 1 t-dis
rt(10,df=40) #generate 10 t-dis

#graph t-distributions with different degrees of freedom

par(mfrow=c(2,2)) #set the graphic panel to be 2x2
#df=1
plot(seq(-6,6,length=10000),dnorm(seq(-6,6,length=10000)),
     type="l",lty=3,ylab="",xlab="",main="t-dist w/ df=1")
lines(seq(-6,6,length=10000),dt(seq(-6,6,length=10000),df=1),
      type="l",ylab="",xlab="")
legend(x=2,y=.4,lty=c(1,3),legend=c("t-dist, df=1","N(0,1)"))

#df=5
plot(seq(-6,6,length=10000),dnorm(seq(-6,6,length=10000)),
     type="l",lty=3,ylab="",xlab="",main="t-dist w/ df=5")
lines(seq(-6,6,length=10000),dt(seq(-6,6,length=10000),df=5),
      type="l",ylab="",xlab="")
legend(x=2,y=.4,lty=c(1,3),legend=c("t-dist, df=5","N(0,1)"))
#df=20
plot(seq(-6,6,length=10000),dnorm(seq(-6,6,length=10000)),
     type="l",lty=3,ylab="",xlab="",main="t-dist w/ df=20")
lines(seq(-6,6,length=10000),dt(seq(-6,6,length=10000),df=20),
      type="l",ylab="",xlab="")
legend(x=2,y=.4,lty=c(1,3),legend=c("t-dist, df=20","N(0,1)"))

#df=50
plot(seq(-6,6,length=10000),dnorm(seq(-6,6,length=10000)),
     type="l",lty=3,ylab="",xlab="",main="t-dist w/ df=50")
lines(seq(-6,6,length=10000),dt(seq(-6,6,length=10000),df=50),
      type="l",ylab="",xlab="")
legend(x=2,y=.4,lty=c(1,3),legend=c("t-dist, df=50","N(0,1)"))


par(mfrow=c(1,1))






#Confidence Interval
load(url("http://foxweb.marist.edu/users/duy.nguyen2/ames.RData"))
College=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/College.csv",header=T)
head(College)
Auto=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/Auto.csv",header=T,na.strings="?")
Auto=na.omit(Auto)
head(Auto)
attach(Auto)
hist(weight)
hist(displacement)
hist(acceleration)
dim(Auto)

m=mean(acceleration)
s=sd(acceleration)

#95% CI for the mean of the accerlation
n=nrow(Auto)
n
t.value=qt(0.975,n-1)
t.value
ME=t.value*s/sqrt(n)
c(m-ME,m+ME) #print the 95CI



#Getting multiple samples

sample_mean50 <- rep(NA, 100)

for(i in 1:100){
  samp <- sample(acceleration, size = 50, replace = TRUE)
  sample_mean50[i] <- mean(samp)
}
hist(sample_means50)

#Getting multiple samples

sample_mean500 <- rep(NA, 500)

for(i in 1:1000){
  samp <- sample(acceleration, size = 500, replace = TRUE)
  sample_mean500[i] <- mean(samp)
}
hist(sample_means500)

#Contructs 100  95% CIs

sample_mean50 <- rep(NA, 100)
sample_sd50 <- rep(NA, 100)

for(i in 1:100){
  samp <- sample(acceleration, size = 50, replace = TRUE)
  sample_mean50[i] <- mean(samp) #get the sample mean
  sample_sd50[i]<-sd(samp) #get the sample sd
}

t.value=qt(0.975,50-1)
t.value

lower_vector <- sample_mean50 -t.value * sample_sd50 / sqrt(50) 
upper_vector <- sample_mean50 +t.value * sample_sd50 / sqrt(50)

c(lower_vector[1],upper_vector[1]) #the first confidence interval

#Plot all the confidence intervals
plot_ci(lower_vector,upper_vector,mean(acceleration))

#Re-calculate
(100-6)/100


#
source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
#Q1: add a column called bmi
#    contruct 95% CI for the diffrence of bmi between male and female using 500 randonm sample
#    contruct 95% CI for the diffrence of bmi between smoke and non-smkoke using 500 random sample



#Confidence interval for the Binomial parameter: p

head(College)
College$Private

n=length(College$Private)

private=sum(College$Private=="Yes")
private

#95% CI

z_value=qnorm(0.975)
ME=z_value*sqrt(private/n*(1-private/n)/n)
c(private/n-ME,private/n+ME)


 
 
 
 
 