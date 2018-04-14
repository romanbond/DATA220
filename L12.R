dbinom(0,size=3,prob=1/2)
x=seq(0,3,by=1)
x
prob_x=dbinom(x,size=3,prob=1/2)
cbind(x,prob_x) # find the distribution table
plot(x,prob_x,type="h",col="red",main="Binom(3,1/2)")


rbinom(1,size=10,prob=0.5)
binom_sample=rbinom(1000,size=10,prob=0.5)
table(binom_sample)
freq=table(binom_sample)
barplot(freq,xlab="X=Number of heads" )


y=seq(0,50,by=1)
prob_y=dbinom(y,size=50,prob=0.2)
plot(y,prob_y,type="h",col="red",main="Binom(50,0.2)")


#Sampling distribution
#https://istats.shinyapps.io/BinomialDist/
outcomes <- c("H", "T")
sample(outcomes, size = 1, replace = TRUE)

sim_fair_coin <- sample(outcomes, size = 1000, replace = TRUE)
T=table(sim_fair_coin)/1000
barplot(table(sim_fair_coin)/1000)
T[1]


#Fair die example

n=10000 #the number of samples
S=rep(0,n)


for(i in 1:n){
  sim_fair_coin <- sample(outcomes, size = 1000, replace = TRUE)
  T=table(sim_fair_coin)/1000
  S[i]=T[1]
}
#> t[1:10]
#[1] 0.054 0.054 0.056 0.055 0.053 0.049 0.046 0.056 0.045 0.054
#each of these are a percentage for flipping a coin 1000 times

#this is the Central Limit Theorem

plot(density(S),main="Density  plot of S")
mean(S)
sd(S)

qqnorm(S)
qqline(S)



start=40
end=60
percentile=0.95
for(i in start:end){
  q=qt(percentile, df=i)
  print(q)
}


#Unfair coin example

n=10000
S1=rep(0,n)

for(i in 1:m){
  sim_fair_coin <- sample(outcomes, size = 1000, replace = TRUE,prob = c(0.1,0.9))
  T=table(sim_fair_coin)/1000
  S1[i]=T[1]
}

mean(S1)
sd(S1)

plot(density(S1),main="Density  plot of S")
qqnorm(S1)
qqline(S1)


#Simulation of a die
outcomes1=c("1","2","3","4","5","6")
n=10000
S2=rep(1,n)

for(i in 1:n){
  sim_fair_die <- sample(outcomes1, size = 1000, replace = TRUE,prob=rep(1/6,6))
  T=table(sim_fair_die)/1000
  S2[i]=T[1]
}

mean(S2)
sd(S2)

plot(density(S2),main="Density  plot of a die")
qqnorm(S2)
qqline(S2)

#T-distribution

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




#Bin machine
#http://vis.supstat.com/2013/04/bean-machine/

