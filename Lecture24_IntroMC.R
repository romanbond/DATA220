#Basic Monter Calor simulation  with R
#Prob 1: Estimate the probability of having a head
#when tossing a coin 100,000


n=100000

S=sample(c("H","T"),size=n,replace = T)
table(S)[1]/n


S=sample(c("H","T"),size=n,replace = T)
table(S)[1]/n

#Prob 2: Three men and two women sit randomly in a row of 5 chairs.
#Estimate the probability that men and women are alternative

#-----------Simple way-------
nrep=100000
count=0

for(i in 1:nrep){
  x=sample(c("M","W","M","W","M"),replace = F)
  if (all(x[c(1,3,5)]=="M")) count=count+1
}
count/nrep

#--------functional way------

my.pro=function(){
nrep=100000
count=0
for(i in 1:nrep){
  x=sample(c("M","W","M","W","M"),replace = F)
  if (all(x[c(1,3,5)]=="M")) count=count+1
}
count/nrep
}

my.pro()

replicate(10,my.pro())

#Prob3:Roll three dice three times, estimate the probability that
#the sum of three numbers is 10

nrep=100000
count=0
for(i in 1:nrep){
  x=sample(1:6,size=3,replace=T)
  if (sum(x)==10) count=count+1
}

count/nrep

#-------functional form

my.prob=function(){
  nrep=100000
  count=0
  for(i in 1:nrep){
    x=sample(1:6,size=3,replace=T)
    if (sum(x)==10) count=count+1
  }
    count/nrep
}

replicate(10,my.prob())


#Prob 4: Urn 1 contains three white balls and two black balls. 
#Urn 2 contains 1 white ball and three black balls
#A ball is randomly selected from Urn 1 and transferred to Urn 2.
#Next, a ball is randomly selected from Urn 2 and transferred to Urn 1.
#Finally, a ball is randomly selcted from Urn 1.
#Use Monter Carlo to estimate the probability that last selected ball is black


nprep=100000
count=0

for (i in 1:nprep){
  urn1=c("W","W","W","B","B")
  urn2=c("W","B","B","B")
  x=sample(urn1,size=1)
  urn2=c(urn2,x)
  urn2
  y=sample(urn2,size=1)
  urn1=c(urn1,y)
  urn1
  z=sample(urn1,1)
  if(z=="B") count=count+1
}
count/nprep

#--------functional version
my.urn=function(){
  nprep=100000
  count=0
  
  for (i in 1:nprep){
    urn1=c("W","W","W","B","B")
    urn2=c("W","B","B","B")
    x=sample(urn1,size=1)
    urn2=c(urn2,x)
    urn2
    y=sample(urn2,size=1)
    urn1=c(urn1,y)
    urn1
    z=sample(urn1,1)
    if(z=="B") count=count+1
  }
  count/nprep
}

replicate(10,my.urn())

#--Prob 6: A and B agree to meet at the sun dial at 2:00pm
#There arrival times are independent and uniformly distributed 
#from 2:00-3:00 pm
#If one comes, s/he will wait for 15 minutes and leave
#Estimate the probability they will meet.

nrep=100000
x=runif(nprep,0,60)
y=runif(nprep,0,60)
sum(abs(x-y)<15)/nprep


# Prob7: Estimate Pi using MC
myPi = function(){
  n=100000
  numberIn = 0
  for(i in 1:n){
    x = runif(2,-1,1)
    if(x[1]*x[1] + x[2]*x[2] <= 1){
      numberIn = numberIn + 1
    }
    
  }
  return(numberIn/n*4)
}

x=replicate(10,myPi())
x
mean(x)

#---------Practice examples
#Prob 1: Use Monte Carlo to estimate the probability that all six faces 
#exactly once in six tosses of a fair die
#Prob 2: A waiting line consist of 40 men
# and 40 women in a random order.  Use Monte Carlo
# simulation to estimate the probability that now two women in line
# are adjacent to one another





#Q1: estimate the prob of having a head when tossing a coin
# the exact value is 0.5
nrep=100000
count=0
for(i in 1: nrep){
  s=sample(c("H","T"),size=1)
  if(s=="H") count=count+1
}
count
count/nrep

my.head=function(){
  nrep=100000
  count=0
  for(i in 1: nrep){
    s=sample(c("H","T"),1)
    if(s=="H") count=count+1
  }
  count/nrep
}
my.head()
replicate(10,my.head())

# Rolling 3 Dice
my.sum=function(){
  nrep=100000
  count=0
  for(i in 1: nrep){
    x=sample(1:6,1)
    y=sample(1:6,1)
    z=sample(1:6,1)
    if(z+y+z==10) count=count+1
  }
  count/nrep
}

my.sum()
replicate(10, my.sum())


# 3 men & 2 Women sit randomly in a row of 5 chairs
# [m|m|m|w|w] or
# [m|w|m|w|m]
# Estimante the probability
my.mw=function(){
  nrep=100000
  count=0
  for(i in 1: nrep){
    x=sample(c("M","M","M","W","W"),5, replace = T)
    if(all(x[c(1,2,3)]=="M") || all(x[c(2,3,4)]=="M") || all(x[c(3,4,5)]=="M")) count=count+1
  }
  count/nrep
}
my.mw()
replicate(10, my.mw())





# Estimate Pi - area = pi*r^2
my.pi=function(){
  nrep=100000
  count=0
  for(i in 1:nrep){
    x=runif(2,-1,1)
    d=sqrt(x[1]^2+x[2]^2)
    if(d<=1) count=count+1
  }
  count/nrep
}
my.pi()
replicate(10,my.pi())


# To do only the top right corner you would do 
my.piQ=function(){
  nrep=100000
  count=0
  for(i in 1:nrep){
    x=runif(2,0,1)
    d=sqrt(x[1]^2+x[2]^2)
    if(d<=1) count=count+1
  }
  count/nrep*4
}
my.piQ()
replicate(10,my.piQ())


