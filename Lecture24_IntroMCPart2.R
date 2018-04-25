#Prob 1: Urn 1 contains three white balls and two black balls. 
#Urn 2 contains 1 white ball and three black balls
#A ball is randomly selected from Urn 1 and transferred to Urn 2.
#Next, a ball is randomly selected from Urn 2 and transferred to Urn 1.
#Finally, a ball is randomly selcted from Urn 1.
#Use Monter Carlo to estimate the probability that last selected ball is black

#--------functional version
my.urn=function(){
  nprep=100000
  count=0
  
  for (i in 1:nprep){
    urn1=c("W","W","W","B","B")
    urn2=c("W","B","B","B")
    x=sample(1:5,size=1)
    urn2=c(urn2,urn1[x])
    
    y=sample(1:5,size=1)
    urn1[x]=urn2[y]
    
    z=sample(urn1,1)
    if(z=="B") count=count+1
  }
  count/nprep
}

replicate(10,my.urn())


my.urn=function(){
  nrep=100000
  count=0 #count the # of balls int eh 3rd pick
  for(i in 1:nrep){
    urn1=c("W","W","W","B","B")
    urn2=c("W","B","B","B")
    x=sample(1:5, size=1)
    urn2=c(urn2,urn1[x])
    y=sample(1:5, size=1)
    urn1[x]=urn2[y]
    z=sample(urn1, size=1)
    if(z=="W") count=count+1
  }
  count/nrep
}

my.urn()
replicate(10,my.urn())

#--Prob 2: A and B agree to meet at the sun dial at 2:00pm
#There arrival times are independent and uniformly distributed 
#from 2:00-3:00 pm
#If one comes, s/he will wait for 15 minutes and leave
#Estimate the probability they will meet.

nrep=100000
x=runif(nprep,0,60)
y=runif(nprep,0,60)
sum(abs(x-y)<15)/nprep

my.date=function(){
  nrep=100000
  count=0
  for(i in 1:nrep){
    A=runif(1,0,60)
    B=runif(1,0,60)
    if(abs(A-B)<=15) count=count+1
  }
  count/nrep
}

my.date()
replicate(10,my.date())

# Prob3: Estimate Pi using MC
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
my.die=function(){
  nrep=100000
  count=0
  for(i in 1:nrep){
    x=sample(1:6, size=6, T)
    x=sort(x)
    if(x[1]==1 && x[2]==2 && x[3]==3 && x[4]==4 && x[5]==5 && x[6]==6) count=count+1
  }
  count/nrep
}

my.die()

my.die2=function(){
  nrep=100000
  count=0
  for(i in 1:nrep){
    x=sample(1:6, size=6, T)
    total=0
    for(j in 1:6){
      for(k in j+1:6){
        if(x[j]!=x[k]) total=total+1
      }
    }
    if(total==6) count=count+1
  }
  count/nrep
}

my.die2()


#Prob 2: A waiting line consist of 40 men
# and 40 women in a random order.  Use Monte Carlo
# simulation to estimate the probability that now two women in line
# are adjacent to one another


#Prob4: Coin tossing
#Peter and Paul play a simple game involving repeated tosses of a fair coin.
#In a given toss, if heads is observed, Peter wins $1 from Paul; otherwise if
#tails is tossed, Peter gives $1 to Paul. If Peter starts with zero dollars,
#we are interested in his change
#in fortune as the game is played for 50 tosses

# Sample the space
set.seed(1) #set.seed so we can have the same outcomes
options(width=60)
win=sample(c(-1, 1), size=50, replace=TRUE)

#Exploring cumulative winnings
cum.win = cumsum(win)
cum.win

#Exploring cumulative winnings in 4 simulations
par(mfrow=c(2, 2))
for(j in 1:4){
   win = sample(c(-1, 1), size=50, replace=TRUE)
   plot(cumsum(win), type="l" ,ylim=c(-15, 15))
   abline(h=0)
   }
par(mfrow=c(1, 1))

#Some questions 
#Q1: What is the chance that Peter will break even after 50 tosses?
#Q2:???? What are likely number of tosses where Peter will be in the lead?
#Q3:???? What will be the value of Peter's best fortune during the game?

set.seed(2)
peter.paul=function(n=50){
  win = sample(c(-1, 1), size=n, replace=TRUE)
  sum(win)
}

peter.paul()
F = replicate(1000, peter.paul())
table(F)
plot(table(F))

#Q1: What is the chance that Peter will break even after 50 tosses?
#Prob(Break even)~116/1000=0.116


#-------Answer Q2, Q3
set.seed(4)
peter.paul=function(n=50){
  win=sample(c(-1, 1), size=n, replace=TRUE)
  cum.win = cumsum(win)
  c(F=sum(win), L=sum(cum.win > 0), M=max(cum.win))
}

peter.paul()
S = replicate(1000, peter.paul())
head(S)
dim(S)
times.in.lead = S["L", ]
plot(prop.table(table(times.in.lead)))


#---------Maximum lead
maximum.lead = S["M",]
plot(table(maximum.lead))
sum(maximum.lead >= 10) / 1000
###----------------------------------------------------------


#####The hat problem
#In the old days, a man
#would wear a top hat to a restaurant and check his hat with a person at the
#door. Suppose n men check their hats and the hats are returned to the men
#in a random fashion when they leave the restaurant. How many men will
#receive their own hats?

n = 10
hats = 1:n
hats
set.seed(5)
mixed.hats = sample(hats)
mixed.hats
hats == mixed.hats

my.hat=function(){
  nrep=100000
  count=0
  for(i in 1:nrep){
    x=sample(1:10)
    if(sum(x==1:10)==0) count=count+1
  }
  count/nrep
}

my.hat()
replicate(10,my.hat())

#Number of people who receive their hats
sum(hats == mixed.hats) 

#Write a function to find the number of correct hats returned
set.seed(6)
scramble.hats = function(n){
  hats = 1:n
  mixed.hats = sample(n)
  sum(hats == mixed.hats)
}

scramble.hats(30)
matches = replicate(1000, scramble.hats(10))
table(matches)

table(matches)/1000
