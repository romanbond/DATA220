#Character strings
x="R"
x
x='S'
x

nchar(x)
x="Hello World"
nchar(x)

substr(x,start=2,stop=10)

x="I can\'t go there"
x
nchar(x)

x=c("R",4.4,3,-8,6.02e23,pi)
x
nchar(x)
substr(x,2,4)

#bult-in objects

letters
LETTERS
letters[18]
month.name
month.abb
state.name
state.abb

#Character string manipulation

animals=c("pig","cow","gnu")
animals
animals[1]
animals[2]
toupper(animals)

substr(animals,1,2)

paste("big",animals)
animals

sprintf("%s %d by %d gives %d","multiplying",3,4,3*4)


#Logical elements

x=TRUE
x
x=T
x=c(T,F,T,F,T,F)
mode(x)
length(x)

any(x) #are there any TRUE elements in x ?
all(x) # are all the elements in x TRUE)

x
!x

TRUE& TRUE
TRUE&FALSE
FALSE&FALSE


y=c(T,T,T,F,F,F)
y
x
x&y

TRUE|FALSE
TRUE|TRUE
FALSE|FALSE

x|y
x
sum(x)
mean(x)

as.numeric(x)

range(x)

diff(x)

x
z=1:6
z
z[x]

#Relational Operators


x=c(2,-3,0,8)
x

x==7

x==8
x[x<0]
y=c(1,4,2,5)
x>y

x[x>y]

which(x<5)
x[which(x<5)]


#Ifelse (test,yes, no)
x=sample(1:10,1)
x

ifelse(x%%2==0,"even","odd")


#Probability
#Titanic data set
test=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/test.csv",header = TRUE)
train=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/train.csv",header = TRUE)
n=length(train$PassengerId)

#Overal survival for all passengers
overral.survival=sum(train$Survived==1)/n
overral.survival

#survival rate for female passengers

f.survival=sum(train$Survived==1 & train$Sex=="female")/sum(train$Sex=="female")
f.survival

#survival rate for male passengers

m.survival=sum(train$Survived==1 & train$Sex=="male")/sum(train$Sex=="male")
m.survival


#class 1 survival rate
p1.survival=sum(train$Pclass==1 & train$Survived==1   )/sum(train$Pclass==1)
p1.survival

#class 2 survival rate
p2.survival=sum(train$Pclass==2 & train$Survived==1   )/sum(train$Pclass==2)
p2.survival

#class 3 survival rate
p3.survival=sum(train$Pclass==3 & train$Survived==1   )/sum(train$Pclass==3)
p3.survival


#cdc example
source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")

