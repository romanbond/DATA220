#Chapter 4
#create an vector
seq(20,1,by=-3)
rep(5,4)
rep(1:3,2)
rep(seq(1,5,by=2),2)

#Undefined calculation
1/0
Inf-Inf
-1/0
0/0
(-9)^(1/2)
#Chapter 5: Matrices
#function: matrix
#          rbind
#          cbind
matrix(1:6,nrow=2,ncol=3) # create a 2x3 matrix
matrix(1:6,2,3,byrow=T)
matrix(0,2,3)

#use rbind to create a matrix
rbind(1:5,2:6,3:7)
rbind(1:8,1:2) # shorter vector gets recycled
x=1:2
y=8:9
z=cbind(x,y)
#Extracting elements of a matrix
rm(list=ls()) #remove all the previous variables
x=matrix(1:6,2,3,byrow=T)
x  #get all the elements of x
x[2,1]
x[,1]
x[2,]
x[2,2:3]
x[1:2,2:3]
x[,-1]
#Matrix arithmetic
x
x+7
7*x
x^2
x%%2
x
x[,1]=0
x[2,]=-1
x

#Chapter 7: Built-in function
x=c(2,-3,0,8)
x
sum(x)
x
x^2
sum(x^2)
prod(x)
max(x)
min(x)
median(x)
range(x)
diff(x)
abs(x)

###truncating and rounding
rm(list=ls()) # remove all the previous variable
x=c(289.333,4.9)
x
floor(x)
trunc(x)
round(x)
ceiling(x)

# sorting, ordering, and ranking
rm(list=ls())
x=c(2,0,-3,2)
x
rev(x)
unique(x)
sort(x)
sort(x,decreasing = T)
order(x)
#properties of an object
x=1:8
y=matrix(1:8,2,4)
class(x)
class(y)
mode(x)
mode(y)

#User written function
cube
cube=function(x) x^3
cube(2)
cube(10)
cube(1:10)

##pythagorean theorem
hyp=function(a=3,b=4)
  sqrt(a^2+b^2)

hyp()
hyp(6,8)
hyp(1,1)

#scoping
setxtotwo=function()x=2
setxtotwo
x=3
setxtotwo()
x


