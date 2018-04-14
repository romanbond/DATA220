

#Example: life expectancy
lifexp<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/LifeExpTable.txt")
head(lifexp)
tail(lifexp)
names(lifexp)
#change the names of lifexp
names(lifexp)=c("country","year")
attach(lifexp)
country
year

min(year)
max(year)
m=which.min(year)
lifexp[m,]
M=which.max(year)
lifexp[M,]

detach(lifexp)
country

#More built-in function
x=c(1:6)
x
sum(x)
cumsum(x)
cumprod(x)

y=c(-1,2,-5,-9)
z=abs(y)
z
log(z)
log10(z)
log2(z)

factorial(z)

#ceiling and floor function

floor(3.7)
floor(-3.7)
ceiling(3.7)
ceiling(-3.7)

round(5.45)

#Properties of an object
x=1:8
y=matrix(1:8,2,4)
class(x)
class(y)
mode(x)
mode(y)

y
nrow(y)
ncol(y)

sapply(1:4,factorial)
factorial(1:4)

y
apply(y,1,max)
apply(y,2,max)

#Do Problem in Chapter 7: 7.11,7.12,7.13,7.15, 7.16




 #Mammals data 
 library(MASS) #load the package
 data(package="MASS")
 head(mammals)
 ?mammals
 names(mammals)
 summary(mammals)
 boxplot(mammals)
 plot(mammals)
 plot(log(mammals$body), log(mammals$brain),
      xlab="log(body)", ylab="log(brain)")
 
 boxplot(log(mammals), names=c("log(body)", "log(brain)"))
 cor(log(mammals))
 cor(log(mammals$body), log(mammals$brain))
 
 lapply(mammals,mean,na.rm=T)
 
 m = median(mammals$body)
 
 
 largemm<-subset(mammals,mammals$body>=m)
 smallmm<-subset(mammals, mammals$body<m)
 head(largemm)
 head(smallmm)
 
 #add a column to the data frame

 #method 1: use transfomr function
 mammals1 <- transform(mammals,size=ifelse(mammals$body >= m, "large", "small"))
 head(mammals1)
 
 #method 2: simpler approach
 mammals$size = ifelse(mammals$body >= m, "large", "small")
 head(mammals)
 #large mammals
  large <- subset(mammals1, mammals1$size == "large")
  head(large)
 
 #Practice example
 #(IQ of twins separated near birth). The data file "twinIQ.txt"
 #contains IQ data on identical twins that were separated near birth
 #There are 27 observations on 3 variables
 #  Foster:    IQ for twin raised with foster parents
 #  Biological: IQ for twin raised with biological parents
 #  Social:  Social status of biological parents
 
 twins<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txt",header=T)
 head(twins)
 #Q1: Create a data frame containing all twins with high Soical and find the boxplot 
 #Q2: Create a data frame containing all twins with middle Soical and find the boxplot
 #Q3: Create a data frame containing all twins with low Soical and find the boxplot
 
 
 
 
 
 
 
 
 #answer Q1
 high<-subset(twins,twins$Social=="high")
 boxplot(high$Foster,high$Biological,main="twin IQ with high social parents",names=c("Foster","Biological"))
 

 
 #--------------------------
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











