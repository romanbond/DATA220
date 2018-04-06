#--------------------------------------------------------------------------
#Confidence band 
ad=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/Advertising.csv",head=TRUE)
head(ad)
attach(ad)
cor(sales,TV)
L1=lm(sales~TV)
summary(L1)
plot(TV,sales,col="red")
abline(L1,col="red")
segments(TV,fitted(L1),TV,sales,col="green")

#Confidence band for predict value

plot(TV,sales,col="red")
abline(L1,col="red")
newTV=seq(20,300,by=10)
pred_interval <- predict(L1, newdata=data.frame(TV=newTV), interval="prediction",
                         level = 0.9)
lines(newTV, pred_interval[,2], col="blue", lty=3)
lines(newTV, pred_interval[,3], col="blue", lty=3)


#Use the ggplot package to plot the confidence interval

#install.packages("ggplot2")
require(ggplot2)
ggplot(ad, aes(x=TV, y=sales))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)

#-----practice example
#https://archive.ics.uci.edu/ml/datasets/Auto+MPG
library(ISLR)
?Auto
#Q1: Produce a scatterplot matrix which includes all of the variables
#in the data set

# Answer:
#pairs(Auto[,c("mpg","cylinders","displacement","horsepower","weight","acceleration","year","origin")],gap=0,pch=".")

#Q2: Compute the matrix of correlations between the variables using
#the function cor(). You will need to exclude the name variable, cor() which is qualitative

# Answer:
#cor(Auto[,c("mpg","cylinders","displacement","horsepower","weight","acceleration","year","origin")])

#Q3:Use the lm() function to perform a multiple linear regression
#with mpg as the response and all other variables except name as
#the predictors. Use the summary() function to print the results.
#Comment on the output. For instance:
#   1)  Is there a relationship between the predictors and the response

# Answer:
# l3=lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=auto)
#summary(l3)

#   2) Which predictors appear to have a statistically significant
#      relationship to the response?

# Answer:
# significant variables: displacement, weight, year, origin because the p-values are <0.05

#   3) What does the coefficient for the year variable suggest

# the year coeff means for every increase in a year by one unit then the mpg increases by 0.75

#Q4: Use the plot() function to produce diagnostic plots of the linear
#regression fit. Comment on any problems you see with the fit
#Q5: Use the *  symbol to fit linear regression models with
#interaction effects. Do any interactions appear to be statistically
#significant
#Q6: Try a few different transformations of the variables, such as
#log(X), SQRT(X), X^2. Comment on your findings

#---------------Analysis of variance - anova------------------



y1<-c(82,93,61,74,69,70,53)
y2<-c(71,62,85,94,78,66,71)
y3<-c(64,73,87,91,56,78,87)





x1bar=mean(y1)
x2bar=mean(y2)
x3bar=mean(y3)

x1bar
x2bar
x3bar

boxplot(y1,y2,y3,names=c("Year1","Year2","Year3"),col=2:3)

x=c(y1,y2,y3)

#grand mean
xbar=mean(x)
xbar
n1=length(y1) # size of the 1st sample
n2=length(y2) # size of the 2nd sample
n3=length(y3) # size of the 3rd sample

#Total variance
SSD.total=sum((x-xbar)^2)
#variance between group
SSD.B=n1*(x1bar-xbar)^2+n2*(x2bar-xbar)^2+n3*(x3bar-xbar)^2;

#Variance within groups
SSD.W=sum(sum((y1-x1bar)^2)+sum((y2-x2bar)^2) +sum((y3-x3bar)^2))

#F-stat
k=3 # there are 3 groups
N=length(x) # size of the big sample

#Compute the F-ratio

F=(SSD.B/(k-1))/(SSD.W/(N-k))

#F-distribution : F=T_1/T_2








curve(df(x,20,20),0,10,col="red")
curve(df(x,k-1,N-k),0,10,col="red")

#Find the 95th quantile of F-disbution
qf(0.95,k-1,N-k)

#Find the p-value
1-pf(F,k-1,N-k)



#Use R to test
combine.y=data.frame(cbind(y1,y2,y3))
stacked.y=stack(combine.y)
results=aov(values~ind,data=stacked.y)
summary(results)




#Example 2: Smoking data 
z1=c(69,52,71,58,59,65)
z2=c(55,60,78,58,62,66)
z3=c(66,81,70,77,57,79)
z4=c(91,72,81,67,95,84)

#Use R to test
combine.z=data.frame(cbind(z1,z2,z3,z4))
stacked.z=stack(combine.y)
results.z=aov(values~ind,data=stacked.z)
summary(results.z)


#cdc data set
source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R") #get the data
names(cdc) # list all the variable names from the data
cdc$bmi=(cdc$weight/cdc$height^2)*703 # add a column bmi into the data
boxplot(cdc$bmi ~ cdc$genhlth) # Find the boxplot based on the health condition
by(cdc$bmi,cdc$genhlth,sd)


#Multiple  histogram
op <- par(mfrow=c(1,5))
for(i in levels(cdc$genhlth)){
  tmp <- with(cdc, bmi[genhlth==i])
  hist(tmp,main=i)
}
par(op)
rm(i,tmp)

#Plot all the qqlines/qqnorm on the sample graph
op <- par(mfrow=c(1,5))
for(i in levels(cdc$genhlth)){
  tmp <- with(cdc, bmi[genhlth==i])
  qqnorm(tmp,xlab="bmi",main=i)
  qqline(tmp)
}
par(op)
rm(i,tmp)

#Do Anova analysis
summary(aov(cdc$bmi~cdc$genhlth))

r=anova(lm(cdc$bmi~cdc$genhlth))
names(r)



#Multiple comparisons: which pairs are different
# Method 1 : ultiple comparisons with Adjusting the level of significan using "bonferroni" method
pairwise.t.test(cdc$bmi,cdc$genhlth,p.adj="bonferroni") 


# Method 2 : ultiple comparisons with Adjusting the level of significan using "Holm" method
pairwise.t.test(cdc$bmi,cdc$genhlth) 

#---------Iris data set---------------
library(ggplot2)
head(iris) #Present iris data
plot(iris[c("Sepal.Length", "Sepal.Width" , "Petal.Length", "Petal.Width")],col=iris$Sepal.Width)
#legend("topleft", legend=levels(iris$Species), pch=16, col=unique(iris$Species))


qplot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
qplot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)

#Q1: use aov to compare the Sepal.Length based on Specices
#Q1: use aov to compare the Sepal.Width based on Specices
#Q1: use aov to compare the Petal.Length based on Specices
#Q1: use aov to compare the Petal.Width based on Specices

#----------------------------------
plant.df = PlantGrowth
plant.df$group = factor(plant.df$group,
                        labels = c("Control", "Treatment 1", "Treatment 2"))
head(plant.df)
tail(plant.df)
attach(plant.df)
boxplot(weight~group,col=2:4)

#Q1: compare the weight based on the group
#Q2: do the pairwise test to see which group is different from the other groups

#----------------------------------
library(ISwR)
data(package="ISwR")
attach(red.cell.folate)
?red.cell.folate
#Method 1: 
summary(aov(folate~ventilation,data=red.cell.folate))
#Method 2: 
anova(lm(folate~ventilation))

#pairwise comparison
pairwise.t.test(folate,ventilation,p.adj="bonferroni")

#-----juul data
?juul
head(factor(juul$tanner))
anova(lm(juul$igf1~juul$tanner))
attach(juul)
juul$tanner=factor(juul$tanner,labels=c("I","II","III","IV","V"))
detach(juul)
attach(juul)
summary(tanner)
anova(lm(igf1~tanner))

