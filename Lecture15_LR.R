#-------Correlation------------
#Example 1
attach(cars)
?cars       #List all information about the data set
names(cars) #List all variables in the data set
plot(cars)  #plot the graph of distance based speed
#From the graph, it seems that there is a linear relation 
# between : dist and speed
#We use the cor function to confirm there is indeed a relation
cor(dist,speed)
# the correlation is 0.8 which is very strong
# now we need to find a model that this data fits in
# using a linear model

mod1=lm(dist~speed)
mod1$coefficients
#dist~-17.579095 + 3.932409 * speed

-17.579095 + 3.932409 * 21 
# this is the stopping dist when speed is 21mph

new_obs=data.frame(speed=21)
new_obs
# Output
#  speed
#1    21
predict(mod1, newdata = new_obs)
# Output
#       1 
#65.00149 
Eddie.data=data.frame(speed=30)
Eddie.data
speed
#1    30
predict(mod1, Eddie.data)
#1 
#100.3932 
my.data=data.frame(speed=c(21,30))
my.data
speed
#1    21
#2    30
predict(mod1, my.data)
#1         2 
#65.00149 100.39317 
# 6, 14, 19
my.data2=data.frame(speed=c(6,14,19))
my.data2
speed
#1     6
#2    14
#3    19
predict(mod1, my.data2)
#1         2         3 
#6.015358 37.474628 57.136672 

#      -----      New data      -----       

Ad=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/Advertising.csv",header = T)
par(mfrow=c(1,3))
plot(Ad$TV, Ad$sales)
plot(Ad$radio, Ad$sales)
plot(Ad$newspaper, Ad$sales)
attatch(Ad)
par(mfrow=c(1,1))
attach(Ad)
cor(TV, sales)
#[1] 0.7822244
cor(radio, sales)
#[1] 0.5762226
cor(newspaper, sales)
#[1] 0.228299
mod2 =lm(sales~TV)
mod2$coefficients
#(Intercept)          TV 
#7.03259355  0.04753664 
#sales~7.03259355 + 0.04753664 * TV
plot(TV,sales)
abline(7.03259355, 0.04753664)
abline(mod2,col="red")


my.data3=data.frame(TV=c(120,180,270))
> predict(mod2, my.data3)
1        2        3 
12.73699 15.58919 19.86749 
> cor(TV, sales)
[1] 0.7822244
> mod3 = lm(sales~radio)
> plot(mod3, sales)
Error in plot.lm(mod3, sales) : 'which' must be in 1:6
> plot(radio, sales)
> cor(radio, sales)
[1] 0.5762226
> mod3 = lm(sales~radio)  #lm for linear model
> mod3$coefficients
(Intercept)       radio 
9.3116381   0.2024958 
> plot(radio,sales)
> abline(mod3, col=2)
> my.data4 = data.frame(radio=c(12,14,44)) #these are made up fyi
> my.data4
radio
1    12
2    14
3    44
> predict(mod3, my.data4)
1        2        3 
11.74159 12.14658 18.22145 
> mod4 = lm(sales~newspaper)
> plot(newspaper, sales)
> mod4$coefficients
(Intercept)   newspaper 
12.3514071   0.0546931 
> abline(mod4, col=2)
> my.data5 = data.frame(newspaper=c(10,20,60))
> predict(mod4, my.data4)


> View(mod1)
> summary(mod3)$r.squared
[1] 0.3320325
> summary(mod2)$r.squared
[1] 0.6118751
> summary(mod1)$r.squared
[1] 0.6510794
> summary(mod4)$r.squared
[1] 0.05212045
> mod1=lm(sales~TV)
> head(Ad)
X    TV radio newspaper sales
1 1 230.1  37.8      69.2  22.1
2 2  44.5  39.3      45.1  10.4
3 3  17.2  45.9      69.3   9.3
4 4 151.5  41.3      58.5  18.5
5 5 180.8  10.8      58.4  12.9
6 6   8.7  48.9      75.0   7.2
> mod1=lm(sales~radio)
> mod1=lm(sales~TV)
> mod2=lm(sales~radio)
> mod3=lm(sales~newspaper)
> par(mfrow=c(1,3))
> plot(TV, sales)
> abline(mod1)
> abline(mod1,col=2)
> plot(radio, sales)
> abline(mod2,col=2)
> plot(newspaper, sales)
> abline(mod3,col=2)
> summary(mod1)$r.squared
[1] 0.6118751
> 3 61% of data can be explained by the linear model
Error: unexpected numeric constant in "3 61"
> summary(mod2)$r.squared
[1] 0.3320325
> summary(mod3)$r.squared
[1] 0.05212045
# model 1 is the most accurate data model since 61 percent of the data 
# can be explainedby the linear model

summary(mod1) #for all info on p value and slope etc... in this case we
              # reject the null hyp. since there is a linear relationship
              # for x and y
summary(mod2)
summary(mod3)





rm(list=ls())  # to remove all variables 

#Example 2:
#Moore's law :"The number of transistors on a chip double every  24 months"
# x: time
#that is :         computer speed=(intitial speed)*2^(beta*x)

#equivalently:     log_2(computer speed) = log_2(initial speed) +beta*x

cpu=read.table("http://foxweb.marist.edu/users/duy.nguyen2/CPUspeed.txt",header=TRUE)
#varibles
#     1. year  : calendar year
#     2. month : calendar month
#     3. day   : calendar day
#     4. time  : time in years
#     5. speed : Max IA-32 speed (GHz)
#     5. log10speed: log base 10 of speed
names(cpu)

plot(cpu$year,cpu$speed)
cor(cpu$year,cpu$speed)

#----------------------------------------------------------------------------
#Example 3: Multiple regression
#http://www.statsci.org/data/general/cherry.html
trees<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/cherry.txt",header=TRUE)
#Variable : 
#      Diam   : diameter in inches
#      Height : height in feet
#      Volume : cubic feet
names(trees)

attach(trees)

pairs(trees)
cor(trees)  # Correlation among the varibles






#------------Linear regression---------------------------
#Example 1
attach(cars)
?cars       #List all information about the data set
names(cars) #List all variables in the data set
plot(cars)  #plot the graph of distance based speed
            #From the graph, it seems that there is a linear relation 
            # between : dist and speed
#We use the cor function to confirm there is indeed a relation
cor(dist,speed)
#           We will fit the model
#           dist=alpha+beta * speed +epsilon
#                alpha: intercept
#                beta : slope
?lm()     #lm=linear model
l=lm(dist~speed)
print(l)
summary(l)
l$coefficients

#alpha=-17.579
#beta =3.932
#    dist=-17.579095 + 3.932409*speed

l$residuals
plot(l$residuals,main="Residuals",ylab="y")



plot(cars, main="dist=-17.579095 + 3.932409*speed",xlim=c(0,25))
?abline()  #plot a straight line with the intercept : a; slope: b
abline(-17.579095,3.932409)

#Use the result to predict the dist when speed =21
#aks if 20 is a speed value
which(speed==21)

#method 1: Direct method
-17.579095 + 3.932409*21

#method 2 : use the function provided by R
new=data.frame(speed=21)
predict(l,new)


#-----------------------------------------------------------------------
#Predicting using linear model
# We will devide the whole data set into two set
#            trainingData : contains 80% of randomly choose rows
#            testData     : contains the rest 20% of data 

# Create training data
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars),size=0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data

set.seed(10)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(ames$SalePrice),size=50)  # row indices for training data
trainingData <- ames$SalePrice[trainingRowIndex, ]  # model training data

# Create test data 
testData  <- cars[-trainingRowIndex, ]   # test data


# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model

#Use the resuls from training data to predict 
distPred <- predict(lmMod, testData)  # predict distance

actuals=testData$dist  #the true dist value from the testData
predicteds=distPred    #the predicted dist value 
error=abs(actuals-predicteds) #error of the prediction

cbind(actuals,predicteds,error) # list them column by column so we can compare 

#---------------------------------------------------------------------------
#Example 2:
#Moore's law :"The number of transistors on a chip double every  24 months"
# x: time
#that is :         computer speed=(intitial speed)*2^(beta*x)

#equivalently:     log_2(computer speed) = log_2(initial speed) +beta*x

cpu=read.table("http://foxweb.marist.edu/users/duy.nguyen2/CPUspeed.txt",header=TRUE)
#varibles
#     1. year  : calendar year
#     2. month : calendar month
#     3. day   : calendar day
#     4. time  : time in years
#     5. speed : Max IA-32 speed (GHz)
#     5. log10speed: log base 10 of speed
names(cpu)

plot(cpu$year,cpu$speed)
cor(cpu$year,cpu$speed)

log2(4)
log2(16)
graphics.off()

plot(cpu$year,log2(cpu$speed))
cor(cpu$year,log2(cpu$speed))


L=lm(log2(cpu$speed)~cpu$year)
plot(cpu$year,cpu$speed)
curve(2^(-1151.7756+0.5759*x),add=TRUE)

#Predict the speed in November 13, 2005

predicted.speed=2^(-1151.7756+0.5759*(2005+316/365))
predicted.speed



#Note that the exact value of cpu speed on November 13 2005 was : 3.8 GHz
#This illustrates the danger of extrapolation



#----------------------------------------------------------------------------
#Example 3: Multiple regression
#http://www.statsci.org/data/general/cherry.html
trees<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/cherry.txt",header=TRUE)
#Variable : 
#      Diam   : diameter in inches
#      Height : height in feet
#      Volume : cubic feet
names(trees)

attach(trees)

pairs(trees)
cor(trees)  # Correlation among the varibles

#Practice problem: Regress 
#                  Volume=alpha+beta*Diam
# Use this to predict the Volume when the diameter is 16










#Solution:

V1=lm(Volume~Diam)
summary(V1)

new=data.frame(Diam=16)
predict(V1,new)







#We will fit the model
#   volume= alpha+beta1*Diam +beta2*Height

L2=lm(Volume~Diam+Height)

#Volume =57.9877  +     4.7082*Diam+      0.3393 *Height

summary(L2)

install.packages("scatterplot3d")
library(scatterplot3d) 

graph=scatterplot3d(Diam,Height, Volume, pch=16, highlight.3d=TRUE,
               main="3D Scatterplot")
graph$plane3d(L2)










