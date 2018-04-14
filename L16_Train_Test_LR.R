#Plot 3D regression
trees<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/cherry.txt",header=TRUE)
attach(trees)
#We will fit the model
#   volume= alpha+beta1*Diam +beta2*Height

L2=lm(Volume~Diam+Height)
summary(L2)$r.squared
#install.packages("scatterplot3d")
library(scatterplot3d) 

graph=scatterplot3d(Diam,Height, Volume, pch=16,
                    highlight.3d=TRUE,
                    main="3D Scatterplot")
graph$plane3d(L2)


#--------Practice with Advertising data set
ad=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/Advertising.csv",head=TRUE)
head(ad)
attach(ad)
#Q1: regress sales ~TV+radio. What is the R^2 value
#Q2: Plot the points + regression surface
#Q3: do the same for the following model
#            sales~radio+newspaper
#            sales~TV+newspaper
#-----------Interaction terms
mod1=lm(sales~TV+radio+TV*radio)
summary(mod1)$r.squared

#Practice with different intraction term

#-----------------------------------------------------------------------
#Predicting using linear model
# We will devide the whole data set into two set
#            trainingData : contains 80% of randomly choose rows
#            testData     : contains the rest 20% of data 

# Create training data
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars),size=0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data

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

#-------------------Practice--------------------------------------------------------
ad=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/Advertising.csv",head=TRUE)
#Q1: Split the adversiting data into train data and test data, each constains 50% of data values
#Q2: fit the model: sales~TV+radio on the train data
#Q3: use it to predict the sales on the test data
#Q4: print out the predicted value+ actual values + error for the test data
#Q5: repeat with : sales ~TV+radio+TV*radio
