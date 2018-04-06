# DATA_220L_112_18S
Data Analysis 220 working files and notes along with completed labs and exams.


## File Directory

19. 4/5/18 - Lecture20_LogisticRegression_Titanic_s18.R
    1. Using training and test data to predict outcomes and measuring their accuracy.
20. 4/6/18 - Lecture20_LogisticRegression_Titanic_s18_Updated.R
    1. Using training and test data to predict outcomes in a large data set.

<details><summary></summary>
```r
# * This is just a place holder inside the readme code.
#   -------------------------------   ⇩ Lecture Code Below ⇩   -------------------------------   #
#
#
#
#
#
#
#
#
#
# 
#
#
#
#
#
#
#
#
#   -------------------------------   ⇩ Lecture Code Here ⇩   -------------------------------   #
```
</details>

## Lecture Code Directory
<details><summary>Lecture 20 F - Topic Description Here</summary>
<p>

#### Lecture 20 (Updated) F

```r
#---------------Logistic regression- Titanic example
#install.packages("Hmisc")
#install.packages("rms")
require("Hmisc")
require("rms")
#install.packages("ggplot2")
library(ggplot2)
require(ggplot2)


getHdata(titanic3)
head(titanic3)

#age, fare, embarked, body
#age and embarked are the only ones we must fix

#replacing na in age with mean
#find na values in age
index=which(is.na(titanic3$age),arr.ind=TRUE)
titanic3$age[index]=mean(titanic3$age,na.rm=TRUE)

#changing embarked to numerical values
##southampton == 1
##cherbourg == 2
##queenstown == 3

titanic3$embarked1= NA
titanic3$embarked1[titanic3$embarked %in% "Southampton"] = 1
titanic3$embarked1[titanic3$embarked %in% "Cherbourg"] = 2
titanic3$embarked1[titanic3$embarked %in% "Queenstown"] = 3
titanic3$embarked1=as.factor(titanic3$embarked1)

#remove rows with na from embarked, there are only 2
index1=which(is.na(titanic3$embarked1),arr.ind=TRUE)
titanic<-titanic3[-c(index1),]
attach(titanic)

# Several models
##model using only pclass to predict survival
mod1 = glm(survived~pclass,family=binomial)
##model using only embarked
mod2 = glm(survived~embarked1,family=binomial)

##
mod3=glm(survived~pclass+age+sex+sibsp+embarked1, family=binomial)

##pR2 values: higher===> better
pR2(mod1)["McFadden"] 
pR2(mod2)["McFadden"] 
pR2(mod3)["McFadden"] 

##Smaller AIC values are better
AIC(mod1,mod2,mod3)

#train and test
rowtrainTitanic<-sample(1:nrow(titanic),size=800)
trainTitanic=titanic[rowtrainTitanic,]
testTitanic<-titanic[-rowtrainTitanic,]
attach(trainTitanic)


glm.fit.model=glm(survived~pclass+age+sex+sibsp+embarked1, family=binomial,data=trainTitanic)

attach(testTitanic)
glm.probs=predict(glm.fit.model,testTitanic,type="response")

glm.pred=rep("0",507)
glm.pred[glm.probs>.5]="1"

table(glm.pred,testTitanic$survived)
mean(glm.pred==testTitanic$survived)

#--------------------------------Smarket data
library(ISLR)
?Smarket
head(Smarket)
#Q1:Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?

#Q2:Use the full data set to perform a logistic regression with 
#Direction as the response and the five lag variables plus  Volume as predictors. 

#Q3: Use the summary funcion to print the results. Doe any of the predictors appear to
#be statistically significant? If so, which ones?

#Q4:Now fit the logistic regression model using a training data period from 2001 to 2004, 
#with Lag1, Lag2 as the only predictors. Compute the overall fraction of correct predictions for 
#the test data (that is, the data from 2005.)


#--------------------------------Default data
library(ISLR)
?Default
head(Default)
#Q1:Produce some numerical and graphical summaries of the Default data. Do there appear to be any patterns?

#Solution: Piecewise graph 
pairs(Default[,c("default","student","balance","income")], gap = 0, pch = ".")

#Q2:Use the full data set to perform a logistic regression with 
#default as the response and student, balance, and income as predicitors. 

#Solution:
mod1 = glm(default~student+balance+income, data = Default, family = binomial)

#Q3: Use the summary funcion to print the results. Doe any of the predictors appear to
#be statistically significant? If so, which ones?

#Solution:
summary(mod1)
#The student and the balance are statistically significant because the p-value is less than 0.05 or 5%.

#Q4:Now fit the logistic regression model using a train data set which contains 70% of data from Default, with student, and balance as the only predictors. Compute the overall fraction of correct predictions for the test data (that is, the other 30% of data from Default)

#Solution:
index=sample(1:nrow(Default), size = trunc(0.7*nrow(Default)))
train=Default[index,]
test=Default[-index,]
mod2=glm(default~student+balance, family = binomial, data = train)
dim(train)
# [1] 7000    4
dim(test)
# [1] 3000    4
d=rep("No", dim(test)[1]) # dim(test)[1] is used to pull the first number in dim result
prob = predict(mod2, test, "response")
d[prob>=0.5] = "Yes"
table(d, test$default)
# d       No  Yes
#   No  2902   53
#   Yes   12   33
prate = (2902+33)/3000
prate
# [1] 0.9783333


#Q5:?Auto and create a column called mpg01 = 0 if mpg<median(mpg); 1 otherwise.
#     Predict mpg01 using the "good" variables.
Auto$mpg01=ifelse(Auto$mpg<median(Auto$mpg),0,1)
head(Auto)

# ------------------------------ inclass ------------------------------ #
trainIndex=Smarket$Year<2005 #This is a logic vector
trainIndex[1:10]

train=Smarket[trainIndex,] #for train data
test=Smarket[!trainIndex,]

mod1=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=train,family = binomial)
dim(test)
d=rep("Down",252)
prob=predict(mod1, test, type = "response")
prob[1:5]

d[prob>=0.5]="Up"
table(d, test$Direction)
(77+44)/252  #

#Q1: Use Lag1 and Lag2 and repeat what we just did

```

</p>
</details>

<details><summary>Lecture 20 T - Topic Description Here</summary>
<p>

#### Lecture 20 T

```r
#---------------Logistic regression- Titanic example
#install.packages("Hmisc")
#install.packages("rms")
require("Hmisc")
require("rms")
#install.packages("ggplot2")
library(ggplot2)
require(ggplot2)


getHdata(titanic3)
head(titanic3)


# Q1: Use Logistic regression to regress survived ~pclass
#     predict the prob to survive with different class ticket.

# Solution:
#mod1=glm(survived~pclass, family=binomial())
mod1=glm(survived~pclass, family=binomial)
summary(mod1)



# Q2: Model using pclass +parent and children, predict the prob 
#     is you 1st, 4 children: 2nd 4 children.

#Solution:
new_obs = data.frame(pclass = "1st")
predict(mod1, newdata = new_obs, type = "response")

obs = data.frame(pclass = "2nd")
predict(mod1, obs, type = "response")

new3 = data.frame(pclass = "3rd")
predict(mod1, new3, type = "response")



# Q3:
#Solution:
mod2 = glm(survived~pclass + parch, family = binomial)
new_obs = data.frame(pclass="1st",parch=4)
predict(mod2, newdata = new_obs, type = "response")

new_obs3 = data.frame(pclass = "2nd", parch = 4)
predict(mod2, newdata =  new_obs3, type = "response")


# Q4: Mod3 for pclass + #siblings + #parents
# predict prob: a) pclass = 3rd, #s=3, #p=4 b) pclass = 1st, #s=3, #p=3
# based on the three models state which model is better/best.

# Solution:
mod3=glm(survived~pclass+sibsp+parch, family = binomial)
new_obS=data.frame(pclass="3rd",sibsp = 3, parch = 4)
predict(mod3, newdata = new_obS, type = "response")
AIC(mod1, mod2, mod3)
# Mod3 is the best b/c the AIC number is the lowest.





# Pred Ex.1:
mod4=glm(survived~pclass+sibsp+parch, family = binomial, data=train)
d=rep(0,655)
prob=predict(mod4, test, type = "response")
prob[1:10]
# If the probability is bigger than 0.5 then they will survive, 
# these are the percentage chance of surviving. If it is less than
# 0.5 then they die.
d[prob>=0.5]=1
table(d, test$survived)
(337+115)/655  #The ans shows our model's predictions were 69-70% correct
(58+145)/655   #This is the error rate for the model



mod2_1=glm(survived~pclass+sibsp, family = binomial, data = train)
d1 = rep(0,655)
prob = predict(mod2_1, test, type = "response")
d1[prob>=0.5]=1
table(d1, test$survived)





index=sample(1:nrow(titanic3), size = trunc(0.7*nrow(titanic3)), replace = F)
train = titanic3[index,]
test = titanic3[-index,]
mod3 = glm(survived~pclass+sibsp+parch+embarked, family = binomial, data = train)
dim(train)
# [1] 916  14
d = rep(0, 393)
prob3 = predict(mod3, test, type = "response")
d[prob3>=0.5]=1
table(d, test$survived)
# d     0   1
# 0 190  80
# 1  44  79
(206+70)/393
# [1] 0.7022901


#age, fare, embarked, body
#age and embarked are the only ones we must fix

#replacing na in age with mean
#find na values in age
index=which(is.na(titanic3$age),arr.ind=TRUE)
titanic3$age[index]=mean(titanic3$age,na.rm=TRUE)

#changing embarked to numerical values
##southampton == 1
##cherbourg == 2
##queenstown == 3

titanic3$embarked1= NA
titanic3$embarked1[titanic3$embarked %in% "Southampton"] = 1
titanic3$embarked1[titanic3$embarked %in% "Cherbourg"] = 2
titanic3$embarked1[titanic3$embarked %in% "Queenstown"] = 3
titanic3$embarked1=as.factor(titanic3$embarked1)

#remove rows with na from embarked, there are only 2
index1=which(is.na(titanic3$embarked1),arr.ind=TRUE)
titanic<-titanic3[-c(index1),]
attach(titanic)

# Several models
##model using only pclass to predict survival
mod1 = glm(survived~pclass,family=binomial)
##model using only embarked
mod2 = glm(survived~embarked1,family=binomial)

##
mod3=glm(survived~pclass+age+sex+sibsp+embarked1, family=binomial)

##pR2 values: higher===> better
pR2(mod1)["McFadden"] 
pR2(mod2)["McFadden"] 
pR2(mod3)["McFadden"] 

##Smaller AIC values are better
AIC(mod1,mod2,mod3)

#train and test
rowtrainTitanic<-sample(1:nrow(titanic),size=800)
trainTitanic=titanic[rowtrainTitanic,]
testTitanic<-titanic[-rowtrainTitanic,]
attach(trainTitanic)


glm.fit.model=glm(survived~pclass+age+sex+sibsp+embarked1, family=binomial,data=trainTitanic)

attach(testTitanic)
glm.probs=predict(glm.fit.model,testTitanic,type="response")

glm.pred=rep("0",507)
glm.pred[glm.probs>.5]="1"

table(glm.pred,testTitanic$survived)
mean(glm.pred==testTitanic$survived)
```

</p>
</details>

<details><summary>Lecture 19 M - Topic Description Here</summary>
<p>

#### Lecture 19 M

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 19 F - Topic Description Here</summary>
<p>

#### Lecture 19 F

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 18 T - Topic Description Here</summary>
<p>

#### Lecture 18 T

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 17 M - Topic Description Here</summary>
<p>

#### Lecture 17 M

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 16 F - Topic Description Here</summary>
<p>

#### Lecture 16 F

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 15 T - Topic Description Here</summary>
<p>

#### Lecture 15 T

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 14 M - Topic Description Here</summary>
<p>

#### Lecture 14 M

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 13 F - Topic Description Here</summary>
<p>

#### Lecture 13 F

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 12 T - Topic Description Here</summary>
<p>

#### Lecture 12 T

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 11 M - Topic Description Here</summary>
<p>

#### Lecture 11 M

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 10 F - Topic Description Here</summary>
<p>

#### Lecture 10 F

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 9 T - Topic Description Here</summary>
<p>

#### Lecture 9 T

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 8 M - Topic Description Here</summary>
<p>

#### Lecture 8 M

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 7 F - Topic Description Here</summary>
<p>

#### Lecture 7 F

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 6 T - Topic Description Here</summary>
<p>

#### Lecture 6 T

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 5 M - Topic Description Here</summary>
<p>

#### Lecture 5 M

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 4 F - Topic Description Here</summary>
<p>

#### Lecture 4 F

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 3 T - Topic Description Here</summary>
<p>

#### Lecture 3 T

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 2 M - Topic Description Here</summary>
<p>

#### Lecture 2 M

```r
#rcodehere
```

</p>
</details>

<details><summary>Lecture 1 M - Topic Description Here</summary>
<p>

#### Lecture 1 M

```r
#rcodehere
```

</p>
</details>
