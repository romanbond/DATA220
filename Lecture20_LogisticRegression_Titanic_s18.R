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