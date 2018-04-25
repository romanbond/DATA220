# Auto data set notes for final exam and resolving issues with Exam 2

# Q1: Add mpg01 column into the data set
#mpg01 = 1 if mpg > median ; 0 otherwise
Auto$mpg01=ifelse(Auto$mpg>median(Auto$mpg),1,0)
head(Auto)

# Q2: Plot and find the variables that can be used to predict mpg01
# we will use logistic regression to do this
plot(Auto, gap=0, pch=".")
# since the mpg01 is based off of the mpg set then we can use that to predict mpg01
# look at the overview graph and look at correlations between columns

# we choose : displ, horsepower, weight
# here we will use 70% of the data set for trainning data abd 30% for the test data
index=sample(1:nrow(Auto), size=trunc(0.7*nrow(Auto)), replace=F)
train=Auto[index,]
test=Auto[-index,]
# we're trying to predict mpg01 so we cant use linear reg we have to use the glm
mod=glm(mpg01~displacement+horsepower+weight, family = binomial, data=train)
# use can not use mpg to predict mpg01 since its from mpg
# now that we have our model we predict the probability
prob=predict(mod, test, type="response")
d=cbind(test, prob)
# here we are predicting the probability of mpg01 being larger than 0.5 which we get o0.00197, since this is so small then mpg01 will likely be 0, if the prob was closer to 0.95 then we could say mpg01 will likely be = 1.
head(d)
t=rep(1, nrow(test))
# this is where we say if mpg01 < 0.5 then mpg01 is likely 0
t[prob<0.5]=0
table(t, test$mpg01)
# This is the probability
mean(t==test$mpg01)
t1=rep(0, nrow(test))
# this is where we say if mpg01 > 0.5 then mpg01 is likely 1
t1[prob>0.5]=1
mean(t1==test$mpg01)
# So far we've only done the binary classification i.e. only two outcomes
# you can"t do this on sets with 3 or more outcomes


# ------------ Begining of regular lecture ---------- #

#Two-way anova
df=data.frame(sydney=c(75,70,50,65,80,65),
              brisbane=c(75,70,55,60,65,65),
              melbourne=c(90,70,75,85,80,65)
              )
df

sapply(df,mean,data=df)

df.stacked=stack(df)
df.stacked

block=factor(rep(1:6,times=3))
# 1=shoper1 etc

df.stacked.df=data.frame(df.stacked,block)

head(df.stacked.df)

names(df.stacked.df)=c("rating","city","shopper")
head(df.stacked.df)


#Now we are ready to do two-way analysis of variance
#                       without interaction
#Ho: There is no significant effect on rating among the cities
  #Conclusion: reject H0 b/c p-value <5%
#Ho: There is no significant effect on rating among the shoppers
  #Conclusion: we fail to reject H0 b/c p-value >5%

L=aov(rating~city+shopper,data=df.stacked.df)
summary(L)

#Which cities has different ratings
CIs=TukeyHSD(L,which=1)
CIs
plot(CIs)

#Practice example
#step1: install.packages("bootstrap)
#step2: ?scor
#Perform the two-way analysis of variance for scor
#Write down the hypotheses
#Your conclusions

#Example 2:

poison=read.csv("http://foxweb.marist.edu/users/duy.nguyen2/poison.csv",header = T)
head(poison)
df

sapply(df, mean, data=df)

df.stacked=stack(df)
df.stacked

#Now we are ready to do two-way analysis of variance
#                       with interaction  
#Ho: The effect of Poison is significant
#Ho: The effect of treament is significant
#Ho: Poison and Treatment interaction will have no significant effect on Time

L=aov(Time~Poison+Treatment+Poison*Treatment,data=poison)
summary(L)


#Practice example
#Exercise 1: The data in "rounding.txt" gives the times
#required to round first base for 22 baseball players using three styles: rounding
#out, a narrow angle and a wide angle.1 The goal is to determine if the method
#of rounding first base has a significant effect on times to round first base.
#   The data and the format of the data can be viewed using a text editor or
#a spreadsheet. With the data file in the current working directory, input the
#data using
#rounding = read.table("http://foxweb.marist.edu/users/duy.nguyen2/rounding.txt", header=TRUE)
#    Check using the str function that the data is in stacked format with three
#variables: time, method, player, where time is numeric and method and
#player are factors.
#    Analyze the data assuming a randomized block design with time to round
#first base as the response, method of rounding first as the treatment, and
#player as the block variable. Plot residuals to check whether the assumptions
#for the distribution of random error appear to be valid.


#Exercise 2
#The morley data in R contains the classical data
#of Michaelson and Morley on the speed of light, recording five experiments
#of 20 consecutive runs each. The response is the speed of light measurement
#Speed. The experiment is Expt and the run is Run. See the documentation
#(?morley) and also http://lib.stat.cmu.edu/DASL/Stories/
#SpeedofLight.html for more details about the experiments and the data set.
#   Use the str function to check that there are 100 observations of the response
#Speed, Expt, and Run; all integer variables. Convert Expt and Run to
#factors using

#  morley$Expt = factor(morley$Expt)
#  morley$Run = factor(morley$Run)

#Display a boxplot of Speed by Expt. Speed of light is a constant, so we see
#there are some problems because the measurements of speed do not seem to
#be consistent across the five experiments.
#The data can be viewed as a randomized block experiment. What is the
#null hypothesis of interest? Analyze the data and residuals and summarize
#your conclusions.


# ---------- Bootstrap Example ---------- #
install.packages("bootstrap")
library(bootstrap)
?scor

# Need to form our Hypthesis'
# H0: There is no significant effect on scores among the subjects
# H1: There is no significant effect on scored among the students

head(scor)
scor.stacked=stack(scor)
head(scor.stacked)
block=(rep(1:88, 5))
scor.stacked.combined=cbind(scor.stacked, block)
names(scor.stacked.combined)=c("score", "subject", "student")
head(scor.stacked.combined)
L=aov(score~subject+student, data=scor.stacked.combined)
summary(L)
