##============================================================================================
#Removing NA values
x1<-c(1,2,NA,4,NA,5)
bad<-is.na(x1)
bad
x1[!bad]

#-----------example 2
x2=c("a","b",NA,"d",NA,"e")
good=complete.cases(x1,x2)
good
x1[good]
x2[good]

#--------example 3
?airquality
head(airquality)
any(is.na(airquality)) # asking if there are missing values
goodair<-complete.cases(airquality) # give the indices of rows without NA values
head(airquality[goodair,])


# Brain size example
# Gender:  Male or Female
# FSIQ:  Full Scale IQ scores based on four Wechsler (1981) subtests
# VIQ:  Verbal IQ scores based on four Wechsler (1981) subtests
# PIQ:  Performance IQ scores based on four Wechsler (1981) subtests
# Weight:  Body weight in pounds
# Height:  Height in inches
# MRI:  Count total pixel Count from the 18 MRI scans (size of the brain)

brain=read.table("http://foxweb.marist.edu/users/duy.nguyen2/brainsize.txt",header=TRUE)
summary(brain)
mean(brain$Weight)
mean(brain$Weight,na.rm = TRUE)
by(brain[, -1], brain$Gender, colMeans, na.rm=TRUE)

which(is.na(brain), arr.ind=TRUE) #identify missing values

brain[2,]
brain[21,]

brain[c(2, 21), ] #get rows 2 and 21

brain[2, 5] = mean(brain$Weight, na.rm=TRUE)
brain[21, 5:6] = c(mean(brain$Weight, na.rm=TRUE), mean(brain$Height, na.rm=TRUE))
brain[c(2, 21), ] #get rows 2 and 21

which(is.na(brain), arr.ind=TRUE) #identify missing values



# Ourliers and remove the outliers

# NY RedBull 2012 Salary example
#http://foxweb.marist.edu/users/duy.nguyen2/NYRedBullsalary.pdf
sal=read.table("http://foxweb.marist.edu/users/duy.nguyen2/NYRedBullSal.txt",sep=" ")
names(sal)=c("player","salary")
names(sal)
hist(sal$salary, 20)
boxplot(sal$salary,horizontal = TRUE,main="Salary")



boxplot.stats(sal$salary)$out # find all the ourliers

sal$salary %in% boxplot.stats(sal$salary)$out # ask if a given value is an outlier


# get the index of all outliers
index=which(sal$salary %in% boxplot.stats(sal$salary)$out)

#remove the ourlier
sal2=sal[-c(index),]
sal2
boxplot(sal2$salary/1000,main="Salary in thousand of dollars")

#boxplot(sal$salary/1000,outline = F,main="Salary in thousand of dollars")

#Practice example: Look at mammals data set: remove all the body weight outliers and
# find the boxplox
# do the same for the brain weight
library(MASS)
head(mammals)










remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

sal.rm=remove_outliers(sal$Salary)
boxplot(sal.rm)


#-----------Reading data
# read.table, read.csv : read tabular data
# source: reading in R codes
#read.table:
#       file: the name of a file, or a connection
#       header: logical indicating if the file has a header line
#       sep: a string indicating how the columns are seperated
#       skip: the number of lines to skip from the beginning

twins1<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txtn")
head(twins1)
names(twins1)

twins2<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txt",header=T)
head(twins2)
names(twins2)

twins3<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txt",header=T,skip = 3)
head(twins3)
names(twins3)



#-----------------read.csv example
#http://foxweb.marist.edu/users/duy.nguyen2/lunatics.csv

lunatics<-read.csv("http://foxweb.marist.edu/users/duy.nguyen2/lunatics.csv")



#
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





