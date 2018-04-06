#------Mammals data 
library(MASS) #load the package
library(MASS)
data(package="MASS")
?mammals
head(mammals)
#summary function
summary(mammals)

m = median(mammals$body)
mammals1 <- transform(mammals,size=ifelse(mammals$body >= m, "large", "small"))
head(mammals1)
attach(mammals1)
str(size)
levels(size)
mammals1["size"]



l.m=sapply(body,size,length) # find the numbers of large/small mammals
l.m

mean.m=sapply(body,size,mean) # find the mean of the body weight based on the size
mean.m

sd.m=sapply(body,size,sd) # find the standard deviation of the body weight based on the size
sd.m


cbind(mean=mean.m,std.dev=sd.m,n=l.m)

#by: function

by(mammals1,mammals1["size"],summary)



head(mammals1)
#Plot the two histograms
par(mfrow=c(1,2)) #start the graph
hist(mammals1$body,main="Histogram of the body weight",xlab="Body weight")
hist(mammals1$brain,main="Histogram of the brain weight",xlab="Brain weigth")
par(mfrow=c(1,1)) #end the graph


#plot two boxplots: one for body weigth and one for brain weight
boxplot(mammals1$body,mammals1$brain,names=c("Body weigth","Brain weight"))


# plot two boxplot based on the size of mammals
boxplot(mammals1$body~mammals1$size)

# plot two boxplot based on the size of mammals
boxplot(mammals1$brain~mammals1$size)



#Example: Twin IQ

#Practice example
#(IQ of twins separated near birth). The data file "twinIQ.txt"
#contains IQ data on identical twins that were separated near birth
#There are 27 observations on 3 variables
#  Foster:    IQ for twin raised with foster parents
#  Biological: IQ for twin raised with biological parents
#  Social:  Social status of biological parents

twins<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txt",header=T)
head(twins)

summary(twins)

par(mfrow=c(1,6))
boxplot(twins$Foster[twins$Social=="high"],twins$Biological[twins$Social=="high"],xlab="High")
boxplot(twins$Foster[twins$Social=="middle"], twins$Biological[twins$Social=="middle"],xlab="Middle")
boxplot(twins$Foster[twins$Social=="low"], twins$Biological[twins$Social=="low"],xlab="Low")
par(mfrow=c(1,1))




#Summary using "by" function
by(twins,twins["Social"],summary)

by(twins,twins$Social,summary)



by(twins$Foster,twins["Social"],length)
tapply(twins$Foster,twins["Social"],length)

by(twins$Foster,twins["Social"],mean)

by(twins$Foster,twins["Social"],sd)


#Q1: Create a data frame containing all twins with high Soical and find the boxplot 
#Q2: Create a data frame containing all twins with middle Soical and find the boxplot
#Q3: Create a data frame containing all twins with low Soical and find the boxplot

#answer Q1
high<-subset(twins,twins$Social=="high")
boxplot(high$Foster,high$Biological,main="twin IQ with high social parents",names=c("Foster","Biological"))




# Boxplot based the Social status
par(mfrow=c(1,2))
boxplot(twins$Foster~twins$Social,main="Foster",col=2:4)
boxplot(twins$Biological~twins$Social,main="Biological",col=2:4)
par(mfrow=c(1,1))



source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
#Q1: Use what we just learn to find the number of males/females
#Q2: Use what we just learn to find the number of people have excellent/very good,...health conditions
#Q3: Find the summary of BMI based on genlth









#Example 2

caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67),nrow=3,byrow=T)
caff.marital

colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital

names(dimnames(caff.marital)) <- c("marital","consumption")
caff.marital


#find the total of each row
total.caff.row <- margin.table(caff.marital,1)
total.caff.row


#Find the total of each column
total.caff.col <- margin.table(caff.marital,2)
total.caff.col


barplot(total.caff.col, col="white",xlab="Amount consumed",ylab="Frequency")


par(mfrow=c(2,2))
barplot(caff.marital, col="white")
barplot(t(caff.marital), col="white")
barplot(t(caff.marital), col="white", beside=T)
barplot(prop.table(t(caff.marital),2), col="white", beside=T)
par(mfrow=c(1,1))


caff.marital
t(caff.marital)

prop.table(t(caff.marital),2)

barplot(prop.table(t(caff.marital),2),beside=T,  legend.text=colnames(caff.marital), col=c("white","grey80","grey50","black"))


#  Dot Chart plot
dotchart(t(caff.marital), lcolor="black")



# Pie Chart plot


opar <- par(mfrow=c(2,2),mex=0.8, mar=c(1,1,2,1))
slices <- c("white","grey80","grey50","black")
pie(caff.marital["Married",], main="Married", col=slices)
pie(caff.marital["Prev.married",], main="Previously married", col=slices)
pie(caff.marital["Single",], main="Single", col=slices)
par(opar)

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
goodair<-complete.cases(airquality) # give the indices of rows without NA values
head(airquality[goodair,])


#-----------Reading data
# read.table, read.csv : read tabular data
# source: reading in R codes
#read.table:
#       file: the name of a file, or a connection
#       header: logical indicating if the file has a header line
#       sep: a string indicating how the columns are seperated
#       skip: the number of lines to skip from the beginning

twins1<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txt")
head(twins1)
names(twins1)

twins2<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txt",header=T)
head(twins2)
names(twins2)

twins3<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twinIQ.txt",header=T,skip = 3)
head(twins3)
names(twins3)
