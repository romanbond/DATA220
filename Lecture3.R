#Find the median 
#Example 1
x<-c(6,8,13,2,17,2,14)
sort(x)
median(x)
#Example 2
y<-c(7,4,8,3,5,3)
sort(y)
median(y)
#Find the mean
x1<-c(10,12,15,15,18,20)
mean(x1)

#Histogram
age<-c(36,25,38,46,55,68,72,55,36,38,67,45,22,48,91,46,52,61,58,55)
hist(age)
#Housefly wing length
fly<-read.table("https://www.seattlecentral.edu/qelp/sets/057/s057.txt")
names(fly)
hist(fly$V1,n=20, main="Housefly wing length", xlab="Bins", ylab="Frequency")
var(fly$V1)
sd(fly$V1)
mean(fly$V1)
#Quartiles
z<-c(4, 17, 7, 14, 18, 12, 3, 16, 10, 4, 4, 11)
summary(z)
#Life expectancy example
lifexp<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/LifeExpTable.txt")
head(lifexp)
tail(lifexp)
names(lifexp)
#change the names of lifexp
names(lifexp)=c("countries","lifeexpectancy")
names(lifexp)
plot(lifexp$lifeexpectancy, xlab="Countries", ylab="Life Expectancy") # scatter plot

#Lifex expectancy of USA
lifexp[lifexp$countries=="United_States",]

#sort the life expectancy

lifexp.sorted<-sort(lifexp$lifeexpectancy)
min(lifexp.sorted)
max(lifexp.sorted)
mean(lifexp.sorted)
median(lifexp.sorted)
var(lifexp.sorted)
sd(lifexp.sorted)
boxplot(lifexp.sorted)

# which country has lowest life expectancy
which.min(lifexp$lifeexpectancy)
lifexp$countries[156]
#Which country has the highest life expectancy
which.max(lifexp$lifeexpectancy)
lifexp$countries[88]

#order the countries in term of life expectancy
od<-order(lifexp$lifeexpectancy)
lifexp[od,]







