source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
n=length(cdc$gender)
T=table(cdc$gender)/n

phat=T[1]

z.value=qnorm((1-0.95)/2)

me=z.value*sqrt(phat*(1-phat)/n)

CI=phat+c(me,-me)
CI



#Chapter 5: Peter Dalgaard

#I) One-sample t-test

daily.intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)




mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)

t.test(daily.intake,mu=7725)

#II)  Wilcoxon signed-rank test
# t-test:  we assume the sample is normally distributed
#       :  Large sample size
hist(daily.intake)
qqnorm(daily.intake)
qqline(daily.intake)
wilcox.test(daily.intake, mu=7725)
#Sinc the p-value >5%, The test is not significant at the 5% level

#Sleeping patterns of college students
#Example 1: H0: mu=8 (hrs)
#           H1: mu is not 8 (hrs)
sleep = c(7.75, 8.5, 8, 6, 8, 6.33, 8.17, 7.75, 7, 6.5, 8.75, 8, 7.5, 3, 6.25, 8.5, 9, 6.5, 9, 9.5, 9, 8, 8, 9.5)
hist(sleep)
qqnorm(sleep)
qqline(sleep)
plot(sleep)
sleep.new=sleep[-14]

t.test(sleep.new,mu=8,conf.level = 0.9)
# Since the p-value is large, there is
#  INSUFFICIENT evidence from the data to conclude the mean sleeping time of
# students is not equal to 8 hours

# Proprtion test
sleep = c(7.75, 8.5, 8, 6, 8, 6.33, 8.17, 7.75, 7, 6.5, 8.75, 8, 7.5, 3, 6.25, 8.5, 9, 6.5, 9, 9.5, 9, 8, 8, 9.5)
# H0 : p = 0.5
# H1:  p is not 0.5
nine.hours = ifelse(sleep >= 9, "yes", "no")
table(nine.hours)
y = 5; n = 24
Test = prop.test(y, n, p=0.5, alternative="two.sided",conf.level = 0.95)
Test


#III) Two sample t-test
library(ISwR)
attach(energy)
energy
t.test(expend~stature)


#Twin data set
twins<-read.table("http://foxweb.marist.edu/users/duy.nguyen2/twins.txt",sep=",",header = TRUE)
names(twins)
#HRWAGEL.......Hourly wage of twin 1
#EDUCL.........Self-reported education (in years) of twin 1
#HRWAGEH.......Hourly wage of twin 2
#EDUCH.........Self-reported education (in years) of twin 2
wageH<-as.numeric(twins$HRWAGEH)
collegeH<-ifelse(twins$EDUCH>12,"Yes","No")
boxplot(wageH~collegeH,horizontal=TRUE)
t.test(wageH~collegeH)



#Iv) Comparison of variances

var.test(expend~stature)

#V) two-sample Wilcoxon test

wilcox.test(expend~stature)

#VI) the paired t-test

attach(intake)
intake
post-pre
t.test(pre,post,paired=T)

#twin data set
wageL<-as.numeric(twins$HRWAGEL)
t.test(wageH,wageL,paired=TRUE)

#VII) The matched-pairs Wilconson test

wilcox.test(pre,post,paired=T)
wilcox.test(wageH,wageL,paired=T)

#cdc example
source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")

#barplot(table(cdc$gender)/20000)

#Q1: add the bmi column into cdc
#Q2: test if the bmi=21.5
#Q3: test if two genders have the same bmi index
#Q4: Combine(relevel): (excellent,very good,good)->good
#                      (fair, poor)   -> bad
#             test if these two new levels
