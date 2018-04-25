library(ggplot2)
?mpg

#I-Boxplots, Jittered plots

#a) scattered plot
ggplot(mpg,aes(drv,hwy))+
  geom_point()
#b)Jittered plot

#Here you ad random noise so data is seperated unlike the previous graph
ggplot(mpg,aes(drv,hwy))+
  geom_jitter()

#c)box plot
# The catagorical value (drv) always goes in the middle.
ggplot(mpg,aes(drv,hwy))+
  geom_boxplot()

#c)box plot
# Picks colors based off of drv catagories
ggplot(mpg,aes(drv,hwy,col=drv))+
  geom_boxplot()


#d) violin plot
ggplot(mpg,aes(drv,hwy))+
  geom_violin()

#Q1: Scattered plot/Jitter plot/
#    boxplot/violin plox of class vs cty

ggplot(mpg, aes(x=class, y=cty))+geom_point()
ggplot(mpg, aes(x=class, y=cty))+geom_jitter()
ggplot(mpg, aes(x=class, y=cty, col=class))+geom_boxplot()
ggplot(mpg, aes(x=class, y=cty, col=class))+geom_violin()

#Q1: Scattered plot/Jitter plot/
#    boxplot/violin plox of model vs cty


#cdc example
source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
head(cdc)
#Q1: scattered plot of genhlth vs weight
ggplot(cdc, aes(genhlth, weight))+geom_point()
#Q2: Jittered plot of genhlth vs weight
ggplot(cdc, aes(genhlth, weight, col=genhlth))+geom_jitter()
#Q3: boxplot
ggplot(cdc, aes(genhlth, weight, col=genhlth))+geom_boxplot()
#Q4: violin plot
ggplot(cdc, aes(genhlth, weight, col=genhlth))+geom_violin()
#Q5: weight vs smoke100
cdc$smoke100a=as.factor(cdc$smoke100)
# Need to do the as factor since smoke100 is all 1 or 0 for do or do not smoke.
ggplot(cdc, aes(smoke100a, weight, col=smoke100a))+geom_boxplot()
#This is the wrong ans since it plots one boxplot but we should have two for those who do and do not smoke.
ggplot(cdc, aes(smoke100, weight, col=smoke100))+geom_boxplot()






#II: Histogram and frequency plot

ggplot(mpg,aes(hwy))+geom_histogram()

ggplot(mpg,aes(hwy))+geom_freqpoly() #frequency plot

ggplot(mpg,aes(hwy))+geom_freqpoly(binwidth=2.5) #frequency plot

ggplot(mpg,aes(hwy))+geom_freqpoly(binwidth=1) #frequency plot

#Question: repeat above with hwy replaced by cty

#Color freqency plot
ggplot(mpg,aes(displ,col=drv))+
  geom_freqpoly(binwidth=0.5)


#Color freqency plot
ggplot(mpg,aes(displ,col=drv))+
  geom_freqpoly(binwidth=0.5)+
  facet_wrap(~drv)
#Histogram plot
ggplot(mpg,aes(displ,fill=drv))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~drv,ncol=2)

#Histogram plot
ggplot(mpg,aes(displ,fill=drv))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~drv,ncol=1)

#Hist for cdc weight vs gender
ggplot(cdc, aes(weight, fill=gender))+geom_histogram()+facet_wrap(~gender)
# Histogram, input must be numberical, boxplot is catagorical. You will lose points of you get this wrong.

#III: Bar chart

ggplot(mpg,aes(manufacturer))+
  geom_bar()


#Bar chart with simple example

df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
head(df)

# Basic barplot
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
p + coord_flip()


# Change the width of bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", width=0.5)
# Change colors
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", color="blue", fill="white")

# Minimal theme + blue fill color
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p

#Barplot with label

# Outside bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme_minimal()
# Inside bars
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=1.5, color="white", size=3.5)+
  theme_minimal()



#Back to mpg data set
#Q1: Use the table function to find the #cars in each class
#Ans This tells us hwo many cars are in each class.
table(mpg$class)   

#Q2: Create a barplot with the numbers inside the bars
# to do this we will create a data frame from table(mpg$class)
df1=data.frame(class=c("2seater","compact","midsize","minivan","pickup","subcompact","suv"),number=c(5,47,41,11,33,35,62))
head(df1)
# ^^^This (df1) is the same as... line 160
ggplot(df1, aes(class,number))+geom_bar(stat="identity", fill="steelblue")+geom_text(aes(label=number),vjust=1.5, color="white",size=3.5)+theme_minimal()
# Just written shorter...
df2=as.data.frame(table(mpg$class))

df3=data.frame(table(cdc$genhlth))
head(df3)
ggplot(df3, aes(Var1, Freq))+geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=Freq),vjust=1.5, color="white", size=3.5)+theme_minimal()

#Q3: repeat Q1,Q2 for the drv

ggplot(cdc, aes(weight, fill=gender))+geom_histogram()+facet_wrap(~gender)
ggplot(mpg, aes(manufacturer))+geom_bar()
ggplot(mpg, aes(manufacturer))+geom_bar()
ggplot(mpg, aes(manufacturer, col=drv))+geom_bar()
ggplot(mpg, aes(drv, fill=drv))+geom_bar()
ggplot(mpg, aes(drv, fill=class))+geom_bar()

#
source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
#Q1: use table function to find the number of males/females
#Q2: create a barplot with the numbers outside the bars
#Q3: repeat Q1,Q2 with smoke100
#Q4: Repeat Q1,Q2 with genhlth

#barplot : gender, fill = gender
#barplot : gender, fill = smoke100a
#barplot : gender, fill = genhlth
#barplot : genhlth, fill = genhlth
#barplot : genhlth, fill = gender
ggplot(cdc,aes(gender,fill=gender))+geom_bar()+facet_wrap(~gender)



#economics data
?economics
#Find all continous/discrete variables

#Time series plot and path plot
ggplot(economics,aes(date,unemploy/pop))+
  geom_line()


#Time series plot and path plot
ggplot(economics,aes(date,unemploy))+
  geom_line()

#Time series plot and path plot
ggplot(economics,aes(unemploy/pop,uempmed))+
  geom_path()+
  geom_point()

  
#Time series plot and path 
year=function(x) as.POSIXlt(x)$year+1900
ggplot(economics,aes(unemploy/pop,uempmed))+
    geom_path(color="grey50")+
  geom_point(aes(color=year(date)))+theme_gray()

#------practice example with cdc
#add bmi in to cdc
#Plot  heigth vs weight using bmi as the scale





# ----- More CDC examples ----- #
table(cdc$genhlth)
df=data.frame(dose=c("D0.5","D1","D2"), len=c(4.2,10,29.5))
df
p=ggplot(df, aes(dose, len))+geom_bar(stat="identity")
p
p+coord_flip()
ggplot(df, aes(dose, len))+geom_bar(stat="identity", col="blue", fill="white")
ggplot(data=df, aes(x=dose, y=len))+geom_bar(stat="identity", fill="steelblue")+geom_text(aes(label=len), vjust=1.2, size=3.5)+theme_minimal()


# ----- CDC Examples End ----- #

cdc$bmi=cdc$weight/cdc$height^2*703

ggplot(cdc,aes(height,weight))+
  #geom_path(color="grey50")+
  geom_point(aes(color=bmi))
  

#III) Modifying the axes
ggplot(mpg,aes(cty,hwy))+
  geom_point(alpha=1/3)

ggplot(mpg,aes(cty,hwy))+
  geom_point(alpha=0.1)


ggplot(mpg,aes(cty,hwy))+
  geom_point(alpha=1/3)+
  xlab("City driving (mpg)")+
  ylab("Highway driving (mpg)")

#Remove the x/y label with NULL

ggplot(mpg,aes(cty,hwy))+
  geom_point(alpha=1/3)+
  xlab(NULL)+
  ylab(NULL)


