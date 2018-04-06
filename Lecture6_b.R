#Normal distribution

pnorm(120,mean=100,sd=15) #Find P(X<120) if X ~ N(100,15)
1-pnorm(120,mean=100,sd=15) #Find P(X>120) if X ~ N(100,15)
qnorm(.95,mean=100,sd=15) # find the 95th percentile of X~N(100,15)
rnorm(1,100,15) #get a sample from N(100,15)

x=rnorm(100000,100,15)
mean(x)
sd(x)

qqnorm(x)
qqline(x)

#Plot the normal curve

curve(1/sqrt(2*pi)*exp(-x^2/2),-4,4)

#Q: Sketch the graph of the curve 1/(15*sqrt(2*pi))*exp(-(x-100)^2/(2*15^2))




source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
hist(cdc$height,20)

qqnorm(cdc$height)
qqline(cdc$height)

#Q: Find the qqplot for weight, age.


table(cdc$smoke100)
table(cdc$smoke100)/20000 
barplot(table(cdc$smoke100))

barplot(table(cdc$gender))

barplot(table(cdc$gender),names.arg=c("male","female"))

barplot(table(cdc$gender),names.arg=c("male","female"),
        xlab   = "Gender",
        ylab   = "Frequency",
        main   = "Barplot for gender",
        col    = "dodgerblue",
        border = "darkorange")

table(cdc$gender,cdc$smoke100)

mosaicplot(table(cdc$gender,cdc$smoke100))

mosaicplot(table(cdc$gender,cdc$smoke100),
           col    = "yellow",
           border = "darkorange")

#Q: Find the mosaicplot of gender vs genhth
#   Find the mosaicplot of genhth vs smoke100


boxplot(cdc$height)

boxplot(cdc$height,
        xlab="Height",
        ylab="Inch",
        col    = "dodgerblue",
        border = "darkorange")


hist(cdc$height,20)

hist(cdc$height,20,
     main="Histogram of Height",
     xlab="Height",
     ylab="Frequency",
     col    = "dodgerblue",
     border = "darkorange")


cdc$bmi <- (cdc$weight / cdc$height^2) * 703
boxplot(cdc$bmi ~ cdc$genhlth)

#change the levels

  #Example 1
cdc$exerany[1:10]

cdc$exerany_cat = NA  # create new variable, and fill it in with NAs first
cdc$exerany_cat[cdc$exerany == 1] = "exercised"
cdc$exerany_cat[cdc$exerany == 0] = "didn't exercise"

boxplot(cdc$bmi~cdc$exerany_cat)

  #Example 1
cdc$genhlth_twolevs = NA  # create new variable, and fill it in with NAs first
cdc$genhlth_twolevs[cdc$genhlth %in% c("fair", "poor")] = "bad"
cdc$genhlth_twolevs[cdc$genhlth %in% c("good", "very good", "excellent")] = "better"
cdc$genhlth_twolevs = as.factor(cdc$genhlth_twolevs)  # save new variable as factor

#
median_height = median(cdc$height)
cdc$height_cat = NA  # create new variable, and fill it in with NAs first
cdc$height_cat[cdc$height < median_height] = "below median"
cdc$height_cat[cdc$height >= median_height] = "at or above median"

table(cdc$height_cat)

cdc$height_cat = as.factor(cdc$height_cat)  # save new variable as factor
cdc$exerany_cat = as.factor(cdc$exerany_cat)  # save new variable as factor


#Example
surveyS15 = read.csv("http://foxweb.marist.edu/users/duy.nguyen2/surveyS15.csv")

#More information abou the data set can be found at:
#http://foxweb.marist.edu/users/duy.nguyen2/Data%20description.pdf
#Q1: explore the data and find all qualitative/quantitative variables
#Q2: Find a color boxplot for countries_visited based on class_year
#Q3: Find a color boxplot for first_kiss based on class_year
#Q4: How many us_region are there from this survey?
#Q5: Create a variable called us_region_fourlevs by combining
#   New England, Southeast---> east
#   Mid-Atlandtic, Midwest----->middle
#   Southwest, California, Pacific Northwest--->west
#   Not from the US--->Not US
#Q6: Find a color boxplot for games_attended based on us_region_fourlevs you just created.
