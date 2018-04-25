source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
head(cdc)
library(ggplot2)
ggplot(cdc,aes(weight))+
  geom_line(stat="density")

ggplot(cdc,aes(weight))+
  geom_line(stat="density",col="red")

ggplot(cdc,aes(weight))+
  geom_density(fill="grey",col="blue")
#Overlay the density on the histogram

ggplot(cdc,aes(weight,y=..density..))+
  geom_histogram(fill="blue")+
  geom_density(col="red")+
  xlim(0,400)

#
ggplot(cdc,aes(weight,y=..density..))+
  geom_histogram(fill="blue",size=0.1)+
  geom_density(col="red")+
  facet_wrap(~gender)+
  theme_classic()+
  xlim(0,400)
  

#
ggplot(cdc,aes(weight,y=..density..))+
  geom_histogram(fill="blue",col="grey60",size=0.1)+
  geom_density(col="red")+
  facet_wrap(~gender)+
  theme_classic()+
  xlim(0,400)
#------Practice example
#Q1: create a density plot for height
ggplot(cdc, aes(height, y=..density..))+geom_histogram(fill="blue", col="red")
#Q2: overlay the density curve on the histogram
ggplot(cdc, aes(height, y=..density..))+geom_histogram(fill="blue", size=0.1)
#Q3: use facet to seprate the density plot from Q2
ggplot(cdc, aes(height, y=..density..))+geom_histogram(fill="blue", size=0.1)+geom_density(col="red")+facet_wrap(~gender)+theme_minimal()+xlim(0,400)



?faithful
names(faithful)
#Q1: create a density plot for waiting
ggplot(faithful, aes(waiting, y=..density..))+geom_histogram(fill="blue", col="grey60")+geom_density(col="red")
#Q2: overlay the density curve on the histogram



#Legends
ggplot(cdc,aes(x=genhlth,y=height,fill=genhlth))+
  geom_boxplot()

#remove Legends
ggplot(cdc,aes(x=genhlth,y=height,fill=genhlth))+
  geom_boxplot()+
  guides(fill=F)

#Change the position of the legends

ggplot(cdc,aes(x=genhlth,y=height,fill=genhlth))+
  geom_boxplot()+
  theme(legend.position = "top")


ggplot(cdc,aes(x=genhlth,y=height,fill=genhlth))+
  geom_boxplot()+
  theme(legend.position = "bottom")

ggplot(cdc,aes(x=genhlth,y=height,fill=genhlth))+
  geom_boxplot()+
  theme(legend.position = "left")

#Changing legend title
ggplot(cdc,aes(x=genhlth,y=height,fill=genhlth))+
  geom_boxplot()+
  labs(fill="Group")

ggplot(cdc, aes(age, height, col=gender))+geom_point()
ggplot(cdc, aes(age, height, col=gender))+geom_point(aes(size=age))

#Q1: Consider the gender vs weight
#A)Find the boxplot filled with gender, legend on the right
#b)Find the boxplot filled with gender, legend on the left
#c)Find the boxplot filled with gender, legend on the top
#d)Find the boxplot filled with gender, legend on the bottom
#e)change the legend title to "Sex"
#f) Practice a)--e) using the mpg data set

#-------------------------------------------  
##multiple legends
ggplot(cdc,aes(x=age,y=height,col=gender))+
  geom_point()


ggplot(cdc,aes(x=age,y=height,col=gender))+
  geom_point(aes(size=age))
  
#Q2: consider the age and weight
#A) Find the scatter plot of age vs weight
ggplot(cdc, aes(age, weight))+geom_point()
ggplot(cdc, aes(age, weight, col=age))+geom_point()
#B) Add the age legend into the plot
ggplot(cdc, aes(age, weight))+geom_point(aes(size=age))
#c) Practice a)--b) using the mpg data set
ggplot(mpg, aes(cty, hwy, col=drv))+geom_point(aes(size=cty))

ggplot(mpg, aes(displ,hwy,col=cyl))+geom_point(aes(size=displ))

?mpg

ggplot(mpg, aes(cty, hwy, col=class))+geom_point()









#-----------------------------------------
####ggplot : function-Example 2

1e+06 # =1,000,000

# Setup
options(scipen=999)  
# turn off scientific notation like 1e+06
library(ggplot2)
data("midwest", 
     package = "ggplot2")  # load the data
?midwest
str(midwest)
head(midwest)

# Coninuous and Catagorical data in this set
# Cat: county, state, 
# Con: 

# Init Ggplot
# nothing will show up
ggplot(midwest,
       aes(x=area,
           y=poptotal))  # area and poptotal are columns in 'midwest'


# add points into the graph
ggplot(midwest,
       aes(x=area,
           y=poptotal)) + 
  geom_point()

# Add regression line to this
g <- ggplot(midwest, aes(x=area, y=poptotal, col=state))+geom_point()+geom_smooth(method = "lm")

g <- ggplot(midwest, aes(x=area, y=poptotal, col=state))+geom_point(aes(size=poptotal))

g <- ggplot(midwest, aes(x=area, y=poptotal, col=state))+geom_point(aes(size=poptotal))+ylim(c(0,1000000))

plot(g)

#change lengend to "Midwest State"
g <- ggplot(midwest, aes(x=area, y=poptotal, col=state))+geom_point(aes(size=poptotal))+ylim(c(0,100000))+labs(col="Midest State")
plot(g)
# add paths into the graph
ggplot(midwest, 
       aes(x=area,
           y=poptotal)) + 
  geom_line()


#You can save the plot to a variable and use the plot function to plot it
#   + geom_smooth(method="lm")  : add a linear regression + 95CI
g <- ggplot(midwest,
            aes(x=area,
                y=poptotal)) + 
  geom_point() +
  geom_smooth(method="lm")  
plot(g)


#You can save the plot to a variable and use the plot function to plot it
#   + geom_smooth(method="lm")  : add a linear regression + 95CI
# set se=FALSE to turnoff confidence bands
g <- ggplot(midwest,
            aes(x=area,
                y=poptotal)) +
  geom_point() + 
  geom_smooth(method="lm",se = F)  
plot(g)


#Set axes' limits
g <- ggplot(midwest,
            aes(x=area, 
                y=poptotal)) + 
  geom_point() + 
  geom_smooth(method="lm") 
g + xlim(c(0, 0.1)) +
  ylim(c(0, 1000000))   # deletes points
# Delete the points outside the limits<<<<=====



# Zoom in without deleting the points outside the limits. 
g <- ggplot(midwest, 
            aes(x=area,
                y=poptotal)) +
  geom_point() +
  geom_smooth(method="lm")  
# As a result, the line of best fit is the same as the original plot.
g1 <- g +
  coord_cartesian(xlim=c(0,0.1),
                  ylim=c(0, 1000000))  # zooms in
plot(g1)



#add titles into the graph

g <- ggplot(midwest, 
            aes(x=area,
                y=poptotal)) +
  geom_point() + 
  geom_smooth(method="lm")  

g1 <- g + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in

g1 + labs(title="Area Vs Population", 
          subtitle="From midwest dataset", 
          y="Population", x="Area", 
          caption="Midwest Demographics")

# or
g1 + ggtitle("Area Vs Population",
             subtitle="From midwest dataset") +
  xlab("Area") +
  ylab("Population")


#or 

ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population",
       subtitle="From midwest dataset",
       y="Population", x="Area", 
       caption="Midwest Demographics")



######Change the size of the points+ color

ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(col="steelblue", size=3) +   # Set static color and size for points
  geom_smooth(method="lm", col="firebrick") +  # change the color of line
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population",
       subtitle="From midwest dataset",
       y="Population", x="Area", 
       caption="Midwest Demographics")


#Add colors for different states
gg <- ggplot(midwest, aes(x=area,
                          y=poptotal)) + 
  geom_point(aes(col=state),
             size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm",
              col="firebrick", 
              size=2) + 
  coord_cartesian(xlim=c(0, 0.1),
                  ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population",
       subtitle="From midwest dataset",
       y="Population", x="Area",
       caption="Midwest Demographics")
plot(gg)


#Add colors for different states
gg <- ggplot(midwest, 
             aes(x=area,
                 y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", 
              col="firebrick",
              size=2) + 
  coord_cartesian(xlim=c(0, 0.1), 
                  ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", 
       subtitle="From midwest dataset",
       y="Population", x="Area", 
       caption="Midwest Demographics")
gg + theme(legend.position="None")  # remove legend
gg + scale_colour_brewer(palette = "Set1")  # change color palette
plot(gg)



#Change the scales of the x-axis
# Base plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
# Change breaks
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))


#Reverse the x-scale
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Reverse X Axis Scale
gg + scale_x_reverse()


#Theme

gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

gg <- gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01))
gg + theme_classic() + labs(subtitle="Classic Theme") # set classic theme


#cdc example
source("http://foxweb.marist.edu/users/duy.nguyen2/cdc.R")
head(cdc)
#a ) use the ggplot to plot the age as a function of weight
#b ) use the ggplot to plot the age as a function of weight
#    add colors using general health conditions
#c ) use the ggplot to plot the age as a function of weight
#    add colors using general health conditions
#    

#d ) use the ggplot to plot the age as a function of weight
#    add colors using genders

#e ) use the ggplot to plot the age as a function of weight
#    add colors using genders
#    


