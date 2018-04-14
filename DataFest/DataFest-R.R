# Data Fest

sVSj <- ggplot(us.sample.m, aes(us.sample.m$stateProvince, us.sample.m$avgOverallRating))+geom_point()
plot(sVSj)


head(us.sample.m$estimatedSalary)

statesAB = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

stateIncomeTax = c(5.0,0,4.54,6.9,13.3,4.63,6.99,6.6,0,6.0,8.25,7.4,3.75,3.3,8.98,4.6,6.0,6.0,7.15,5.75,5.1,4.25,9.85,5.0,6.0,6.9,6.84,0,5.0,8.97,4.9,8.82,5.75,2.9,4.997,5.0,9.9,3.07,5.99,7.0,0,6.0,0,5.0,8.95,5.75,0,6.5,7.65,0)

head(states)



# ----- states[1] = "AB"

us.sample.m$stateIncomeTax <- 0

for (i in 1:49987) {
  for (i in 1:nrow(statesAB)) {
    if(us.sample.m$stateProvince[i]==statesAB[i]){
      us.sample.m$stateIncomeTax[i] = stateIncomeTax[i]
    }
  }
}









us.sample.m$avgOverallRatingA=as.factor(us.sample.m$avgOverallRating)

# Rating vs Clicks
rVSc <- ggplot(us.sample.m, aes(us.sample.m$avgOverallRatingA,
                   us.sample.m$clicks, 
                   col=us.sample.m$avgOverallRatingA))+
  geom_point(aes(size=us.sample.m$estimatedSalary))+
  labs(title="Rating vs Clicks", 
       subtitle="From US Data Set", 
       y="Clicks", 
       x="Rating", 
       caption="")+
  coord_cartesian(ylim=c(0, 1250))+
  theme_minimal()+
  theme(legend.title = element_blank())+
  theme(legend.position='none')
plot(rVSc)


us.sample.m$avgOverallRatingA=as.factor(us.sample.m$avgOverallRating)
# Need to do the as factor since rating is all 0..5.
ggplot(cdc, aes(smoke100a, weight, col=smoke100a))+geom_boxplot()

# -------------------- German Data -------------------- #

library(ggplot2)
options(scipen=999) # turn off scientific notation like 1e+06

plot(us.sample.m$educationRequirements)
plot(us.sample.m$estimatedSalary)
length(us.sample.m$estimatedSalary)
length(us.sample.m$educationRequirements)


# --------------------   Graphs    -------------------- #

# ---------- Point Plot for Education vs Salary in DE
ggplot(us.sample.m)+
  geom_boxplot(aes(us.sample.m$educationRequirements,
                   us.sample.m$estimatedSalary, 
                   col=us.sample.m$educationRequirements))+
  labs(title="Education Vs Salary", subtitle="From German Data Set", 
       y="Salary", x="Education Level", caption="", col="Education")+
  theme_minimal()


# Zoom in without deleting the points outside the limits. 
ed_vs_sal <- ggplot(us.sample.m) +
  geom_boxplot(aes(us.sample.m$educationRequirements,
                   us.sample.m$estimatedSalary, 
                   col=us.sample.m$educationRequirements))+
  labs(title="Education Vs Salary", subtitle="From German Data Set", 
       y="Salary", x="Education Level", caption="", col="Education")+
  theme_minimal()
plot(ed_vs_sal)

# As a result, the line of best fit is the same as the original plot.
ed_vs_sal1 <- ed_vs_sal +
  coord_cartesian(ylim=c(0, 20000))  # caps at 20,000
plot(ed_vs_sal1)


# ---------- Point Plot for Clicks vs Salary in DE
ggplot(cus.sample.m)+
  geom_point(aes(us.sample.m$estimatedSalary,
                   us.sample.m$clicks, 
                   col=us.sample.m$estimatedSalary))+
  labs(title="Clicks Vs Est. Salary", subtitle="From German Data Set", 
       y="Clicks", x="Est. Salary", caption="", col="Clicks")+
  theme_minimal()
  
  # labs(title="Education Vs Salary", subtitle="From German Data Set", 
  #      y="Salary", x="Education Level", caption="", col="Education")
  



# -------------------- Detecting Zeros & Blanks -------------------- #
levels(us.sample.m$educationRequirements)
sum(us.sample.m$estimatedSalary == 0)
numEdReq = c(sum(us.sample.m$educationRequirements == "Higher Education"),
             sum(us.sample.m$educationRequirements == "High School"),
             sum(us.sample.m$educationRequirements == "None"),
             sum(us.sample.m$educationRequirements == ""))
numEdReq

#Changing order for box plot and added labels
us.sample.m$educationRequirementsOrdered <- factor(us.sample.m$educationRequirements, levels=c("Higher Education", "High School", "None", ""), labels=c("Higher Ed", "High School", "None", "NA"))



# -------------------- Unique Obs -------------------- #
length(unique(us.sample.m$jobId)) #[1] 28727
length(us.sample.m$jobId)         #[1] 50000
length(unique(us$jobId))        #[1] 71758
length(us$jobId)                #[1] 1925795

# Total DE Set
length(unique(us$jobId))/length(us$jobId) 
#   --->   3.72% of the data is unique

# Sample DE Set 
length(unique(us.sample.m$jobId))/length(us.sample.m$jobId)
#   --->  57.45% of the data is unique


