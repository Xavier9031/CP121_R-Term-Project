# Student ID: 1082414
# 
# Load package (need to install first)
library(tidyverse)
library(ggplot2)
library(psych)

# Load workspace in the Environment of RStudio
setwd("C:/Users/ed307/OneDrive/TermProject")
# Dataset: Data_Hotels.RData
load("C:/Users/ed307/OneDrive/TermProject/data_hotels.rdata")
# Figure 1.
barplot(sort(table(Data$country), decreasing = TRUE), main = "Figure 1",ylim=c(0,2500) ,col = c(rep("red",3),rep("blue",11)))

# Question 1


#####################
# Data Processing 1
#####################

DataAU = subset(Data,country == "Austria" & city == "Vienna")[c(1,4,9,7,6,5)]
colnames(DataAU)

# Figure 2

barplot(sort(table(DataAU$type), decreasing = TRUE), main = "Figure 2",ylim=c(0,300) ,col = c(rep("red",1),rep("blue",7)))

# Figure 3
pie(c(sort(table(DataAU$type))[7],sort(table(DataAU$type))[8],sum(sort(table(DataAU$type))[1:6])), labels = c(paste("Apartment = ",round(100*sort(table(DataAU$type))[7]/sum(sort(table(DataAU$type))),2),"%"),paste("Hotel = ",round(100*sort(table(DataAU$type))[8]/sum(sort(table(DataAU$type))),2),"%"),paste("Other = ",round(100*sum(sort(table(DataAU$type))[1:6])/sum(sort(table(DataAU$type))),2),"%")), main = "figure 3")

# Figure 4
boxplot(DataAU$rating~DataAU$stars,main = "figure 4", ylab = "Rating", xlab = "Stars", col = rainbow(7, alpha = 0.2))


#####################
# Data Processing 2
#####################

DataAU34 = subset(DataAU,type == "Hotel" & stars >=3 & stars <=4)
nrow(DataAU34) 

# Figure 5
ggplot(DataAU34,aes(x=distance,y=log(price)))+labs(title = "Figure 5")+geom_point(size=2, color="dodgerblue")+geom_smooth(method = "gam", color = "darkcyan")


# Question 3



#####################
# Data Processing 3
#####################
DataAU2 <- DataAU
DataAU2$close_far <- ifelse(DataAU2$distance<0.9, "close", ifelse(DataAU2$distance>=1.8, "far", "medium"))
table(DataAU2$close_far)

#####################
# Data Processing 4
#####################

describe(subset(DataAU2,close_far == "close" )[3:6])
describe(subset(DataAU2,close_far == "far" )[3:6])
# Question 4


#####################
# Data Processing 5
#####################

DataUK = subset(Data,country == "United Kingdom" & city == "London")[c(1,4,9,7,6,5)]
colnames(DataUK)
DataUK34 = subset(DataUK,type == "Hotel" & stars >=3 & stars <=4)
nrow(DataUK34) 

# Figure 6
plot(density(DataAU34$price), main = "figure 6", col = "blue", xlab = "Prce", xlim = c(0,1100), ylim = c(0,0.015))
par(new=T)
plot(density(DataUK34$price), main = "figure 6", col = "red", xlab = "Prce", xlim = c(0,1100), ylim = c(0,0.015))
legend("topright",lty = 1,col = c("blue", "red"),legend = c("Vienna", "London"))

# Question 5

