
## Hw Number 1

# Load in libraries

library(mosaic)
library(Sleuth2)
library(Sleuth3)
library(ggpubr)
library(tidyverse)

# Question 1.21 ####

# this is all in a text file

# Question 1.27 ####

# load in data
data(ex0127, package="Sleuth3")

# check that it looks ok
str(ex0127)

# look at a box plot to compare the average percent of positive votes by the three parties
ggplot(data=ex0127, aes(Party, PctPro)) +
  geom_boxplot() 

# added a Kruskal-Wallis test to see if the percentage of votes varies significantly by Party
compare_means(PctPro ~ Party, data=ex0127) 



# wanted to try a histogram as well to view the spread of the data more easily


ggplot(data=ex0127, aes(PctPro, fill=Party)) +
  geom_histogram(bins = 25, col=I("black")) +
  facet_wrap(~Party, nrow=3) +
  xlab("Percent of Pro Votes")+
  theme_bw()

# print summary statistics
favstats(PctPro ~ Party, data=ex0127)

# Question 1.16 ####


ex0116 <- Sleuth2::ex0116

ggplot(ex0116, mapping=aes(x=Order, y=Distance)) +
  geom_point() + 
  scale_x_continuous(breaks=seq(0, 10, 2)) +
  theme_bw() +
  labs(title="Distance of Planets from the Sun in Increasing Order")

ex01162 <- mutate(ex0116, lnDist= log(Distance)) 
  ggplot(ex01162, mapping=aes(x=Order, y=lnDist)) +
  geom_point() +
    scale_x_continuous(breaks=seq(0, 10, 2)) +
    theme_bw() +
    labs(y="Natural Log of Distance", title="Log Distance of Planets from the Sun in Increasing Order")

summary(ex0116)

favstats(Distance ~ Order, data = ex0116)

favstats(lnDist ~ Order, data=ex01162)

# Queston 1.26b ####

A <- c(1.31, 1.45, 1.12, 1.16, 1.30, 1.50, 1.20, 1.22, 1.42, 1.14, 1.23, 1.59, 1.11, 1.10, 1.53, 1.52, 1.17, 1.49, 1.62, 1.29)
B <- c(1.13, 1.71, 1.39, 1.15, 1.33, 1.00, 1.03, 1.68, 1.76, 1.55, 1.34, 1.47, 1.74, 1.74, 1.19, 1.15, 1.20, 1.59, 1.47, NA)

RatZinc <- data.frame(A, B)

RatZinc <- pivot_longer(RatZinc, cols=everything(), names_to="Group", values_to = "Zinc_Level")


ggplot(RatZinc, mapping=aes(Group, Zinc_Level, fill=Group)) +
  geom_boxplot() +
  labs(y= "Zinc Concentration (mg/mL)", x="Treatment Group") +
  scale_fill_brewer("BuPu")+
  theme(legend.position="none") 




