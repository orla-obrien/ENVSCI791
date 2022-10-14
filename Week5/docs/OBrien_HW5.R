## Hw Number 5

# Load in libraries
library(Sleuth3)
library(Sleuth2)
library(tidyverse)
library(multcomp)
library(agricolae)

# library(cowplot)
# library(mosaic)
# library(coin)
# library(asht)


#### Question 6.24 A Biological basis for homosexuality ####

dat24 <- Sleuth3::ex0624
head(dat24)

dat24$Group <- factor(dat24$Group, levels = c("Group1", "Group2", "Group3", "Group4", "Group5"))

dat24 <- dat24 %>%
  filter(Group!="Group4")

dat24 %>%
  ggplot(aes(Group, Volume, fill=Group)) +
  geom_boxplot() +
  theme_bw() +
  ylab("INAH3 Volume") +
  xlab("") +
  theme(legend.position="none")

dat24 %>%
  ggplot(aes(Group, log(Volume), fill=Group)) +
  geom_boxplot() +
  theme_bw() +
  ylab("Log of INAH3 Volume") +
  xlab("") +
  theme(legend.position="none")

## ANOVA

myAov24  <- aov(log(Volume) ~ Group, data=dat24) 
plot(myAov, which=1) # Plot residuals versus estimated means 
summary(myAov) 

myAov.nolog <- aov(Volume ~ Group, data=dat24)
myComparisons4nolog <- glht(myAov.nolog, linfct = mycontrast4)
summary(myComparisons4nolog)

myContrast <- c(-.5, -.5, 1, 0)

myContrast2 <- c(-1, 0, 0, 1)

myContrast3 <- c(0, 0, -1, 1)

mycontrast4 <- rbind(c(-1, 1, 0, 0)) # this is to test if cause of death matters

MyContrasts <- rbind(myContrast, myContrast2, myContrast3)



myDunnett  <- glht(myAov, linfct = mcp(Group = "Dunnett"))  
summary(myDunnett) 
confint(myDunnett,level=.95) 
opar <- par(no.readonly=TRUE)  # Save current graphics parameter settings
par(mar=c(4.1,8.1,4.1,1.1)) # Change margins 
plot(myDunnett, 
     xlab="Difference in Mean Qualification Score (and Dunnett-adjusted CIs)") 

plot(myComparisons, myComparisons2)
plot(myComparisons2)





# Do heterosexual males tend to differ from homosexual males in the volume of INAH3?

myComparison <- glht(myAov24, linfct = MyContrasts)
summary(myComparison)
confint(myComparison)

myComparisons <- glht(myAov, linfct = myContrast)
summary(myComparisons)

# Do heterosexual males tend to differ from heterosexual females?
myComparisons2 <- glht(myAov, linfct = myContrast2)
summary(myComparisons2)
# Do heterosexual females differ from homosexual males?

myComparisons3 <- glht(myAov, linfct = myContrast3)
summary(myComparisons3)

# does cause of death matter?

myComparisons4 <- glht(myAov24, linfct = mycontrast4)
summary(myComparisons4)

#no

# Analyze the data and write a statistical report including summary of findings, graphical display, 
# and details of the methods used. Also describe the limitations of inferences that can be made. 









# Question 7.28 Brain Activity in Violin and String Players ####

dat28 <- Sleuth3::ex0728
head(dat28)

dat28 <- dat28 %>%
  mutate(Musician = case_when(Years==0 ~ "N", Years!=0 ~"Y"))

dat28 %>%
   ggplot(aes(Musician, Activity, fill=Musician)) +
   geom_boxplot()

dat28 %>%
  ggplot(aes(Activity, fill=Musician)) +
  geom_histogram(position="dodge")


# Is the neuron activity different in the stringed musicians and the controls?
dat28_pivot <- dat28 %>% pivot_longer(cols=1:2, names_to="Treatment", values_to = "Height")
pivot_longer(dat28, names_to = "dat", values_to= "value", cols=1:3)

strng_player <- filter(dat28, Musician=="Y")
not_player <- filter(dat28, Musician=="N")

t.test(strng_player$Activity, not_player$Activity)

# Is the amount of activity associated with the number of years the individual has been playing the instrument?

dat28 %>%
  filter(Years!=0) %>%
  ggplot(aes(Activity, Years)) +
  geom_point() +
  geom_smooth(method=lm)

ggplot() +
  geom_point(data=(filter(dat28, Years!=0)), aes(Activity, Years)) +
  geom_abline(intercept=10.1944, slope = 0.8681)


myLM <- lm(Activity~Years, data=(filter(dat28, Years!=0)))

plot(Activity~Years, data=(filter(dat28, Years!=0)))
abline(myLM)

summary(myLM)


# Question 6.21 Sleuth 2 Failure Time of Bearings ####

dat21 <- Sleuth2::ex0621
head(dat21)


# Which compounds tend to differ in their performance from the others?
# Analyze the data and write a report with statistical findings, graph, and details describing methods.

dat21 %>%
  ggplot(aes(Compound, Time, fill=Compound)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  ylab("Time to failure (Millions of cycles)")


aov21 <- aov(Time ~ Compound, data=dat21)

plot(aov21, which=1) # Plot residuals versus estimated means 
summary(aov21) 

dunc <- duncan.test(aov21,"Compound", 
                   main="Sleuth2 6.21")

# tukey hsd

str(dunc)
plot(out,variation="IQR")
duncan.test(aov21,"Compound",alpha=0.05,console=TRUE)


# Question 7.27 Sleuth 2 Big Bang II ####

dat18 <- Sleuth2::ex0727

dat18

plot(dat18$Velocity, dat18$Distance)

mylm <- lm(Distance ~ Velocity, data=dat18)
abline(mylm)

summary(mylm)

# Master Problem Analyze Case 5.1 using agricolae::duncan.test ####

masterdat <- Sleuth3::case0501


myAov1 <- aov(Lifetime ~ Diet, data = case0501) # One-way analysis of variance
myAov1
summary(myAov1)
summary.lm(myAov1)

attributes(myAov1)

# This plot is not appropriate since the contrasts were hypothesized a priori,
# and the 95% CI's will be tighter given these a priori contrasts

# Analyze the resdual plot 

# Residual vs. fitted plot
plot(myAov1, which=1) # Plot residuals versus estimated means.
summary(myAov1)
# This plot indicates no problems.

# Weindruch et al. (1986) didn't set 5 a priori hypotheses as described in 
# Sleuth3. They used Duncan's multiple comparison procedure, available in the
# agricolae package (installed and loaded above)

out <- duncan.test(myAov1,"Diet", 
                   main="Sleuth3 Case Study 5.1, Duncan's multiple range test")
str(out)
plot(out,variation="IQR")
duncan.test(myAov1,"Diet",alpha=0.05,console=TRUE)

TukeyHSD(myAov1)
plot(TukeyHSD(myAov1))




masterdat$diet    <- factor(masterdat$Diet,labels=c("NN85", "NR40", "NR50", "NP", "RR50", "lopro")) 
myAov2  <- aov(Lifetime ~ diet - 1, data=masterdat) 
myComparisons <- glht(myAov2,
                      linfct=c("dietNR50 - dietNN85 = 0", 
                               "dietRR50  - dietNR50 = 0",
                               "dietNR40  - dietNR50 = 0",
                               "dietlopro - dietNR50 = 0",
                               "dietNN85  - dietNP   = 0")   ) 
summary(myComparisons,test=adjusted("none")) # No multiple comparison adjust.
confint(myComparisons, calpha = univariate_calpha()) # No adjustment




