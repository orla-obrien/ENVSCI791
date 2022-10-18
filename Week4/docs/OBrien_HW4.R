
## Hw Number 4

# Load in libraries
library(Sleuth3)
library(Sleuth2)
library(tidyverse)
library(cowplot)
library(mosaic)
library(coin)
library(asht)



# Question 5.23 Was T rex warm blooded? ####

# The question asks if there is a difference in the means of oxygen isotopic composition in 12 different T rex bones.

# Load in the data

dat23 <- Sleuth3::ex0523

dat23$Bone <- factor(dat23$Bone,levels=c("Bone1", "Bone2", "Bone3", "Bone4", "Bone5", "Bone6", "Bone7",
                                         "Bone8", "Bone9", "Bone10", "Bone11", "Bone12"), 
                     labels = c("Rib 16", "Gastralia", "Gastralia", "Dorsal Vertebra", "Dorsal Vertebra", "Femur", "Tibia",
                                "Metatarsal", "Phalange", "Proximal caudal", "Mid-caudal", "Distal caudal"
                                                                                                     ))

colourCount = length(unique(dat23$Bone))
getPalette = colorRampPalette(brewer.pal(9, "PuBuGn"))

dat23 %>%
  ggplot(aes(Bone, Oxygen, fill=Bone)) +
  geom_boxplot() +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Oxygen Isotopic Composition")


# Want to get the average of each bone

dat23 <- dat23 %>%
  group_by(Bone) %>%
  mutate(avg.oxy= mean(Oxygen))

# Now I want to only have one value for each bone

dat23 <- dat23 %>%
  distinct(Bone, .keep_all=TRUE)

aov23 <- aov(Oxygen ~ Bone, data=dat23)
plot(aov23, which =1)
summary(aov23)


boxplot(Oxygen ~ Bone, data=dat23)

pt <- pairwise.t.test(dat23$Oxygen, dat23$Bone, p.adjust.method = "bonferroni")

pt.tab <- tidy(pt) %>%
  filter(p.value < 0.05) %>%
  rename(Bone_1 = group1, Bone_2 = group2)

pt.tab <- as.data.frame(pt.tab)
  
  myAov2 <- aov(Oxygen ~ Bone, data=dat23)  # Note the -1 is in the Sleuth3 vignette
  # It drops the dietlopro diet from the analysis, making it the llinear model
  # reference group
  summary(myAov2)
  plot(myAov2, which=1)
  myComparisons <- glht(myAov2)
  summary(myComparisons,test=adjusted("free")) # No multiple comparison adjust.
  confint(myComparisons, calpha = adjusted_calpha())

  myComparisons2 <- glht(myAov2,
                        linfct=c("Bone12 - Bone4 = 0"))
  summary.lm(myAov2)

  
# Question 5.25 Education and future income ####
  
# How strong is the evidence that at least one of the five population distributions is different from the others?
  

  
# load data 
dat25 <- Sleuth3::ex0525

head(dat25)

# view data




dat25$Educ <- factor(dat25$Educ, levels = c("<12", "12", "13-15", "16", ">16"), 
                     labels = c("Less_than_12", "12", "1315", "16", "More_than_16"))
                                                                                         

dat25%>%
  ggplot(aes(Educ, Income2005, fill=Educ)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "BuPu") +
  theme_bw() +
  theme(legend.position = "none")+
  xlab("Education Level") +
  ylab("Income") +
  scale_y_continuous(labels = scales::dollar)


aov25 <- aov(Income2005 ~ Educ, data = dat25)
plot(aov25, which=1)


aov25 <- aov(log(Income2005) ~ Educ, data = dat25)
summary(aov25)


myComparisons <- glht(aov25, linfct = c("Educ12 = 0", 
                       "Educ1315 = 0",
                       "Educ16 = 0",
                       "EducMore_than_16 = 0"))

myComparisons <- glht(aov25)
summary(myComparisons,test=adjusted("free")) 
confint(myComparisons, calpha = adjusted_calpha())

test=adjusted()
summary.lm(aov25)


contrasts <- summary.aov(aov25, split = list(tension=list(L=1, Q=2)))[[1]][c(3, 4, 6, 7), c(2, 1, 4, 5)]










# By how many dollars or by what percent does the mean or median for each of the last four categories
# exceed that of the next lowest category?

avg.income <- dat25 %>%
  group_by(Educ) %>%
  mutate(averg = mean(Income2005)) %>%
  distinct(averg, .keep_all = TRUE)


avg.income <- avg.income %>%
  arrange(averg)



a1 <- avg.income$averg[2] - avg.income$averg[1]
a2 <- avg.income$averg[3] - avg.income$averg[2]
a3 <- avg.income$averg[4] - avg.income$averg[3]
a4 <- avg.income$averg[5] - avg.income$averg[4]


# Question 5.24 Sleuth 2: Vegetarians and Zinc ####

# What evidence is there that pregnant vegetarians tend to have lower zinc levels than pregnant nonvegetarians?


dat24 <- Sleuth2::ex0524
head(dat24)

boxplot(log(Zinc) ~ Group, data=dat24)

dat24%>%
  ggplot(aes(Group, Zinc, fill=Group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "GnBu") +
  theme_bw() +
  theme(legend.position = "none")+
  xlab("") +
  ylab("Zinc Level") +
  scale_x_discrete(labels = c("Pregnant, Not Veg", "Pregnant, Veg", "Not Pregnant, Not Veg"))

aov24 <- aov(log(Zinc) ~ Group, data=dat24)

plot(aov24, which=1)

summary(aov24)
plot(aov24, which=1)
myComparisons <- glht(aov24)

summary(myComparisons,test=adjusted("free"))
confint(myComparisons, calpha = adjusted_calpha())


PregNonVeg <- list(exp(5.18), exp(5.05), exp(5.31))
PregVeg <- list(exp(sum(5.18, -0.01)), exp(sum(5.05, -0.17)), exp(sum(5.31, 0.15)))
NonPregNonVeg <- list(exp(sum(5.18, 0)), exp(sum(5.05, -0.19)), exp(sum(5.31, 0.19)))


res.tab <- rbind(PregNonVeg, PregVeg, NonPregNonVeg)
colnames(res.tab) <- c("Estimate", "Lower CI", "Upper CI")

res.tab
# There is no evidence that pregnant vegetarians have lower zinc levels - the ANOVA was not significant and so the 
# three groups are not different.

# Question 5.25 Sleuth 2: Duodenal ulcers ####

dat0525 <- read.csv(".\\Week4\\data\\ex0525_2nd.csv", head=T)

head(dat0525)


dat0525$Diagnosis <- factor(dat0525$Diagnosis, levels = c("Conrols", "Gallstone", "Ulcer"), 
                     labels = c("Controls", "Gallstone", "Ulcer"))

# Describe the difference in distribution of CCK activity for subjects with gallstones and for healthy subjects

dat0525%>%
  ggplot(aes(Diagnosis, CCK, fill=Diagnosis)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Oranges") +
  theme_bw() +
  theme(legend.position = "none")+
  ylab("CCK Level") 

kruskal.test(CCK ~ Diagnosis, data=dat0525)

wilcox.test(dat0525$CCK, dat0525$Diagnosis, confint=T, p.adjust.method = "bonferroni")

boxplot(log(CCK) ~ Diagnosis, data=dat0525)

aov0525 <- aov(log(CCK) ~ Diagnosis, data=dat0525)
plot(aov0525, which =1)

summary(aov0525)

dat0525%>%
  ggplot(aes(CCK, color=Diagnosis, fill=Diagnosis)) +
  geom_density(alpha=.5) +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") + 
  theme_bw()

pairwise.t.test(dat0525$CCK, dat0525$Diagnosis, p.adjust.method = "bonferroni")

density

# Describe the difference in the distribution of CCK activity for subjects with ulcers, and for healthy subjects.

# Master Problem: Generate graphical ANOVA for Education and Future Income ####

anovaPlot(aov25, stacked = TRUE, base = TRUE, axes = TRUE,
          faclab = TRUE, labels = TRUE, cex = par("cex"),
          cex.lab = par("cex.lab"))


lmod <- lm(Fertility ~ ., data = swiss)

K <- diag(length(coef(lmod)))[-1,]
