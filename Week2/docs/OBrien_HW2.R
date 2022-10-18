
## Hw Number 1

# Load in libraries
library(Sleuth3)
library(Sleuth2)
library(tidyverse)
library(cowplot)
library(mosaic)

# Question 2.22: Male vs Female Intelligence ####

dat22 <- Sleuth3::ex0222

ggplot(data=dat22, aes(Gender, AFQT)) +
  geom_boxplot()

dat22 %>%
  pivot_longer(cols=2:6, names_to="Test", values_to = "Score") %>%

ggplot(aes(Gender, Score, fill=Gender)) +
  geom_boxplot()+
  facet_wrap(~Test) +
  theme_bw() +
  scale_fill_manual(breaks=c("female", "male"), 
                    values=c("lavender", "#99d8c9")) +
  scale_x_discrete(labels=c("Female", "Male")) +
  labs(title="Combined and Composite AFQT Test Scores by Gender")
  

tests <- list()


tests[[1]] <- t.test(AFQT ~ Gender, var.equal = TRUE, data = dat22)
tests[[2]] <-t.test(Arith ~ Gender, var.equal = TRUE, data = dat22)
tests[[3]] <-t.test(Math ~ Gender, var.equal = TRUE, data = dat22)
tests[[4]] <-t.test(Parag ~ Gender, var.equal = TRUE, data = dat22)
tests[[5]] <-t.test(Word ~ Gender, var.equal = TRUE, data = dat22)

results <- sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
})
colnames(results) <- c("AFQT", "Arithmetic", "Math", "Word Knowledge", "Paragraph Comprehension")
rownames(results) <- c("Mean Score (Female)", "Mean Score (Male)", "Confidence Interval (Upper)",
                       "Confidence Interval (Lower)", "p-value")

results




# Question 3.32: College Tuition ####

dat32 <- Sleuth3::ex0332 # load in data

# want to make sure that i can plot things easily, pivot the data to one column
dat32_pivot <- dat32 %>% pivot_longer(cols=3:4, names_to="InOut", values_to = "Cost")

# take a look at the spread of the data
dat32_pivot %>%
  filter(Type=="Public") %>%
  ggplot(aes(InOut, Cost, fill=InOut)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(breaks=c("InState", "OutOfState"), 
                    values=c("#d95f02", "#1b9e77")) +
  scale_x_discrete(labels=c("In State Tuition", "Out of State Tuition")) +
  labs(x= "", y="Cost (USD)", title="Comparison of College Tuition")

# i didnt think it looked very skewed.. but maybe a histogram is a better way to view it
# interquartile ranges are very different

dat32_pivot %>%
  ggplot(aes(Cost)) +
  geom_histogram(binwidth=2000, color="black", fill="blue") +
  facet_grid(rows=vars(Type), cols=vars(InOut)) +
  theme_bw()+
  labs(y="Count", x="Cost (USD)")
  
# now i would say this looks skewed, so lets see what i should do to transform
# note that in vs out of state is the same for private colleges... that makes sense


# use boxcox to see what type of transformation is warranted
MASS::boxcox(Cost ~ Type, data=dat32_pivot,lam=seq(-1,1,.1))

# it looks like a log transformation would be appropriate, because lambda is zero

# now i'll make box plots for the three comparisons
In.V.Out <- dat32_pivot %>%
  filter(Type=="Public") %>%
  ggplot(aes(InOut, log(Cost), fill=InOut)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(size=10)) +
  scale_fill_manual(breaks=c("InState", "OutOfState"), 
                    values=c("#d95f02", "#1b9e77")) +
  scale_x_discrete(labels=c("In State Tuition", "Out of State Tuition")) +
labs(x= "", y="Log of Cost (USD)", title="In vs Out of State Public College Tuition")

Private.V.Public <- dat32_pivot %>%
  filter(InOut=="InState") %>%
  ggplot(aes(Type, log(Cost), fill=Type)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(size=10)) +
  scale_fill_manual(breaks=c("Private", "Public"), 
                    values=c("#d95f02", "#1b9e77")) +
  scale_x_discrete(labels=c("In State Private", "In State Public")) +
  labs(x= "", y="Log of Cost (USD)", title="Private vs Public College In-State Tuition")

Private.V.PublicOut <- dat32_pivot %>%
  filter(InOut=="OutOfState") %>%
  ggplot(aes(Type, log(Cost), fill=Type)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(size=10)) +
  scale_fill_manual(breaks=c("Private", "Public"), 
                    values=c("#d95f02", "#1b9e77")) +
  scale_x_discrete(labels=c("Out of State Private", "Out of State Public")) +
  labs(x= "", y="Log of Cost (USD)", title="Private vs Public College Out of State Tuition")

# use cowplot to display them together
plot_grid(In.V.Out, Private.V.Public, Private.V.PublicOut, labels = c('A', 'B', 'C'))


# t-tests for these three comparisons

# now i want to show the statistical results in a table


tests32 <- list()
t1 <- dat32_pivot %>% filter(Type=="Public") 
t3 <- dat32_pivot %>% filter(InOut=="OutOfState")


tests32[[1]] <- t.test(log(Cost) ~ InOut, var.equal = TRUE, data=t1)
tests32[[2]] <- t.test(log(InState) ~ Type, var.equal = TRUE, data=dat32)
tests32[[3]] <- t.test(log(Cost) ~ Type, var.equal = TRUE, data=t3)

results32 <- sapply(tests32, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

results32
colnames(results32) <- c("In Vs Out of State", "Priv vs Pub (In State)","Priv vs Pub (Out Of State)" )
rownames(results32) <- c("Mean Score (Group 1)", "Mean Score (Group 2)", "Confidence Interval (Upper)",
                       "Confidence Interval (Lower)", "p-value")
results32


# Question 2.21 from Sleuth 2: Weights of Birds that Survived and Perished ####

dat0221 <- Sleuth2::ex0221

ggplot(dat0221, mapping=aes(Status, Weight, fill=Status)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position="none") +
  scale_fill_manual(breaks=c("perished", "survived"), 
                    values=c("#d7301f", "#fdbb84")) +
  scale_x_discrete(labels=c("Perished", "Survived")) +
  labs(x= "", y="Sparrow Weight", title="Weights of Sparrows that Perished or Survived During a Winter Storm")




t.test(Weight ~ Status, data=dat0221)
result21 <- favstats(Weight ~ Status, data=dat0221)

tests21 <- list()



tests21[[1]] <- t.test(Weight ~ Status, data=dat0221)


results21 <- sapply(tests21, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

results21
# Question 3.31 from Sleuth 2: Iron Supplementation ####

dat0331 <- Sleuth2::ex0331

a <- ggplot(dat0331, mapping=aes(Supplement,Iron, fill=Supplement)) +
  geom_boxplot()+
  scale_fill_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y= "Percent Iron Retained", x="")

b <- ggplot(dat0331, mapping=aes(Iron, fill=Supplement, linetype = Supplement, color=Supplement)) +
  geom_histogram(position="identity", alpha=.5, binwidth = .5) +
  scale_fill_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) +
  scale_color_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(x= "Percent Iron Retained", y="Count")

plot_grid(a, b, ncol=1, labels = c('A', 'B'))

# maybe get rid of some outliers

dat0331 <- subset(dat0331, (Supplement=="Fe3" & Iron < 7.5) | (Supplement=="Fe4" & Iron < 10))

a <- ggplot(dat0331, mapping=aes(Supplement,Iron, fill=Supplement)) +
  geom_boxplot()+
  scale_fill_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y= "Percent Iron Retained", x="")

b <- ggplot(dat0331, mapping=aes(Iron, fill=Supplement, linetype = Supplement, color=Supplement)) +
  geom_histogram(position="identity", alpha=.5, binwidth = .5) +
  scale_fill_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) +
  scale_color_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(x= "Percent Iron Retained", y="Count")

plot_grid(a, b, ncol=1, labels = c('A', 'B'))

# looks better but still need to transform i think
# use boxcox to see what type of transformation is warranted
MASS::boxcox(Iron ~ Supplement, data=dat0331,lam=seq(-1,2,.1))

# looks like a square root transformation is suggested

ggplot(dat0331, mapping=aes(Supplement,sqrt(Iron), fill=Supplement)) +
  geom_boxplot()+
  scale_fill_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y= "Percent Iron Retained (Square Root Transformed)", x="")

t.test(sqrt(Iron) ~ Supplement,  data=dat0331)

favstats(sqrt(Iron) ~ Supplement,  data=dat0331)


