
## Hw Number 3

# Load in libraries
library(Sleuth3)
library(Sleuth2)
library(tidyverse)
library(cowplot)
library(mosaic)
library(coin)
library(asht)

# Question 4.28: Darwin Data ####


# Are cross fertilized plants superior to self-fertilized plants?

dat28 <- Sleuth3::ex0428
head(dat28)

# Draw a histogram of the differences
ggplot(dat28) +
  geom_histogram(aes(Cross-Self),fill="#4292c6", binwidth = .5, alpha=.5, color="black") +
  theme_bw() +
  xlim(0, 10) +
  labs(x= "Difference in plant height (inches)", y="Count")


dat28 %>%
  t.test(Cross, Self, pair=TRUE)


# Find a two-sided p-value for the hypothesis using a paired t-test

result <- t.test(dat28$Cross, dat28$Self, paired = TRUE)

result$p.value


# Find a 95% confidence interval for the additive treatment effect
result$conf.int

# Is there any indication from the plot that the paired t-test is inappropriate?
dat28_pivot <- dat28 %>% pivot_longer(cols=1:2, names_to="Treatment", values_to = "Height")

ggplot(dat0331, mapping=aes(Iron, fill=Supplement, linetype = Supplement, color=Supplement)) +
  geom_histogram(position="dodge", alpha=.5, binwidth = .5) +
  scale_fill_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) +
  scale_color_manual(values=c("Fe3" = "#238b45", "Fe4" = "#4292c6")) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(x= "Percent Iron Retained", y="Count")

ggplot(dat28_pivot, mapping = aes(Height, fill=Treatment, linetype = Treatment, color=Treatment)) +
  geom_histogram(position="dodge", alpha=.5, binwidth = .5) +
  scale_fill_manual(values=c("Cross" = "#4292c6", "Self" = "#238b45")) +
  scale_color_manual(values=c("Cross" = "#4292c6", "Self" = "#238b45")) + 
  theme_bw() +
  labs(x= "Plant height (inches)", y="Count")

dat28_pivot <- dat28 %>% pivot_longer(cols=1:2, names_to="Treatment", values_to = "Height")

dat28_pivot %>%
  ggplot(aes(Treatment, Height, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none")+
  scale_fill_manual(breaks=c("Cross", "Self"), 
                    values=c("#238443", "#d9f0a3")) +
  scale_x_discrete(labels=c("Cross-Pollinated", "Self-Pollinated")) +
  ylim(0, 25) +
  labs(x= "", y="Height (Inches)", title="Effect of Pollination Type on Plant Height")

# Based on the plots I would say that the data are not normally distributed, and thus a t-test is not appropriate.

# Find a two-sided p-value for the hypothesis using the signed-rank test.

result <-wilcox.test(dat28$Cross, dat28$Self, paired=TRUE) 

result$p.value

# Question 4.32: Therapeutic Marijuana ####

# Does marijuana treatment reduce the frequency of vomiting episodes?

dat32 <- Sleuth3::ex0432
head(dat32)

dat32_pivot <- dat32 %>% pivot_longer(cols=2:3, names_to="Treatment", values_to = "Number")

dat32_pivot %>%
  ggplot(aes(Treatment, Number, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none")+
  scale_fill_manual(breaks=c("Marijuana", "Placebo"), 
                    values=c("#238443", "#fee8c8")) +
  scale_x_discrete(labels=c("Marijuana", "Placebo")) +
  labs(x= "", y="Number of Vomiting Episodes", title="The Effect of Marijuana as an Antiemetic in Cancer Patients")


dat32 <- dat32 %>%
  group_by(Subject) %>%
  mutate(diff = (Placebo - Marijuana )) %>%
  mutate(logDiff = log(diff + 1)) %>%
  filter(logDiff!=0)

wilcox.test(dat32$logDiff, alternative = "greater")

wsrTest((dat32$logDiff), conf.int = TRUE, conf.level = 0.95)



hist(dat32$logDiff, breaks=20, xlab = "")
# back transform <- exp(c(lci,uci)) -1


# dont forget to write  summary

# Question 4.30 Sleuth 2: Sunlight protection factor ####


dat30 <- Sleuth2::ex0430
head(dat30)




dat32 <- dat32 %>%
  group_by(Subject) %>%
  mutate(diff = (Placebo - Marijuana )) %>%
  mutate(logDiff = log(diff + 1)) %>%
  filter(logDiff!=0)


hist(dat30$Control)
hist(dat30$Sunscreen)


wilcox.test(log(dat30$Sunscreen), log(dat30$Control), paired= T, conf.int=T, conf.level=0.95)

wsrTest(log(dat30$Sunscreen), log(dat30$Control), conf.int = TRUE, conf.level = 0.95)

q1 <- log(dat30$Control)
q2 <- log(dat30$Sunscreen)


diffq <- q2 - q1

wsrTest(diffq, conf.int = TRUE, conf.level = 0.95)

est <- exp(1.99)

CI.l <- exp(1.53)
CI.u <- exp(2.51)

dat30_pivot <- dat30 %>% pivot_longer(cols=1:2, names_to="Treatment", values_to = "Time")

dat30_pivot %>%
  ggplot(aes(Treatment, Time, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position="none") +
  scale_fill_manual(breaks=c("Control", "Sunscreen"), 
                    values=c("#b30000", "#fed976")) +
  labs(x= "", y="Time (Minutes)", title="Effect of Sunscreen on Sun Tolerance")

# Question 4.31 Sleuth 2: Effect of group therapy on survival of breast cancer patients ####

# Cancer patients were assigned to either a control group or a therapy group to assess whether it improved quality of life.
# Researchers noticed therapy patients appear to have lived longer.

dat31 <- Sleuth2::ex0431
head(dat31)


boxplot(dat31$Survival ~ dat31$Group, main="Case 4.31") 

dat31 %>%
  ggplot(aes(Group, Survival, fill=Group)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(breaks=c("Control", "Therapy"), 
                    values=c("#bdbdbd", "#c994c7")) +
  scale_x_discrete(labels=c("Control", "Therapy")) +
  labs(x= "", y="Survival Time (Months)", title="Effect of Therapy on Cancer Survival")

dat31 %>%
  ggplot(aes(Group, log(Survival), fill=Group)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(breaks=c("Control", "Therapy"), 
                    values=c("#bdbdbd", "#c994c7")) +
  scale_x_discrete(labels=c("Control", "Therapy")) +
  labs(x= "", y="Survival Time (log of Months)", title="Effect of Therapy on Cancer Survival")

t.test(dat31$Survival ~ dat31$Group, var.equal=TRUE)

wilcox.test(log(Survival) ~ Group, exact=FALSE, alternative="two.sided", data=dat31)

wilcox.test(log(Survival) ~ Group, exact=FALSE, 
            alternative="greater", data=dat31)  # Rank-sum test; alternative: group 1 is greater
wilcox.test(log(Survival) ~ Group, exact=FALSE, correct=TRUE, 
            alternative="two.sided", conf.int=TRUE, data=dat31)  # Use 2-sided to get confidence int.  

exp(-.318)
exp(-0.981)
exp(0.251)
summary(dat31)


wsrTest(log(dat31$Survival) ~ dat31$Group, conf.int = TRUE, conf.level = 95, alternative="less")
# Master problem: write code for permutation test ####




