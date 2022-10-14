# RScs0601_3rd
# Code copied from Sleuth3 help file by Eugene.Gallagher@umb.edu, 1/17/13,
# minor revisions 10/4/22

library(car)  # car boxplot will label the outlier in Boxplot
library(multcomp)
library(Sleuth3)
str(case0601)

# Not good programming practice to attach and detach data frames
attach(case0601) 

## EXPLORATION
myHandicap  <- factor(Handicap,  
                      levels=c("None","Amputee","Crutches","Hearing","Wheelchair"))  
# Analyze with car boxplot:
Boxplot(Score ~ myHandicap, 
        ylab= "Qualification Score Assigned by Student to Interviewee",  
        xlab= "Treatment Group--Handicap Portrayed (14 Students in each Group)", 
        main= "Handicap Discrimination Experiment on 70 Undergraduate Students")

## ANOVA
myAov  <- aov(Score ~ myHandicap) 
plot(myAov, which=1) # Plot residuals versus estimated means 
summary(myAov) 

## COMPARE MEAN QUALIFICATION SCORE OF EVERY HANDICAP GROUP TO "NONE" 
# Use the multcomp library & the Dunnet treatment vs. control multiple comparison
myDunnett  <- glht(myAov, linfct = mcp(myHandicap = "Dunnett"))  
summary(myDunnett) 
confint(myDunnett,level=.95) 
opar <- par(no.readonly=TRUE)  # Save current graphics parameter settings
par(mar=c(4.1,8.1,4.1,1.1)) # Change margins 
plot(myDunnett, 
       xlab="Difference in Mean Qualification Score (and Dunnett-adjusted CIs)") 
par(opar)  # Restore original graphics parameter settings

## COMPARE EVERY MEAN TO EVERY OTHER MEAN

myTukey   <- glht(myAov, linfct = mcp(myHandicap = "Tukey"))  
summary(myTukey) 


## TEST THE CONTRAST OF DISPLAY 6.4
myAov2        <- aov(Score ~ myHandicap - 1)    
myContrast    <- rbind(c(0, -1/2, 1/2, -1/2, 1/2)) 

myComparison  <- glht(myAov2, linfct=myContrast)
summary(myComparison, test=adjusted("none"))  
confint(myComparison)  

# BOXPLOTS FOR PRESENTATION   
boxplot(Score ~ myHandicap, 
        ylab= "Qualification Score Assigned by Student to Video Job Applicant",  
        xlab="Handicap Portrayed by Job Applicant in Video (14 Students in each Group)",  
        main= "Handicap Discrimination Experiment on 70 Undergraduate Students", 
        col="green", boxlwd=2, medlwd=2, whisklty=1, whisklwd=2, staplewex=.2,  
        staplelwd=2, outlwd=2, outpch=21,  outbg="green", outcex=1.5) 

detach(case0601)