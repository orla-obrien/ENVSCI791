# RScs0801_3rd.r
# from the Sleuth3 package
# Written some time in 2013, updated with weighted regression 10/13/21
library(car)
library(lmtest)  # Breusch-Pagan test.
library(Sleuth3)
str(case0801)
attach(case0801)

## EXPLORATION
logSpecies <- log(Species)   
logArea  <- log(Area) 
plot(logSpecies ~ logArea, xlab="Log of Island Area",
     ylab="Log of Number of Species",
     main="Number of Reptile and Amphibian Species on 7 Islands")
myLm <- lm(logSpecies ~ logArea)    
abline(myLm)

## INFERENCE AND INTERPRETATION
summary(myLm)
slope     <- myLm$coef[2]
slopeConf <- confint(myLm,2)
sprintf('The unweighted species area relation: Species = %5.3f * Area^%5.3f +/- %5.3f ',
        exp(myLm$coefficients[1]),myLm$coefficients[2],
        slopeConf[2]-myLm$coefficients[2])
100*(2^(slope)-1)   # Back-transform estimated slope
100*(2^(slopeConf)-1) # Back-transform confidence interval 
# Interpretation: Associated with each doubling of island area is a 19% increase 
# in the median number of bird species (95% CI: 16% to 21% increase).

## DISPLAY FOR PRESENTATION
plot(Species ~ Area, xlab="Island Area (Square Miles); Log Scale",  
     ylab="Number of Species; Log Scale", 
     main="Number of Reptile and Amphibian Species on 7 Islands",
     log="xy", pch=21, lwd=2, bg="green",cex=2 )    
dummyArea <- c(min(Area),max(Area)) 
beta <- myLm$coef  
meanLogSpecies <-  beta[1] + beta[2]*log(dummyArea)   
medianSpecies  <-  exp(meanLogSpecies)  
lines(medianSpecies ~ dummyArea,lwd=2,col="blue") 
island <- c(" Cuba"," Hispaniola"," Jamaica", " Puerto Rico", 
            " Montserrat"," Saba"," Redonda")  
for (i in 1:7) {   
  offset <- ifelse(Area[i] < 10000, -.2, 1.5)  
  text(Area[i],Species[i],island[i],col="dark green",adj=offset,cex=.75) }  

# Analyses with SPSS and Matlab indicated that a weighted regression should be
# performed. This website provides an approach:
# https://www.statology.org/weighted-least-squares-in-r/

#create residual vs. fitted plot
plot(fitted(myLm), resid(myLm), xlab='Fitted Values', ylab='Residuals')
#add a horizontal line at 0 
abline(0,0)
# There is the appearance of a trumpet shape, so perform a heteroscedasticy test
#perform Breusch-Pagan (1979) test, note Levene's only works on categorical data
bptest(myLm)
# Weak evidence at best for heteroscedasity, but go ahead with weighted anyway
# define weights to use
# Fox & Weisberg (CAR) have an similar, but not identical, test ncvTest

ncvTest(myLm)
ncvTest(myLm2)

# Again, no indication of heteroscedasticity, probably in part due to small
# sample sizes.
wtfit<-lm(abs(myLm$residuals) ~ myLm$fitted.values)
wt <- 1 / wtfit$fitted.values^2
#perform weighted least squares regression
wls_model <- lm(logSpecies ~ logArea, data = case0801, weights=wt)
summary(wls_model)

# The standard error has been reduced by 30%
plot(logSpecies ~ logArea, xlab="Log of Island Area",ylab="Log of Number of Species",
          main="Number of Reptile and Amphibian Species on 7 Islands")
abline(wls_model)

# Code modified from Dalgaard 2008 p 120)
pred.frame<-data.frame(log.area=logArea)
pc<-predict(wls_model,int="c",newdata=pred.frame)
# Need to apply the weights function, see:
# https://stats.stackexchange.com/questions/246095/weights-with-prediction-intervals
pp<-predict(wls_model,int="p",newdata=pred.frame, weights=wt)
plot(logArea,logSpecies, ylim=range(logSpecies,pp,na.rm=T),
                    xlab="Log of Island Area", ylab="Log of Number of Species",
                    main="Weighted Regression: Number of Reptile and Amphibian Species on 7 Islands")
pred.logarea<-pred.frame$log.area
matlines(pred.logarea,pc,lty=c(1,2,2),col="magenta")
matlines(pred.logarea,pp,lty=c(1,2,3),col="blue")

# Plot on linear scales Can't get the prediction intervals to work, due to weighting
pred.frame<-data.frame(logArea=seq(0.01,log(50000),0.2))
pc<-predict(wls_model,int="c",newdata=pred.frame)
# temp<-predict(wtfit,newdata=pred.frame)
# wtp<-1/temp^2
# Used a regression to find the formula for the weights
# pp<-predict(wls_model,int="p",newdata=pred.frame, weights=7.11*pred.frame^-.258)
plot(case0801$Area,case0801$Species, xlim=c(-10,50000),
     ylim=range(case0801$Species,exp(pp),na.rm=T),
     xlab="Island Area", ylab="Number of Species",
main="Weighted Regression: Number of Reptile and Amphibian Species on 7 Islands")
pred.area<-exp(pred.frame$logArea)
matlines(pred.area,exp(pc),lty=c(1,2,2),col="magenta")
#matlines(pred.area,exp(pp),lty=c(1,2,3),col="blue")

ci<-confint(wls_model)
ci[2,]-wls_model$coefficients[2]

sprintf('The weighted species area relation: Species = %5.3f * Area^%5.3f +/- %5.3f ',
        exp(wls_model$coefficients[1]),wls_model$coefficients[2],
        ci[2,2]-wls_model$coefficients[2])
sprintf('The unweighted species area relation: Species = %5.3f * Area^%5.3f +/- %5.3f ',
        exp(myLm$coefficients[1]),myLm$coefficients[2],
        slopeConf[2]-myLm$coefficients[2])

detach(case0801)

